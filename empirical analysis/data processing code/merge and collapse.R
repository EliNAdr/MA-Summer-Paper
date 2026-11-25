# =============================================================================
# Script 03: Build Representative × Term Panel
#
# INPUTS:
#   vote_data.rds — vote-level panel
#   bio_data.rds  — member biographical data panel
#
# OUTPUTS:
#   final_panel.rds / final_panel.csv
#     One row per Representative × term (Congress).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Load required package
# -----------------------------------------------------------------------------

library(tidyverse)


# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

INPUT_VOTES <- "empirical analysis/data files/vote_data.rds"  # Script 01 output
INPUT_BIO   <- "empirical analysis/data files/bio_data.rds"   # Script 02 output

OUTPUT      <- "empirical analysis/data files/final_panel.rds"


# -----------------------------------------------------------------------------
# 2. Load inputs
# -----------------------------------------------------------------------------

if (!file.exists(INPUT_VOTES)) {
  stop("Vote data not found: '", INPUT_VOTES, "'. Run 'data from Rvoteview.R' first.")
}
if (!file.exists(INPUT_BIO)) {
  stop("Biographical data not found: '", INPUT_BIO, "'. Run 'data from bioguide' first.")
}

message("Loading vote data...")
votes <- readRDS(INPUT_VOTES)
message("  ", nrow(votes), " vote-level rows loaded.")

message("Loading member biographical data...")
bios <- readRDS(INPUT_BIO)
message("  ", nrow(bios), " member-congress biographical rows loaded.")


# -----------------------------------------------------------------------------
# 3. Classify each vote as Yea, Nay, or Absent
# -----------------------------------------------------------------------------
# Voteview vote codes:
#   1 = Yea, 2 = Paired Yea, 3 = Announced Yea                 → treat as Yea
#   4 = Announced Nay, 5 = Paired Nay, 6 = Nay                 → treat as Nay
#   7 = Present (Type 1), 8 = Present (Type 2), 9 = Not Voting → Absent
#   0 = Not a member                                           → drop entirely

votes <- votes |>
  filter(vote != 0) |>     # remove "not a member" rows
  mutate(
    vote_direction = case_when(
      vote %in% 1:3 ~ "yea",
      vote %in% 4:6 ~ "nay",
      vote %in% 7:9 ~ "absent",
      TRUE          ~ NA_character_
    )
  )


# -----------------------------------------------------------------------------
# 4. Compute party majority direction per roll call
# -----------------------------------------------------------------------------
# For each roll call, determine whether the majority of each party voted
# Yea or Nay. Absences are excluded from this calculation — we are
# measuring the direction the party chose, not its turnout.
#
# party_majority: "yea" if more party members voted Yea than Nay,
#                 "nay" if more voted Nay than Yea,
#                 NA if tied or no party members voted either way.

message("Computing party majority direction per roll call × party...")

party_majority <- votes |>
  filter(vote_direction %in% c("yea", "nay")) |>
  group_by(rollcall_id, party_code) |>
  summarise(
    party_yea = sum(vote_direction == "yea", na.rm = TRUE),
    party_nay = sum(vote_direction == "nay", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    party_majority = case_when(
      party_yea > party_nay ~ "yea",
      party_nay > party_yea ~ "nay",
      TRUE                  ~ NA_character_   # tied — will be excluded below
    )
  ) |>
  select(rollcall_id, party_code, party_majority)

message("  Party majority computed for ",
        n_distinct(party_majority$rollcall_id), " roll calls.")

n_tied <- sum(is.na(party_majority$party_majority))
message("  ", n_tied, " rollcall × party combinations tied (will be excluded).")

# Partisan roll calls: Democrats (100) and Republicans (200) had majorities
# in opposite directions.

partisan_rollcalls <- party_majority |>
  filter(party_code %in% c(100, 200)) |>
  pivot_wider(names_from = party_code, values_from = party_majority,
              names_prefix = "party_") |>
  filter(!is.na(party_100), !is.na(party_200), party_100 != party_200) |>
  pull(rollcall_id)

message("  ", length(partisan_rollcalls),
        " roll calls had opposing Dem/Rep majorities (partisan votes).")


# -----------------------------------------------------------------------------
# 5. Classify each vote as with or against party
# -----------------------------------------------------------------------------
# with_party: 1 if voted with party majority, 0 if voted against, NA if absent.
# with_party_against_absent: same but absent = 0 (for unconditional version).
# in_denominator_exclude: TRUE for directional votes (used for main version).

votes <- votes |>
  left_join(party_majority, by = c("rollcall_id", "party_code")) |>
  # Drop tied roll calls — no unambiguous party position to compare against
  filter(!is.na(party_majority))

votes <- votes |>
  mutate(
    with_party = case_when(
      vote_direction == party_majority ~ 1L,
      vote_direction %in% c("yea", "nay") &
        vote_direction != party_majority ~ 0L,
      TRUE ~ NA_integer_    # absent
    ),

    with_party_against_absent = if_else(is.na(with_party), 0L, with_party),

    in_denominator_exclude = vote_direction %in% c("yea", "nay"),

    is_partisan_vote = rollcall_id %in% partisan_rollcalls
  )


# -----------------------------------------------------------------------------
# 6. Collapse to Representative × Congress (= term) level
# -----------------------------------------------------------------------------

message("\nCollapsing to rep × congress panel...")

panel <- votes |>
  group_by(icpsr, congress) |>
  summarise(
    party_code    = first(party_code),
    n_votes_total = n(),
    n_votes_cast  = sum(in_denominator_exclude, na.rm = TRUE),
    n_with_party  = sum(with_party, na.rm = TRUE),

    # Overall loyalty: denominator = directional votes only (absences excluded)
    overall_loyalty = if_else(
      n_votes_cast > 0,
      100*n_with_party / n_votes_cast,
      NA_real_
    ),

    # Partisan votes: only votes where party majorities were in opposite
    # directions (Dem vs. Rep).
    n_votes_cast_partisan = sum(in_denominator_exclude & is_partisan_vote,
                                na.rm = TRUE),
    n_with_party_partisan = sum(with_party[is_partisan_vote], na.rm = TRUE),
    partisan_loyalty = if_else(
      n_votes_cast_partisan > 0,
      100*n_with_party_partisan / n_votes_cast_partisan,
      NA_real_
    ),

    # Unconditional: denominator = all votes (absences count against party)
    unconditional_loyalty = if_else(
      n_votes_total > 0,
      100*sum(with_party_against_absent) / n_votes_total,
      NA_real_
    ),

    # Participation rate
    participation_rate = if_else(n_votes_total > 0,
                                 100*n_votes_cast / n_votes_total,
                                 NA_real_),

    year_start = min(year, na.rm = TRUE),
    year_end   = max(year, na.rm = TRUE),

    .groups = "drop"
  )

message("Rep × congress rows: ", nrow(panel))


# -----------------------------------------------------------------------------
# 7. Merge with member biographical data
# -----------------------------------------------------------------------------

message("\nMerging with member biographical data...")

panel <- panel |>
  mutate(icpsr = as.double(icpsr)) |>
  right_join(
    bios |>
      select(
        icpsr, congress, bioname, bioguide_id, house_term_number,
        first_house_year, is_leader, leadership_position,
        age_at_term_start, birth_year
      ),
    by = c("icpsr", "congress")
  )

n_total <- nrow(panel)

# Bios rows with no matching vote data (right_join keeps every bios row, so
# these show up as NA in the panel-derived columns, e.g. n_votes_total).
n_no_votes <- sum(is.na(panel$n_votes_total))
message("  Bio rows with no matching vote data: ", n_no_votes, " / ", n_total,
        " (", round(100 * n_no_votes / n_total, 1), "%) — ",
        "these will be dropped (no votes to compute loyalty from).")

# Bios rows with no house_term_number (a bios-native gap from the Congress
# API term lookup in Script 02, unrelated to vote-data matching).
n_no_term <- sum(is.na(panel$house_term_number))
if (n_no_term > 0) {
  message("  Bio rows missing house_term_number: ", n_no_term, " / ", n_total,
          " — these will be dropped (no term data).")
}

panel <- panel |>
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican",
      party_code == 328 ~ "Independent",
      TRUE  ~ paste0("Other (", party_code, ")")
    ))


# -----------------------------------------------------------------------------
# 8. Remove unusable observations, select variables
# -----------------------------------------------------------------------------

panel <- panel |>
  # Remove bio rows with no matching vote data (nothing to compute loyalty from)
  filter(!is.na(n_votes_total)) |>
  # Remove members with no matched biographical data
  filter(!is.na(house_term_number)) |>
  # Remove members first seated before FERS took effect
  filter(first_house_year >= 1987) |>
  # Remove observations where member changes party during term
  group_by(bioguide_id, congress) |>
  filter(n() == 1) |>
  ungroup() |>
  # Remove observations with Independent status; loyalty is meaningless
  filter(party != "Independent") |>
  # Remove observations with low participation; causes noisy loyalty
  filter(participation_rate >= 80)

panel <- panel |>
  arrange(bioguide_id, congress) |>
  select(
    # Unit identifiers
    bioname, bioguide_id, first_house_year,
    birth_year,
    # Time-varying variables
    house_term_number, congress, party,
    is_leader, leadership_position,
    age_at_term_start,
    # Outcome variables
    overall_loyalty, unconditional_loyalty,
    partisan_loyalty, participation_rate
  )


# -----------------------------------------------------------------------------
# 9. Save output
# -----------------------------------------------------------------------------

saveRDS(panel, file = OUTPUT)
write_csv(panel, file = str_replace(OUTPUT, "\\.rds$", ".csv"))

message("\nDataframe saved")