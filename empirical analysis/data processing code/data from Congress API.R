# =============================================================================
# Script 02: Build a member-level panel with term number at each Congress.
#
# INPUT:
#   Output of Script 01 — specifically `house_members_rds` (a data frame with
#   icpsr, bioguide_id, bioname, congress, chamber columns).
#
# OUTPUT:
#   bio_data.rds /  bio_data.csv
#     One row per member per Congress, ready to merge to vote data.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Load required packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(httr)


# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

CONGRESS_API_KEY <- Sys.getenv("CONGRESS_API_KEY")
# Set this in your .Renviron file:

if (CONGRESS_API_KEY == "") {
  stop("CONGRESS_API_KEY is not set. Add it to your .Renviron file and restart R.")
}

RATE_LIMIT_PAUSE <- 0.5   # seconds between API calls

INPUT_FILE  <- "empirical analysis/data files/house_members.rds" # output of "data from Rvoteview.R"
OUTPUT_FILE <- "empirical analysis/data files/bio_data.rds"


# -----------------------------------------------------------------------------
# 2. Load Script 01 output
# -----------------------------------------------------------------------------

if (!file.exists(INPUT_FILE)) {
  stop(
    "Input file '", INPUT_FILE, "' not found.\n",
    "Run 'data from Rvoteview.R' first and ensure it saves house_members ",
    "with saveRDS(house_members, '", INPUT_FILE, "')."
  )
}

house_members <- readRDS(INPUT_FILE)

message("Loaded ", nrow(house_members), " member-congress rows.")

# Minimum required columns
required_cols <- c("icpsr", "bioguide_id", "bioname", "congress")
missing_cols <- setdiff(required_cols, names(house_members))
if (length(missing_cols) > 0) {
  stop("Missing columns in input: ", paste(missing_cols, collapse = ", "))
}


# -----------------------------------------------------------------------------
# 3. Fetch Representative data from Congress.gov API
# -----------------------------------------------------------------------------

# Helper: infix null-coalescing operator
# NOTE: must be defined before fetch_member_terms_api()
`%||%` <- function(a, b) if (!is.null(a)) a else b

fetch_member_api <- function(bioguide_id) {
  if (is.na(bioguide_id) || bioguide_id == "") return(NULL)

  url <- paste0(
    "https://api.congress.gov/v3/member/", bioguide_id,
    "?api_key=", CONGRESS_API_KEY,
    "&format=json"
  )

  resp <- tryCatch(GET(url), error = function(e) NULL)
  if (is.null(resp) || http_error(resp)) return(NULL)

  content(resp, as = "parsed", type = "application/json")$member
}

extract_terms <- function(bioguide_id, member) {
  terms <- member$terms
  if (is.null(terms) || length(terms) == 0) return(NULL)

  tibble(
    bioguide_id = bioguide_id,
    chamber     = map_chr(terms, ~ .x$chamber %||% NA_character_),
    congress    = map_int(terms, ~ .x$congress %||% NA_integer_),
    start_year  = map_int(terms, ~ .x$startYear %||% NA_integer_)
  )
}

# Leadership entries only carry {congress, type} — no chamber — so they are
# matched to House terms downstream by bioguide_id + congress alone.
extract_leadership <- function(bioguide_id, member) {
  leadership <- member$leadership
  if (is.null(leadership) || length(leadership) == 0) return(NULL)

  tibble(
    bioguide_id      = bioguide_id,
    congress         = map_int(leadership, ~ .x$congress %||% NA_integer_),
    leadership_type  = map_chr(leadership, ~ .x$type %||% NA_character_)
  )
}

# birthYear is a single top-level field on the member record (one value per
# bioguide_id, not per term).
extract_birth_year <- function(bioguide_id, member) {
  tibble(
    bioguide_id = bioguide_id,
    birth_year  = as.integer(member$birthYear %||% NA_integer_)
  )
}

# Get unique bioguide IDs to query
unique_bioguides <- house_members |>
  filter(!is.na(bioguide_id), bioguide_id != "") |>
  distinct(bioguide_id) |>
  pull(bioguide_id)

message("Fetching term/leadership histories for ",
        length(unique_bioguides), " members from Congress.gov API...")

api_terms_list      <- vector("list", length(unique_bioguides))
api_leadership_list <- vector("list", length(unique_bioguides))
api_birth_year_list <- vector("list", length(unique_bioguides))

for (i in seq_along(unique_bioguides)) {
  bid <- unique_bioguides[i]
  member <- fetch_member_api(bid)
  api_terms_list[[i]]      <- extract_terms(bid, member)
  api_leadership_list[[i]] <- extract_leadership(bid, member)
  api_birth_year_list[[i]] <- extract_birth_year(bid, member)
  Sys.sleep(RATE_LIMIT_PAUSE)
  if (i %% 50 == 0) message("  ... ", i, " / ",
                             length(unique_bioguides), " done")
}

api_terms      <- bind_rows(api_terms_list)
api_leadership <- bind_rows(api_leadership_list)
api_birth_year <- bind_rows(api_birth_year_list)

# Compute term_number from the API data
api_terms_numbered <- api_terms |>
  filter(str_detect(tolower(chamber), "house")) |>
  arrange(bioguide_id, congress) |>
  group_by(bioguide_id) |>
  mutate(
    house_term_number = row_number(),
    first_house_year  = min(start_year, na.rm = TRUE)
  ) |>
  ungroup()


# -----------------------------------------------------------------------------
# 4. Assemble final panel
# -----------------------------------------------------------------------------

# Collapse leadership entries to one row per bioguide_id-congress: a dummy
# for holding any House leadership position, plus the position title(s).
leadership_by_congress <- api_leadership |>
  distinct(bioguide_id, congress, leadership_type) |>
  group_by(bioguide_id, congress) |>
  summarise(
    leadership_position = paste(leadership_type, collapse = "; "),
    is_leader           = TRUE,
    .groups = "drop"
  )

# Join all components:
#   - Voteview base (icpsr, congress)
#   - Canonical biographical data (bioname) — one value per bioguide_id,
#     so a member's record is identical across rows even if their icpsr-keyed
#     Voteview entries differ slightly (e.g. after switching parties)
#   - API term history (house_term_number, first_house_year)
#   - API leadership history (is_leader, leadership_position)
#   - API birth year (age_at_term_start)

member_bio <- house_members |>
  select(icpsr, bioguide_id, congress, bioname) |>
  left_join(
    api_terms_numbered |>
      select(bioguide_id, congress, house_term_number,
             first_house_year, start_year),
    by = c("bioguide_id", "congress")
  ) |>
  left_join(
    leadership_by_congress,
    by = c("bioguide_id", "congress")
  ) |>
  left_join(
    api_birth_year,
    by = "bioguide_id"
  ) |>
  mutate(
    is_leader          = replace_na(is_leader, FALSE),
    age_at_term_start  = start_year - birth_year
  ) |>
  select(
    icpsr, bioguide_id, bioname,
    congress, house_term_number,
    start_year, first_house_year,
    is_leader, leadership_position,
    birth_year, age_at_term_start
  )

message("Final panel: ", nrow(member_bio),
        " member-congress rows, ",
        n_distinct(member_bio$bioguide_id),
        " unique members.")

message(sum(member_bio$is_leader),
        " member-congress row(s) flagged with a House leadership position.")

n_missing_terms <- sum(is.na(member_bio$house_term_number))
if (n_missing_terms > 0) {
  message("--- Rows with missing house_term_number ---")
  member_bio |>
    filter(is.na(house_term_number)) |>
    select(icpsr, bioguide_id, bioname, congress) |>
    print(n = 30)
}


# -----------------------------------------------------------------------------
# 5. Save output
# -----------------------------------------------------------------------------

saveRDS(member_bio, OUTPUT_FILE)
message("Saved → ", OUTPUT_FILE)

write_csv(member_bio, str_replace(OUTPUT_FILE, "\\.rds$", ".csv"))