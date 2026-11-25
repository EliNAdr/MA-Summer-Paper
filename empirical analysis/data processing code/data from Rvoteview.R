# =============================================================================
# Script 01: House of Representatives voting panel data collection.
#
# OUTPUTS:
#   vote_data.rds / vote_data.csv
#     Panel data at the vote x Representative level, covering all House
#     roll-call votes from 1987 to the present.
#
#   house_members.rds / house_members.csv
#     Member-level panel at the member x Congress level.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Install and load required packages
# -----------------------------------------------------------------------------

# Install Rvoteview from GitHub (only needed once)
install.packages("devtools")
devtools::install_github("voteview/Rvoteview")

library(tidyverse)
library(Rvoteview)


# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

# Congress 100  => 1987-1988  (after FERS implementation)
# Congress 118 => 2023-2024  (most recent complete congress at time of writing)
CONGRESS_START <- 100
CONGRESS_END   <- 118

OUTPUT_VOTES      <- "empirical analysis/data files/vote_data.rds"      # vote-level panel
OUTPUT_VOTES_CSV  <- "empirical analysis/data files/vote_data.csv"      # vote-level panel (flat file)
OUTPUT_MEMBERS    <- "empirical analysis/data files/house_members.rds"  # member-level panel for Script 02
OUTPUT_MEMBERS_CSV <-"empirical analysis/data files/house_members.csv"  # member-level panel (flat file)

# Voteview vote codes → human-readable labels
# Full key: https://voteview.com/articles/data_help_members_votes
VOTE_CODE_LABELS <- c(
  "0" = "Not a member",
  "1" = "Yea",
  "2" = "Paired Yea",
  "3" = "Announced Yea",
  "4" = "Announced Nay",
  "5" = "Paired Nay",
  "6" = "Nay",
  "7" = "Present (Type 1)",
  "8" = "Present (Type 2)",
  "9" = "Not Voting"
)


# -----------------------------------------------------------------------------
# 2. Download member-level panel
# -----------------------------------------------------------------------------
# Use the Voteview bulk CSV as the primary source.
#
# The resulting house_members.rds is the INPUT to Script 02.
# It is NOT the same as the vote panel built in sections 3-5.

BULK_CSV_URL <- "https://voteview.com/static/data/out/members/HSall_members.csv"

message("=== Downloading member-level data ===")
message("Attempting bulk CSV download from Voteview...")

house_members_raw <- tryCatch(
  read_csv(BULK_CSV_URL, show_col_types = FALSE),
  error = function(e) stop("Failed to download bulk member CSV: ", e$message)
)

message("Downloaded ", nrow(house_members_raw), " rows from Voteview bulk CSV.")

# Normalise column names — the bulk CSV uses slightly different conventions
# from member_search() output depending on Rvoteview version
house_members <- house_members_raw |>
  rename_with(~ str_replace_all(.x, "\\.", "_")) |>   # dots → underscores
  rename(any_of(c(bioguide_id = "bioguide_id_1"))) |> # fix occasional duplicate suffix
  # Keep the columns Script 02 needs, plus useful extras
  select(any_of(c(
    "icpsr",           # Voteview member ID — merge key with vote data
    "bioguide_id",     # Biographical Directory ID — merge key with biographical data
    "bioname",         # Last, First name as recorded by Voteview
    "congress",        # Congress number
    "chamber",         # Will include Senate rows — filtered below
    "party_code"       # 100 = Democrat, 200 = Republican, etc.
  ))) |>
  # The bulk CSV contains all chambers; keep only House rows
  filter(str_detect(tolower(chamber), "house")) |>
  # Restrict to the congress range of interest
  filter(congress >= CONGRESS_START, congress <= CONGRESS_END) |>
  distinct()

message("Member panel complete: ",
        nrow(house_members), " member-congress rows, ",
        n_distinct(house_members$icpsr), " unique members.")

# Validate that required columns are present
required_member_cols <- c("icpsr", "bioguide_id", "congress", "chamber")
missing_cols <- setdiff(required_member_cols, names(house_members))
if (length(missing_cols) > 0) {
  warning(
    "The following required columns are missing from house_members: ",
    paste(missing_cols, collapse = ", "), "\n",
    "Check the Rvoteview version or column name mapping above."
  )
} else {
  message("All required columns present in house_members.")
}

# Save member panel
saveRDS(house_members, file = OUTPUT_MEMBERS)
write_csv(house_members, file = OUTPUT_MEMBERS_CSV)
message("Saved member panel → ", OUTPUT_MEMBERS, " and ", OUTPUT_MEMBERS_CSV)


# -----------------------------------------------------------------------------
# 3. Helper: download and melt one Congress into a long vote data frame
# -----------------------------------------------------------------------------

download_one_congress <- function(cong) {

  message("\n=== Congress ", cong, " ===")

  # 3a. Search for all House roll calls in this congress
  message("  Searching for roll calls...")
  rc_meta <- tryCatch(
    voteview_search("", chamber = "House", congress = cong),
    error = function(e) {
      message("  ERROR searching congress ", cong, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(rc_meta) || nrow(rc_meta) == 0) {
    message("  No votes found for congress ", cong, ". Skipping.")
    return(NULL)
  }

  message("  Found ", nrow(rc_meta), " roll calls. Downloading...")

  # 3b. Download in batches of 1000 and melt each batch immediately.
  #     Avoids relying on %+% to merge rollcall objects across batches,
  #     which can silently drop the rollcall class.
  ids       <- rc_meta$id
  batch_sz  <- 1000
  n_batches <- ceiling(length(ids) / batch_sz)
  long_list <- vector("list", n_batches)

  for (b in seq_len(n_batches)) {
    idx_start <- (b - 1) * batch_sz + 1
    idx_end   <- min(b * batch_sz, length(ids))
    batch_ids <- ids[idx_start:idx_end]

    message("  Batch ", b, "/", n_batches,
            " (votes ", idx_start, "-", idx_end, ")...")

    long_list[[b]] <- tryCatch({
      rc <- voteview_download(batch_ids)
      want_legis <- c("name", "party_code", "icpsr", "bioguide_id")
      want_votes <- c("vname", "date", "congress", "chamber")
      melt_rollcall(
        rc,
        legiscols = intersect(want_legis, names(rc$legis.data)),
        votecols  = intersect(want_votes, names(rc$vote.data))
      )
    }, error = function(e) {
      message("  ERROR in batch ", b, ": ", e$message)
      return(NULL)
    })

    Sys.sleep(0.5) # Avoid overwhelming API
  }

  # 3c. Drop failed batches and stack into one data frame
  long_list <- Filter(Negate(is.null), long_list)
  if (length(long_list) == 0) return(NULL)

  message("  Combining ", length(long_list), " batch(es)...")
  rc_long <- bind_rows(long_list)

  # 3d. Tidy up: add derived columns
  rc_long <- rc_long |>
    mutate(
      # Date fields
      year      = year(as.Date(date)),

      # Session of Congress (year 1 or year 2 of the two-year term)
      session   = if_else(year %% 2 == 1, 1L, 2L),

      # Vote cast (human-readable)
      vote_cast = recode(as.character(vote),
                         !!!VOTE_CODE_LABELS,
                         .default = "Unknown"),

      # Standardise column names
      rep_name    = name,
      rollcall_id = vname,
      congress    = as.integer(congress)
    ) |>

    select(any_of(c(
      "congress", "session", "year", "rollcall_id",
      "rep_name", "party_code", "icpsr",
      "bioguide_id", "vote_cast", "vote"
    )))

  message("  Done: ", nrow(rc_long), " observations for congress ", cong)
  return(rc_long)
}


# -----------------------------------------------------------------------------
# 4. Loop over all congresses and stack into one vote panel
# -----------------------------------------------------------------------------

all_congresses <- CONGRESS_START:CONGRESS_END

panel_list <- vector("list", length(all_congresses))
names(panel_list) <- as.character(all_congresses)

for (cong in all_congresses) {
  panel_list[[as.character(cong)]] <- download_one_congress(cong)
}


# -----------------------------------------------------------------------------
# 5. Combine and save vote panel
# -----------------------------------------------------------------------------

message("\nCombining all congresses...")

panel <- bind_rows(panel_list)

message("Final vote panel: ", nrow(panel), " observations")
message("                  ", n_distinct(panel$rollcall_id), " unique roll calls")
message("                  ", n_distinct(panel$rep_name),    " unique representatives")
message("                  Years: ", min(panel$year), " to ", max(panel$year))

saveRDS(panel, file = OUTPUT_VOTES)
write_csv(panel, file = OUTPUT_VOTES_CSV)
message("Saved vote panel → ", OUTPUT_VOTES, " and ", OUTPUT_VOTES_CSV)