# =============================================================================
# Script 00: Master file
#
# Runs all other code files for this project
#   - 3 data processing files (R)
#   - 3 empirical analysis files (R)
#   - 1 annuities plotting file (R)
#   - 1 model file (Python)
#
# NOTE: running "data from Congress API.R" requires setting a Congress.gov API
#       key in the .Renviron:
#         CONGRESS_API_KEY = "your_key_here"
#         Get a free key at: https://api.congress.gov/sign-up/
#
# NOTE: running "data from Rvoteview.R" requires installing the Rvoteview
#       package from Github. Installed automatically by Script 01.
#
# If you already have the "final_panel.rds" file you can comment out scripts
# 1-3 and run the remaining code.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Set working directory to project root
# -----------------------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

stopifnot(dir.exists("empirical analysis"), dir.exists("annuities plot"), dir.exists("model"))


# -----------------------------------------------------------------------------
# 1-3. Data collection and merge
# -----------------------------------------------------------------------------

source("empirical analysis/data processing code/data from Rvoteview.R")
source("empirical analysis/data processing code/data from Congress API.R")
source("empirical analysis/data processing code/merge and collapse.R")


# -----------------------------------------------------------------------------
# 4-6. Summary statistics, empirical analysis, and plots
# -----------------------------------------------------------------------------

source("empirical analysis/analysis code/summary statistics.R")
source("empirical analysis/analysis code/regressions.R")
source("empirical analysis/analysis code/robustness panels.R")


# -----------------------------------------------------------------------------
# 7. FERS annuities plot (does not require data files)
# -----------------------------------------------------------------------------

source("annuities plot/FERS annuities.R")


# -----------------------------------------------------------------------------
# 8. Model numerical implementation and plots (Jupyter notebook)
# -----------------------------------------------------------------------------

notebook_path <- file.path("model", "model implementation.ipynb")

status <- system2(
  "python3",
  args = c("-m", "jupyter", "nbconvert", "--to", "notebook", "--execute", "--inplace", shQuote(notebook_path))
)

if (status != 0) {
  stop("Failed to execute '", notebook_path, "'. Ensure jupyter and nbconvert are installed.")
}

message("Clear all outputs in model Jupyter notebook to reduce file size")


# -----------------------------------------------------------------------------
# 9. Completion messages
# -----------------------------------------------------------------------------

message("\nReplication complete")
message("\nEnsure all plots have saved correctly; if not, ensure they are not commented out in their respective files")