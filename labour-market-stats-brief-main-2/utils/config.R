# auto-detect reference month from database

source("utils/helpers.R")

detected <- auto_detect_manual_month()
if (!is.null(detected)) {
  manual_month <- detected
} else {
  # db unavailable, fall back to current date
  manual_month <- tolower(paste0(format(Sys.Date(), "%b"), format(Sys.Date(), "%Y")))
  message("[config] Could not auto-detect from database; using current date: ", manual_month)
}

# baseline dates for "change since" columns
COVID_DATE  <- as.Date("2020-02-01")
ELEC24_DATE <- as.Date("2024-06-01")

# matching lfs period labels for the baseline dates above
COVID_LFS_LABEL <- "Dec-Feb 2020"
COVID_VAC_LABEL <- "Jan-Mar 2020"
ELECTION_LABEL  <- "Apr-Jun 2024"
