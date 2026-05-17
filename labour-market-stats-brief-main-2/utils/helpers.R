# shared formatters and utility functions used across all sheets

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(DBI)
  library(RPostgres)
  library(tibble)
})

month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)

# latest lfs period + 2 months gives us the anchor month
auto_detect_manual_month <- function() {
  tryCatch({
    conn <- DBI::dbConnect(RPostgres::Postgres())
    on.exit(try(DBI::dbDisconnect(conn), silent = TRUE))
    
    res <- DBI::dbGetQuery(conn, 'SELECT DISTINCT time_period FROM "ons"."labour_market__age_group"')
    if (nrow(res) == 0) return(NULL)
    
    parse_end <- function(label) {
      mons <- regmatches(label, gregexpr("[A-Za-z]{3}", label))[[1]]
      yrs  <- regmatches(label, gregexpr("[0-9]{4}", label))[[1]]
      if (length(mons) < 2 || length(yrs) < 1) return(as.Date(NA))
      end_m <- month_map[tolower(mons[2])]
      yr <- suppressWarnings(as.integer(yrs[1]))
      if (is.na(end_m) || is.na(yr)) return(as.Date(NA))
      as.Date(sprintf("%04d-%02d-01", yr, end_m))
    }
    
    ends <- as.Date(vapply(res$time_period, parse_end, as.Date(NA)), origin = "1970-01-01")
    if (all(is.na(ends))) return(NULL)
    
    latest_end <- max(ends, na.rm = TRUE)
    # release anchor = lfs end + 2 months
    anchor <- latest_end %m+% months(2)
    tolower(paste0(format(anchor, "%b"), format(anchor, "%Y")))
  }, error = function(e) {
    warning("auto_detect_manual_month failed: ", e$message, "; using Sys.Date fallback")
    NULL
  })
}

parse_manual_month <- function(mm) {
  if (is.null(mm) || is.na(mm) || !nzchar(mm)) return(NULL)
  mm <- tolower(trimws(mm))
  mon_str <- substr(mm, 1, 3)
  yr_str  <- sub("^[a-z]+", "", mm)
  mon <- match(mon_str, tolower(month.abb))
  yr  <- suppressWarnings(as.integer(yr_str))
  if (is.na(mon) || is.na(yr)) return(NULL)
  as.Date(sprintf("%04d-%02d-01", yr, mon))
}

# e.g. "Oct-Dec 2025"
make_lfs_label <- function(end_date) {
  if (is.null(end_date) || is.na(end_date)) return("")
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(2)
  sprintf("%s-%s %s", format(start_date, "%b"), format(end_date, "%b"), format(end_date, "%Y"))
}

# e.g. "October 2025 to December 2025"
lfs_label_narrative <- function(end_date) {
  if (is.null(end_date) || is.na(end_date)) return("")
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(2)
  paste0(format(start_date, "%B %Y"), " to ", format(end_date, "%B %Y"))
}

make_payroll_label <- function(date) {
  format(date, "%B %Y")
}

make_ymd_label <- function(date) {
  format(date, "%Y-%m-%d")
}

# cpi/hr1 tables expect datetime strings not plain dates
make_datetime_label <- function(date) {
  paste0(format(date, "%Y-%m-%d"), " 00:00:00")
}

# look up a single value by dataset code + period label
val_by_code <- function(pg_data, code, period_label) {
  if (is.null(pg_data) || nrow(pg_data) == 0) return(NA_real_)
  match_row <- pg_data %>%
    filter(dataset_identifier_code == code, trimws(time_period) == trimws(period_label))
  if (nrow(match_row) == 0) return(NA_real_)
  suppressWarnings(as.numeric(match_row$value[1]))
}

# bumps to more decimal places if rounding to 1dp would give zero
fmt_one_dec <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("\u2014")
  if (x == 0) return(format(0, nsmall = 1, trim = TRUE))
  for (d in 1:4) {
    vr <- round(x, d)
    if (vr != 0) return(format(vr, nsmall = d, trim = TRUE))
  }
  format(round(x, 4), nsmall = 4, trim = TRUE)
}

fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("\u2014")
  paste0(fmt_one_dec(x), "%")
}

fmt_pp <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("\u2014")
  paste0(fmt_one_dec(abs(x)), " percentage points")
}

# picks direction word based on sign
fmt_dir <- function(x, up_word = "up", down_word = "down") {
  if (is.na(x)) return("")
  if (x > 0) up_word
  else if (x < 0) down_word
  else "unchanged at"
}

# integer with comma separators, e.g. 1234 -> "1,234"
fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("\u2014")
  format(round(x), big.mark = ",", scientific = FALSE)
}

# --- signed formatters (include +/- prefix) ---

format_int <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(as.numeric(x), 0)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0")
  paste0(ifelse(v > 0, "+", "-"), format(abs(v), big.mark = ","))
}

format_pp <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- as.numeric(x)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0pp")
  for (d in 1:4) {
    vr <- round(v, d)
    if (vr != 0) return(paste0(ifelse(vr > 0, "+", "-"), format(abs(vr), nsmall = d), "pp"))
  }
  paste0(ifelse(v > 0, "+", "-"), format(abs(round(v, 4)), nsmall = 4), "pp")
}

format_pct1 <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- as.numeric(x)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0%")
  for (d in 1:4) {
    vr <- round(v, d)
    if (vr != 0) return(paste0(ifelse(vr > 0, "+", "-"), format(abs(vr), nsmall = d), "%"))
  }
  paste0(ifelse(v > 0, "+", "-"), format(abs(round(v, 4)), nsmall = 4), "%")
}

format_gbp_signed0 <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(as.numeric(x), 0)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("\u00A30")
  paste0(ifelse(v > 0, "+\u00A3", "-\u00A3"), format(abs(v), big.mark = ","))
}

# --- unsigned formatters ---

format_int_unsigned <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(abs(as.numeric(x)), 0)
  if (is.na(v)) return(NA_character_)
  format(v, big.mark = ",")
}

format_pct1_unsigned <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- abs(as.numeric(x))
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0.0%")
  for (d in 1:4) {
    vr <- round(v, d)
    if (vr != 0) return(paste0(format(vr, nsmall = d), "%"))
  }
  paste0(format(round(v, 4), nsmall = 4), "%")
}

# --- summary/narrative helpers ---
safe_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x[1]))
}

# back out percentage change when we only have the absolute delta
pct_from_delta <- function(cur, delta) {
  cur <- safe_num(cur); delta <- safe_num(delta)
  base <- cur - delta
  if (is.na(cur) || is.na(delta) || is.na(base) || base == 0) return(NA_real_)
  (delta / base) * 100
}

fmt_signed_int <- function(x) {
  if (is.na(x)) return("\u2014")
  s <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(s, format(round(abs(x), 0), big.mark = ","))
}

fmt_int_1k <- function(x) {
  if (is.na(x)) return("\u2014")
  format(round(abs(x)), big.mark = ",")
}

fmt_signed_int_1k <- function(x) {
  if (is.na(x)) return("\u2014")
  s <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(s, format(round(abs(x)), big.mark = ","))
}

# signed pp for narrative text
fmt_signed_pp <- function(x) {
  if (is.na(x)) return("\u2014")
  s <- if (x > 0) "+" else if (x < 0) "-" else ""
  v <- abs(x)
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    paste0(s, format(round(v, 2), nsmall = 2), " percentage points")
  } else {
    paste0(s, format(v1, nsmall = 1), " percentage points")
  }
}

# unsigned percentage for narrative text
fmt_pct_unsigned <- function(x) {
  if (is.na(x)) return("\u2014")
  v <- abs(as.numeric(x))
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    paste0(format(round(v, 2), nsmall = 2), "%")
  } else {
    paste0(format(v1, nsmall = 1), "%")
  }
}

# --- oecd fetch from postgres (shared by auto + manual modes) ---
#
# returns list(unemp, emp, inact) where each element is a data.frame with
# columns country/period/value (latest non-NA observation per country), or
# NULL on failure. The same format is produced by .parse_oecd_sdmx() and
# .read_oecd_latest(), so downstream code treats all three sources identically.
fetch_oecd_from_db <- function(verbose = FALSE) {
  result <- list(unemp = NULL, emp = NULL, inact = NULL)
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RPostgres", quietly = TRUE)) {
    if (verbose) message("[oecd_db] DBI/RPostgres not available")
    return(result)
  }
  
  country_map <- c(GBR = "United Kingdom", USA = "United States", FRA = "France",
                   DEU = "Germany", ITA = "Italy", ESP = "Spain",
                   CAN = "Canada", JPN = "Japan", EA20 = "Euro area")
  
  .query <- function(conn, table_name) {
    tryCatch({
      q <- sprintf(
        'SELECT ref_area, time_period, obs_value FROM "oecd"."%s" WHERE ref_area IN (\'GBR\',\'USA\',\'FRA\',\'DEU\',\'ITA\',\'ESP\',\'CAN\',\'JPN\',\'EA20\') ORDER BY ref_area, time_period DESC',
        table_name
      )
      raw <- DBI::dbGetQuery(conn, q)
      if (nrow(raw) == 0) return(NULL)
      raw <- raw[!is.na(raw$obs_value) & nzchar(raw$obs_value), , drop = FALSE]
      raw <- raw[!duplicated(raw$ref_area), , drop = FALSE]
      raw$country <- country_map[raw$ref_area]
      raw$value   <- suppressWarnings(as.numeric(raw$obs_value))
      raw$period  <- raw$time_period
      raw[!is.na(raw$country), c("country", "period", "value"), drop = FALSE]
    }, error = function(e) {
      if (verbose) message("[oecd_db] query '", table_name, "' failed: ", e$message)
      NULL
    })
  }
  
  conn <- NULL
  tryCatch({
    conn <- DBI::dbConnect(RPostgres::Postgres())
    result$unemp <- .query(conn, "labour_statistics__unemployment_rate")
    result$emp   <- .query(conn, "labour_statistics__employment_rate")
    result$inact <- .query(conn, "labour_statistics__inactivity_rate")
  }, error = function(e) {
    if (verbose) message("[oecd_db] connection failed: ", e$message)
  }, finally = {
    if (!is.null(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
  result
}