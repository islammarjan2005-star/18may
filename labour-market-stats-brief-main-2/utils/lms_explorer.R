# LMS Explorer \u2014 read the ONS bulk LMS time-series file, let the user pick
# any series + baselines, and append a "Custom indicators" section to the
# Word briefing. Sourced AFTER utils/word_charts.R so we can reuse
# .wc_xml_escape, .wc_read_text and .wc_write_text from there.

# ---- baseline registry ------------------------------------------------------
# Six entries. `applies` says which frequencies a baseline is valid for.
# `offset_months` is used for month/quarter/year baselines (uniformly in months
# so end-of-month arithmetic always works). Anchored baselines (Covid,
# election) hold a fixed Date.

LMS_BASELINES <- list(
  current  = list(id = "current",  label = "Current",          applies = c("M","Q","A"),
                  offset_months = 0L),
  month    = list(id = "month",    label = "On the month",     applies = "M",
                  offset_months = -1L),
  quarter  = list(id = "quarter",  label = "On the quarter",   applies = c("M","Q"),
                  offset_months = -3L),
  year     = list(id = "year",     label = "On the year",      applies = c("M","Q","A"),
                  offset_months = -12L),
  # Covid baseline = the pre-pandemic LFS quarter, Dec 2019-Feb 2020. The monthly
  # series is a 3-month rolling figure labelled by its MIDDLE month, so that
  # quarter is the "Jan 2020" point (Dec-Jan-Feb). .lms_covid_base resolves it
  # from the monthly series at every frequency; the Q/A anchors are only a
  # fallback for series that have no monthly data.
  precovid = list(id = "precovid", label = "Since Covid",      applies = c("M","Q","A"),
                  anchor = c(M = as.Date("2020-01-31"),
                             Q = as.Date("2019-12-31"),
                             A = as.Date("2019-12-31"))),
  # Since-election baseline = Apr-Jun 2024 (Q2 2024), the last quarter before
  # the Jul 2024 general election; that is the "May 2024" middle-month point.
  election = list(id = "election", label = "Since election",   applies = c("M","Q","A"),
                  anchor = as.Date("2024-05-31"))
)

.LMS_MONTH_LUT <- c(JAN=1, FEB=2, MAR=3, APR=4, MAY=5, JUN=6,
                    JUL=7, AUG=8, SEP=9, OCT=10, NOV=11, DEC=12)
.LMS_MONTH_NAMES_LONG <- c("January","February","March","April","May","June",
                           "July","August","September","October","November","December")
.LMS_MONTH_NAMES_SHORT <- c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec")

# ---- small helpers ----------------------------------------------------------

# 1-indexed column number -> Excel column letter ("A", "Z", "AA", ..., "BRK", ...).
.lms_col_letter <- function(n) {
  out <- ""
  while (n > 0) {
    n <- n - 1L
    out <- paste0(LETTERS[(n %% 26L) + 1L], out)
    n <- n %/% 26L
  }
  out
}

# Last day of the month that is `n` months before/after `d`. Avoids R's
# end-of-month rollover surprises (2026-03-31 -1mo would give 2026-03-03 in
# naive arithmetic).
.lms_last_day_of_offset_month <- function(d, n) {
  lt <- as.POSIXlt(d)
  total_mo <- lt$year * 12L + lt$mon + as.integer(n)
  yr <- (total_mo %/% 12L) + 1900L
  mo <- (total_mo %% 12L) + 1L
  next_mo <- if (mo == 12L) sprintf("%04d-01-01", yr + 1L)
             else            sprintf("%04d-%02d-01", yr, mo + 1L)
  as.Date(next_mo) - 1L
}

# Classify a single period label and return its end-of-period Date plus a
# friendly display string. LMS quarters are calendar quarters (Q1=Jan-Mar ...
# Q4=Oct-Dec). The monthly series is a 3-month rolling figure labelled by its
# MIDDLE month, so monthly "Feb 2020" is the Jan-Mar 2020 quarter and monthly
# "Jan 2020" is the Dec-Feb 2020 (Covid) quarter. Quarters render as
# "Mmm-Mmm YYYY" ("2020 Q1" -> "Jan-Mar 2020"); months as "Mmm YYYY"; annuals
# as the bare year.
.lms_classify_period <- function(label) {
  na_out <- list(freq = NA_character_, date = as.Date(NA), display = NA_character_)
  if (is.na(label) || !nzchar(label)) return(na_out)
  s <- trimws(as.character(label))

  if (grepl("^\\d{4}$", s)) {
    return(list(freq = "A", date = as.Date(sprintf("%s-12-31", s)), display = s))
  }
  m <- regmatches(s, regexec("^(\\d{4})\\s*Q([1-4])$", s))[[1]]
  if (length(m) == 3L) {
    yr <- as.integer(m[2]); q <- as.integer(m[3])
    end_mo   <- q * 3L                 # Mar, Jun, Sep, Dec
    start_mo <- end_mo - 2L            # Jan, Apr, Jul, Oct
    disp <- sprintf("%s-%s %d", .LMS_MONTH_NAMES_SHORT[start_mo],
                    .LMS_MONTH_NAMES_SHORT[end_mo], yr)
    return(list(freq = "Q",
                date = .lms_last_day_of_offset_month(
                  as.Date(sprintf("%04d-%02d-01", yr, end_mo)), 0L),
                display = disp))
  }
  m <- regmatches(s, regexec("^(\\d{4})\\s+(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)$", s))[[1]]
  if (length(m) == 3L) {
    yr <- as.integer(m[2]); mo <- .LMS_MONTH_LUT[[m[3]]]
    return(list(freq = "M",
                date = .lms_last_day_of_offset_month(
                  as.Date(sprintf("%04d-%02d-01", yr, mo)), 0L),
                display = sprintf("%s %d", .LMS_MONTH_NAMES_SHORT[mo], yr)))
  }
  na_out
}

# ---- catalog + period parsing ----------------------------------------------

# Parse the LMS file just enough to build (a) a catalog of all series by CDID
# and (b) the period column with classified frequency + end-of-period dates.
# Reads only metadata rows 1-7 + column A \u2014 sub-second on the 10 MB file.
parse_lms_catalog <- function(path) {
  out <- list(catalog = NULL, periods = NULL, ok = FALSE, warn = character(0))

  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (!"data" %in% sheets) {
    out$warn <- "LMS file must have a sheet named 'data'"
    return(out)
  }

  meta <- tryCatch(
    suppressMessages(readxl::read_excel(
      path, sheet = "data", n_max = 7, col_names = FALSE,
      .name_repair = "minimal")),
    error = function(e) NULL)
  if (is.null(meta) || nrow(meta) < 7 || ncol(meta) < 2) {
    out$warn <- "Could not read metadata rows from LMS file"
    return(out)
  }

  cdid_label_a2 <- trimws(as.character(meta[[1]][2]))
  if (is.na(cdid_label_a2) || toupper(cdid_label_a2) != "CDID") {
    out$warn <- "Row 2 of column A is not 'CDID' \u2014 does not look like an LMS bulk file"
    return(out)
  }

  rowvals <- function(r) trimws(as.character(unlist(meta[r, ])))
  title_row   <- rowvals(1)
  cdid_row    <- rowvals(2)
  preunit_row <- rowvals(3)
  unit_row    <- rowvals(4)
  release_row <- rowvals(5)
  notes_row   <- rowvals(7)

  ok <- !is.na(cdid_row) & nzchar(cdid_row) & toupper(cdid_row) != "CDID"
  ok[1] <- FALSE  # col 1 is the metadata-label column
  if (!any(ok)) {
    out$warn <- "No CDIDs found in row 2"
    return(out)
  }

  na_to_blank <- function(x) { x[is.na(x) | x == "NA"] <- ""; x }

  catalog <- data.frame(
    cdid      = cdid_row[ok],
    title     = na_to_blank(title_row[ok]),
    unit      = na_to_blank(unit_row[ok]),
    pre_unit  = na_to_blank(preunit_row[ok]),
    release   = na_to_blank(release_row[ok]),
    important = na_to_blank(notes_row[ok]),
    col_index = which(ok),
    stringsAsFactors = FALSE)

  dup <- duplicated(catalog$cdid)
  if (any(dup)) {
    out$warn <- c(out$warn, paste0(
      "Duplicate CDIDs dropped: ",
      paste(unique(catalog$cdid[dup]), collapse = ", ")))
    catalog <- catalog[!dup, , drop = FALSE]
  }

  col_a <- tryCatch(
    suppressMessages(readxl::read_excel(
      path, sheet = "data", range = readxl::cell_cols("A"),
      col_names = FALSE, .name_repair = "minimal")),
    error = function(e) NULL)
  if (is.null(col_a) || nrow(col_a) < 8) {
    out$warn <- c(out$warn, "Could not read period column (A) from LMS file")
    return(out)
  }

  labels   <- trimws(as.character(col_a[[1]]))
  n_rows   <- length(labels)
  freqs    <- character(n_rows)
  displays <- character(n_rows)
  dates    <- as.Date(rep(NA, n_rows))
  unmatched <- integer(0)
  for (r in 8:n_rows) {
    lab <- labels[r]
    if (is.na(lab) || !nzchar(lab)) next
    cls <- .lms_classify_period(lab)
    if (is.na(cls$freq)) {
      unmatched <- c(unmatched, r)
    } else {
      freqs[r]    <- cls$freq
      dates[r]    <- cls$date
      displays[r] <- cls$display
    }
  }

  keep <- nzchar(freqs)
  periods <- data.frame(
    row   = which(keep),
    label = displays[keep],
    freq  = freqs[keep],
    date  = dates[keep],
    stringsAsFactors = FALSE)

  if (length(unmatched) > 0) {
    out$warn <- c(out$warn, sprintf(
      "%d period label(s) unrecognised (first rows: %s); skipped.",
      length(unmatched), paste(head(unmatched, 8), collapse = ", ")))
  }

  out$catalog <- catalog
  out$periods <- periods
  out$ok      <- TRUE
  out
}

# ---- per-series reader ------------------------------------------------------

# Transparent in-memory cache of LMS data columns, keyed by file path. Reading
# a single column from the 10 MB workbook costs ~1s (readxl reopens the zip
# each time), so lms_preload() reads the whole sheet once (~3s) into a numeric
# matrix and every later lookup is instant. Without a preload the accessor
# falls back to reading just the requested column.
.LMS_DATA_CACHE <- new.env(parent = emptyenv())

lms_preload <- function(path) {
  d <- tryCatch(suppressMessages(readxl::read_excel(
         path, sheet = "data", col_names = FALSE, .name_repair = "minimal")),
       error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L) return(invisible(FALSE))
  m <- suppressWarnings(matrix(as.numeric(as.matrix(d)), nrow = nrow(d)))
  assign(path, m, envir = .LMS_DATA_CACHE)
  invisible(TRUE)
}

.lms_col_numeric <- function(path, col_index, max_row = 1553L) {
  if (exists(path, envir = .LMS_DATA_CACHE, inherits = FALSE)) {
    m <- get(path, envir = .LMS_DATA_CACHE)
    if (col_index > ncol(m)) return(numeric(0))
    return(m[, col_index])
  }
  letter <- .lms_col_letter(col_index)
  vals <- tryCatch(
    suppressMessages(readxl::read_excel(
      path, sheet = "data", range = paste0(letter, "1:", letter, max_row),
      col_names = FALSE, .name_repair = "minimal")),
    error = function(e) NULL)
  if (is.null(vals) || nrow(vals) == 0L) return(numeric(0))
  suppressWarnings(as.numeric(unlist(vals[[1]])))
}

# Read one series column, filter to the requested frequency, drop NA values.
# Returns df(date, value, period_label) sorted by date.
lms_series <- function(catalog, periods, path, cdid, freq) {
  j <- catalog$col_index[catalog$cdid == cdid]
  if (length(j) == 0L) return(NULL)
  v <- .lms_col_numeric(path, j[1])
  if (length(v) == 0L) return(NULL)

  per_sub <- periods[periods$freq == freq, , drop = FALSE]
  if (nrow(per_sub) == 0L) {
    return(data.frame(date = as.Date(character(0)), value = numeric(0),
                      period_label = character(0), stringsAsFactors = FALSE))
  }
  rows_ok <- per_sub$row <= length(v)
  per_sub <- per_sub[rows_ok, , drop = FALSE]
  vals_at <- v[per_sub$row]
  keep <- !is.na(vals_at)

  out <- data.frame(
    date         = per_sub$date[keep],
    value        = vals_at[keep],
    period_label = per_sub$label[keep],
    stringsAsFactors = FALSE)
  out[order(out$date), , drop = FALSE]
}

# Which frequencies does this CDID actually have at least one value in?
.lms_avail_freqs <- function(lms_data, cdid) {
  j <- lms_data$catalog$col_index[lms_data$catalog$cdid == cdid]
  if (length(j) == 0L) return(character(0))
  v <- .lms_col_numeric(lms_data$path, j[1])
  if (length(v) == 0L) return(character(0))
  periods <- lms_data$periods; res <- character(0)
  for (f in c("M","Q","A")) {
    rows <- periods$row[periods$freq == f]
    rows <- rows[rows <= length(v)]
    if (length(rows) > 0L && any(!is.na(v[rows]))) res <- c(res, f)
  }
  res
}

# ---- series families (breakdowns sharing a title stem) ---------------------

.lms_cpfx <- function(a, b) {            # common prefix length, in characters
  n <- min(nchar(a), nchar(b)); if (n == 0L) return(0L)
  ca <- utf8ToInt(substr(a, 1L, n)); cb <- utf8ToInt(substr(b, 1L, n))
  d <- which(ca != cb); if (length(d)) d[1] - 1L else n
}
.lms_csfx <- function(a, b)              # common suffix length
  .lms_cpfx(intToUtf8(rev(utf8ToInt(a))), intToUtf8(rev(utf8ToInt(b))))
.lms_snap_suf <- function(s, n) {        # pull a suffix length back to a word boundary
  L <- nchar(s); if (n <= 0L) return(0L)
  sub <- substr(s, L - n + 1L, L); m <- gregexpr("[ :/)-]", sub)[[1]]
  if (m[1] == -1L) 0L else (n - min(m) + 1L)
}

# Group the whole catalogue into "breakdown families": series whose titles
# share a stem that ends at a delimiter (" - ", ": ", " by ", " (") and differ
# only in one category slot (industry, age, region, sex, ...). Each title is
# assigned the LONGEST such stem that has >=2 members catalogue-wide, so the
# families come out at a single breakdown level (e.g. vacancies by industry =
# 19, employment by country of birth = 18) rather than as broad grab-bags.
# Returns list(families, fam_of): `families` is sorted largest-first, each
# list(label, cdids, variants, n); `fam_of` maps cdid -> family index (NA if
# the series has no family).
lms_build_families <- function(catalog, min_stem = 10L) {
  titles <- catalog$title; cdids <- catalog$cdid; n <- length(titles)
  ell <- intToUtf8(0x2026L)
  none <- list(families = list(), fam_of = setNames(rep(NA_integer_, n), cdids))
  if (n == 0L) return(none)

  cut_list <- vector("list", n)
  for (i in seq_len(n)) {
    t <- titles[i]
    if (is.na(t) || !nzchar(t)) { cut_list[[i]] <- character(0); next }
    cps <- integer(0)
    for (pat in c(" - ", ": ", " by ", " \\(")) {
      ms <- gregexpr(pat, t, perl = TRUE)[[1]]
      if (ms[1] != -1L) cps <- c(cps, ms + attr(ms, "match.length") - 1L)
    }
    cps <- unique(cps[cps >= min_stem & cps < nchar(t)])
    # substring() (not substr) vectorises over the stop arg -> all cut prefixes
    cut_list[[i]] <- if (length(cps)) substring(t, 1L, cps) else character(0)
  }
  allp <- unlist(cut_list, use.names = FALSE)
  if (!length(allp)) return(none)
  cnt <- table(allp); cntv <- setNames(as.integer(cnt), names(cnt))

  fam_key <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    ps <- cut_list[[i]]; if (!length(ps)) next
    qual <- ps[cntv[ps] >= 2L]
    if (length(qual)) fam_key[i] <- qual[which.max(nchar(qual))]
  }

  keys <- unique(fam_key[!is.na(fam_key)])
  families <- vector("list", length(keys))
  for (ki in seq_along(keys)) {
    key <- keys[ki]; idx <- which(fam_key == key); mt <- titles[idx]
    vars <- substr(mt, nchar(key) + 1L, nchar(mt))
    ovs <- if (length(vars) > 1L) min(vapply(vars[-1], function(x) .lms_csfx(vars[1], x), 0L)) else 0L
    ovs <- .lms_snap_suf(vars[1], ovs)
    sufx   <- if (ovs > 0L) trimws(substr(vars[1], nchar(vars[1]) - ovs + 1L, nchar(vars[1]))) else ""
    vshort <- trimws(substr(vars, 1L, nchar(vars) - ovs)); stem <- trimws(key)
    label  <- if (nzchar(sufx)) paste0(stem, " ", ell, " ", sufx) else paste0(stem, " ", ell)
    families[[ki]] <- list(label = label, cdids = cdids[idx],
                           variants = vshort, n = length(idx))
  }
  families <- families[order(-vapply(families, `[[`, 0L, "n"))]
  fam_of <- setNames(rep(NA_integer_, n), cdids)
  for (fi in seq_along(families)) fam_of[families[[fi]]$cdids] <- fi
  list(families = families, fam_of = fam_of)
}

# ---- baseline lookup --------------------------------------------------------

# Given a (sorted) series and a baseline id, return the (date, value, label)
# of the matching period. Uses an exact match for offset baselines; falls back
# to nearest-within-tolerance for anchored baselines so cross-frequency anchors
# still hit something sensible.
lms_baseline_value <- function(series, freq, baseline_id, catalog_row = NULL,
                               anchor_override = NULL) {
  na_out <- list(date = as.Date(NA), value = NA_real_, period_label = "\u2014")
  if (is.null(series) || nrow(series) == 0L) return(na_out)
  spec <- LMS_BASELINES[[baseline_id]]
  if (is.null(spec) || !freq %in% spec$applies) return(na_out)

  latest_date <- series$date[nrow(series)]

  if (baseline_id == "current") {
    return(list(date = latest_date,
                value = series$value[nrow(series)],
                period_label = series$period_label[nrow(series)]))
  }

  target <- if (!is.null(anchor_override)) anchor_override
            else if (!is.null(spec$anchor)) {
              a <- spec$anchor
              if (!is.null(names(a)) && freq %in% names(a)) a[[freq]] else a[[1]]
            } else .lms_last_day_of_offset_month(latest_date, spec$offset_months)

  diffs <- abs(as.integer(series$date - target))
  nearest <- which.min(diffs)
  if (length(nearest) == 0L) return(na_out)

  tol <- switch(freq, "M" = 20L, "Q" = 50L, "A" = 200L, 20L)
  if (diffs[nearest] > tol) return(na_out)

  list(date         = series$date[nearest],
       value        = series$value[nearest],
       period_label = series$period_label[nearest])
}

# The Covid baseline is the Dec 2019-Feb 2020 quarter. The monthly LFS series
# is a 3-month rolling figure labelled by its MIDDLE month, so that quarter is
# the "Jan 2020" point; resolve "since Covid" from the monthly series whatever
# frequency the analyst chose, falling back to the chosen frequency only when a
# series has no monthly data. Vacancies are the exception: the Vacancy Survey
# reports on calendar quarters, so their pre-Covid baseline is Jan-Mar 2020 -
# the "Feb 2020" monthly point, one month later.
.lms_covid_base <- function(catalog, periods, path, cdid, crow, series, freq) {
  is_vac <- !is.null(crow) && isTRUE(grepl("vacanc", crow$title, ignore.case = TRUE))
  ov <- if (is_vac) as.Date("2020-02-29") else NULL
  mser <- if (identical(freq, "M")) series
          else tryCatch(lms_series(catalog, periods, path, cdid, "M"),
                        error = function(e) NULL)
  if (!is.null(mser) && nrow(mser) > 0L)
    return(lms_baseline_value(mser, "M", "precovid", crow, anchor_override = ov))
  lms_baseline_value(series, freq, "precovid", crow, anchor_override = ov)
}

# ---- unit + formatting ------------------------------------------------------

.lms_is_rate <- function(catalog_row) {
  u <- paste(catalog_row$unit, catalog_row$pre_unit)
  t <- catalog_row$title
  isTRUE(grepl("%", u, fixed = TRUE)) || isTRUE(grepl("\\brate\\b", t, ignore.case = TRUE))
}

.lms_unit_label <- function(catalog_row) {
  if (.lms_is_rate(catalog_row)) return("%")
  u <- trimws(as.character(catalog_row$unit))
  if (is.na(u) || u == "" || toupper(u) == "NA") return("")
  u
}

.lms_format_value <- function(x, is_rate) {
  if (is.na(x)) return("\u2014")
  if (is_rate)  return(formatC(x, format = "f", digits = 1))
  if (abs(x - round(x)) < 1e-9 && abs(x) < 1e15) return(formatC(x, format = "d", big.mark = ","))
  formatC(x, format = "f", digits = 2, big.mark = ",", drop0trailing = TRUE)
}

.lms_format_delta <- function(x, is_rate) {
  if (is.na(x)) return("\u2014")
  sign_ch <- if (x > 0) "+" else if (x < 0) "\u2212" else ""    # \u2212 = U+2212
  body <- if (is_rate) formatC(abs(x), format = "f", digits = 1)
          else if (abs(x - round(x)) < 1e-9 && abs(x) < 1e15) formatC(abs(x), format = "d", big.mark = ",")
          else formatC(abs(x), format = "f", digits = 2, big.mark = ",", drop0trailing = TRUE)
  suffix <- if (is_rate) " pp" else ""
  if (x == 0) return(paste0("0", suffix))
  paste0(sign_ch, body, suffix)
}

# ---- summary-line phrasing --------------------------------------------------

.lms_baseline_phrase <- function(baseline_id) {
  switch(baseline_id,
         month    = "on the month",
         quarter  = "on the quarter",
         year     = "on the year",
         precovid = "since Covid",
         election = "since the general election",
         "vs baseline")
}

format_summary_line <- function(catalog_row, baseline_id, latest, base, freq) {
  if (is.null(latest) || is.na(latest$value)) return("")
  if (is.null(base)   || is.na(base$value))   return("")
  is_rate <- .lms_is_rate(catalog_row)
  unit_lbl <- .lms_unit_label(catalog_row)
  delta <- latest$value - base$value
  cur_s  <- .lms_format_value(latest$value, is_rate)
  prev_s <- .lms_format_value(base$value,   is_rate)
  abs_s  <- if (is_rate) paste0(formatC(abs(delta), format = "f", digits = 1), " percentage points")
            else paste0(.lms_format_value(abs(delta), FALSE),
                        if (nzchar(unit_lbl) && unit_lbl != "%") paste0(" ", unit_lbl) else "")
  per <- latest$period_label
  phrase <- .lms_baseline_phrase(baseline_id)
  title <- catalog_row$title
  base_per <- base$period_label

  if (abs(delta) < 1e-9) {
    return(sprintf("%s: was unchanged at %s%s in %s.",
                   title, cur_s,
                   if (is_rate) "%" else if (nzchar(unit_lbl)) paste0(" ", unit_lbl) else "",
                   per))
  }
  if (is_rate) {
    verb <- if (delta > 0) "\u2191" else "\u2193"  # \u2191 \u2193
    return(sprintf("%s: %s by %s %s to %s, from %s%% to %s%%.",
                   title, verb, abs_s, phrase, per, prev_s, cur_s))
  }
  verb <- if (delta > 0) "rose" else "fell"
  sprintf("%s: %s by %s %s to %s, from %s to %s.",
          title, verb, abs_s, phrase, per, prev_s, cur_s)
}

# Historical context for the latest value: where it sits in the whole series.
# Returns e.g. "the highest since Aug 2021", "the lowest since 2013", or for a
# series extreme "the highest since 1971"; "" when the latest value is not a
# notable (>= ~1 year) high or low. Computed off the full series held in memory.
.lms_context_clause <- function(series, freq) {
  n <- if (is.null(series)) 0L else nrow(series)
  if (n < 6L) return("")
  v <- series$value; labs <- series$period_label; cur <- v[n]
  if (is.na(cur)) return("")
  earlier <- v[seq_len(n - 1L)]
  if (all(is.na(earlier))) return("")
  min_span <- switch(freq, M = 12L, Q = 4L, A = 3L, 12L)
  yr <- function(lbl) { m <- regmatches(lbl, regexpr("[0-9]{4}", lbl)); if (length(m)) m else lbl }

  hi_idx <- suppressWarnings(max(which(earlier >= cur)))   # most recent >= current
  lo_idx <- suppressWarnings(max(which(earlier <= cur)))   # most recent <= current
  if (!is.finite(hi_idx)) return(sprintf("the highest since %s", yr(labs[1])))  # series high
  if (!is.finite(lo_idx)) return(sprintf("the lowest since %s",  yr(labs[1])))  # series low
  hi_span <- n - hi_idx; lo_span <- n - lo_idx
  if (hi_span >= lo_span && hi_span >= min_span) return(sprintf("the highest since %s", labs[hi_idx]))
  if (lo_span >  hi_span && lo_span >= min_span) return(sprintf("the lowest since %s",  labs[lo_idx]))
  ""
}

# ---- preview data frame (used in-app) --------------------------------------

# Returns a data.frame with cols: Series, "Latest period", "Latest value",
# and a \u0394 column per selected baseline (union across selections).
build_custom_preview_df <- function(selections, catalog, periods, path) {
  if (length(selections) == 0L) return(NULL)

  # determine union of \u0394 baselines actually requested (excluding "current")
  union_b <- unique(unlist(lapply(selections, function(s)
    setdiff(s$baselines, "current"))))
  delta_order <- c("month","quarter","year","precovid","election")
  union_b <- intersect(delta_order, union_b)

  delta_headers <- vapply(union_b, function(b) {
    lbl <- LMS_BASELINES[[b]]$label
    paste0("Change ", tolower(substr(lbl, 1L, 1L)),
           substr(lbl, 2L, nchar(lbl)))
  }, "")

  cols <- c("Series", "Latest period", "Latest value", delta_headers)
  rows <- vector("list", length(selections))

  for (i in seq_along(selections)) {
    sel <- selections[[i]]
    crow <- catalog[catalog$cdid == sel$cdid, , drop = FALSE]
    if (nrow(crow) == 0L) next
    is_rate <- .lms_is_rate(crow)
    series <- lms_series(catalog, periods, path, sel$cdid, sel$freq)
    if (is.null(series) || nrow(series) == 0L) {
      r <- c(paste0(sel$cdid, " \u2014 ", crow$title), "\u2014", "\u2014",
             rep("\u2014", length(union_b)))
      rows[[i]] <- r; next
    }
    latest <- list(date = series$date[nrow(series)],
                   value = series$value[nrow(series)],
                   period_label = series$period_label[nrow(series)])
    lat_s  <- .lms_format_value(latest$value, is_rate)
    if (is_rate) lat_s <- paste0(lat_s, "%")
    base_vals <- vapply(union_b, function(b) {
      if (!b %in% sel$baselines) return("\u2014")
      spec <- LMS_BASELINES[[b]]
      if (!sel$freq %in% spec$applies) return("\u2014")
      base <- if (b == "precovid")
                .lms_covid_base(catalog, periods, path, sel$cdid, crow, series, sel$freq)
              else lms_baseline_value(series, sel$freq, b, crow)
      if (is.na(base$value)) return("\u2014")
      .lms_format_delta(latest$value - base$value, is_rate)
    }, "")
    rows[[i]] <- c(paste0(sel$cdid, " \u2014 ", crow$title),
                   latest$period_label, lat_s, base_vals)
  }

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(NULL)
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

# ---- Word OOXML body fragment ----------------------------------------------

# Generate the candidate summary lines for a set of selections. Returns a list
# of records: list(key, cdid, baseline, text). `key` = "<cdid>|<baseline>" \u2014 a
# stable id used to tie each line to its per-line include checkbox.
lms_summary_lines <- function(selections, catalog, periods, path) {
  out <- list()
  for (sel in selections) {
    crow <- catalog[catalog$cdid == sel$cdid, , drop = FALSE]
    if (nrow(crow) == 0L) next
    series <- lms_series(catalog, periods, path, sel$cdid, sel$freq)
    if (is.null(series) || nrow(series) == 0L) next
    latest <- list(date = series$date[nrow(series)],
                   value = series$value[nrow(series)],
                   period_label = series$period_label[nrow(series)])
    ctx <- .lms_context_clause(series, sel$freq)   # "the highest since ..." etc.
    for (b in setdiff(sel$baselines, "current")) {
      spec <- LMS_BASELINES[[b]]
      if (is.null(spec) || !sel$freq %in% spec$applies) next
      base <- if (b == "precovid")
                .lms_covid_base(catalog, periods, path, sel$cdid, crow, series, sel$freq)
              else lms_baseline_value(series, sel$freq, b, crow)
      if (is.na(base$value)) next
      line <- format_summary_line(crow, b, latest, base, sel$freq)
      if (!nzchar(line)) next
      if (nzchar(ctx)) line <- paste0(sub("\\.\\s*$", "", line), ", ", ctx, ".")
      out[[length(out) + 1L]] <- list(
        key = paste0(sel$cdid, "|", b), cdid = sel$cdid,
        baseline = b, text = line)
    }
  }
  out
}

# Build the body fragment to splice in. No new parts; no relationships; no
# content-type changes \u2014 pure WordprocessingML body content. `line_keys`, when
# non-NULL, restricts the bullet list to those summary-line keys (from
# lms_summary_lines); NULL includes every line.
build_custom_indicators_xml <- function(selections, catalog, periods, path,
                                        line_keys = NULL) {
  if (length(selections) == 0L) return("")
  esc <- .wc_xml_escape

  # Page break + heading
  pagebreak <- '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'
  heading <- paste0(
    '<w:p><w:pPr><w:spacing w:before="120" w:after="160"/></w:pPr>',
    '<w:r><w:rPr><w:b/><w:sz w:val="32"/></w:rPr>',
    '<w:t>Custom indicators</w:t></w:r></w:p>')

  # Bullet paragraphs \u2014 one per included summary line. Use the "\u2022" glyph
  # to avoid any numbering.xml dependency.
  recs <- lms_summary_lines(selections, catalog, periods, path)
  if (!is.null(line_keys)) recs <- Filter(function(r) r$key %in% line_keys, recs)
  bullets <- vapply(recs, function(rec) paste0(
    '<w:p><w:pPr><w:ind w:left="360" w:hanging="200"/>',
    '<w:spacing w:before="40" w:after="40"/></w:pPr>',
    '<w:r><w:rPr><w:sz w:val="22"/></w:rPr>',
    '<w:t xml:space="preserve">\u2022  </w:t></w:r>',
    '<w:r><w:rPr><w:sz w:val="22"/></w:rPr>',
    '<w:t xml:space="preserve">', esc(rec$text), '</w:t></w:r></w:p>'), "")

  # Comparison table \u2014 same columns as the preview df.
  df <- build_custom_preview_df(selections, catalog, periods, path)
  if (is.null(df) || nrow(df) == 0L) {
    return(paste0(pagebreak, heading, paste(bullets, collapse = ""), '<w:p/>'))
  }
  hdr <- names(df)
  n_cols <- length(hdr)
  total_w <- 10466L
  first_w <- 3800L
  other_w <- as.integer(floor((total_w - first_w) / max(1L, n_cols - 1L)))
  widths <- c(first_w, rep(other_w, n_cols - 1L))

  cell <- function(text, bold = FALSE, w) {
    rpr <- if (bold) '<w:rPr><w:b/><w:sz w:val="20"/></w:rPr>'
           else      '<w:rPr><w:sz w:val="20"/></w:rPr>'
    paste0(
      '<w:tc><w:tcPr><w:tcW w:w="', w, '" w:type="dxa"/></w:tcPr>',
      '<w:p><w:pPr><w:spacing w:before="40" w:after="40"/></w:pPr>',
      '<w:r>', rpr,
      '<w:t xml:space="preserve">', esc(text), '</w:t></w:r></w:p></w:tc>')
  }
  row_xml <- function(cells, widths, bold = FALSE) {
    cs <- vapply(seq_along(cells),
                 function(k) cell(cells[k], bold, widths[k]), "")
    paste0("<w:tr>", paste(cs, collapse = ""), "</w:tr>")
  }
  border_all <- paste0(
    '<w:tblBorders>',
    '<w:top    w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '<w:left   w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '<w:right  w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>',
    '</w:tblBorders>')

  grid <- paste0(vapply(widths, function(w)
    sprintf('<w:gridCol w:w="%d"/>', w), ""), collapse = "")
  body_rows <- vapply(seq_len(nrow(df)), function(i)
    row_xml(as.character(unlist(df[i, ])), widths), "")
  header_row <- row_xml(hdr, widths, bold = TRUE)

  tbl <- paste0(
    '<w:tbl><w:tblPr><w:tblW w:w="', total_w, '" w:type="dxa"/>',
    '<w:tblLayout w:type="fixed"/>',
    border_all,
    '</w:tblPr>',
    '<w:tblGrid>', grid, '</w:tblGrid>',
    header_row, paste(body_rows, collapse = ""),
    '</w:tbl>')

  paste0(pagebreak, heading, paste(bullets, collapse = ""), tbl, '<w:p/>')
}

# ---- inject the fragment into a finished .docx ------------------------------

# Splice `body_fragment` into the produced briefing immediately before the
# last <w:sectPr> in word/document.xml. No new parts; no relationships; no
# content-type changes. Mirrors the zip-surgery flow in
# word_charts.R::append_key_charts_page().
#
# Insertion order \u2014 IMPORTANT: this function and append_key_charts_page both
# insert before the LAST <w:sectPr>. To get document order OECD -> Custom ->
# Charts, the caller must run append_custom_indicators FIRST, then
# append_key_charts_page (the second call's insert ends up adjacent to sectPr,
# pushing the earlier insert into the middle).
append_custom_indicators <- function(docx_path, body_fragment) {
  if (is.null(body_fragment) || !nzchar(body_fragment)) return(invisible(docx_path))

  tmp <- tempfile("lms_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(docx_path, exdir = tmp)

  doc_path <- file.path(tmp, "word", "document.xml")
  doc <- .wc_read_text(doc_path)
  hits <- gregexpr("<w:sectPr", doc, fixed = TRUE)[[1]]
  if (length(hits) < 1L || hits[1] == -1L) {
    warning("Custom indicators: could not locate <w:sectPr> \u2014 section skipped")
    return(invisible(docx_path))
  }
  pos <- hits[length(hits)]
  doc <- paste0(substr(doc, 1, pos - 1L), body_fragment,
                substr(doc, pos, nchar(doc)))
  .wc_write_text(doc, doc_path)

  out_abs <- normalizePath(docx_path, mustWork = FALSE)
  if (file.exists(out_abs)) unlink(out_abs)
  zip::zip(zipfile = out_abs, files = list.files(tmp), root = tmp)
  invisible(docx_path)
}

# ---- audit-workbook data --------------------------------------------------

# Build the data for the Excel audit workbook's Custom Indicators sheets,
# laid out like the workbook's other comparison tables: one ROW per selected
# series, one COLUMN for the latest value plus one per requested baseline
# change. Values stay numeric so the workbook applies its own number formats
# and sign-based green/red conditional formatting.
#   grid           one row per series: Series, CDID, Frequency, Latest period,
#                  Current, then one "Change on the ..." column per baseline.
#   grid_kind      per-row "rate" / "gbp" / "num" (drives number format).
#   change_headers names of the change columns (the ones coloured by sign).
#   data_blocks    wide full-series blocks, one per frequency present:
#                  list(freq, df[Period + one col per CDID], titles, cdids, kind).
#   lines          included summary-line texts (after the line_keys filter).
#   provenance     one-line source note.
build_custom_audit <- function(selections, catalog, periods, path, line_keys = NULL) {
  flab <- c(M = "Monthly", Q = "Quarterly", A = "Annual")
  gbp_char <- intToUtf8(163L)

  empty <- list(grid = NULL, grid_kind = character(0), change_headers = character(0),
                data_blocks = list(), lines = character(0), provenance = "")
  if (length(selections) == 0L) return(empty)

  # union of requested baselines (the change columns), in canonical order
  delta_order <- c("month", "quarter", "year", "precovid", "election")
  change_ids <- intersect(delta_order, unique(unlist(lapply(selections,
                  function(s) setdiff(s$baselines, "current")))))
  change_headers <- vapply(change_ids, function(b) {
    lbl <- LMS_BASELINES[[b]]$label
    paste0("Change ", tolower(substr(lbl, 1L, 1L)), substr(lbl, 2L, nchar(lbl)))
  }, "")

  n <- length(selections)
  g_series <- character(n); g_cdid <- character(n); g_freq <- character(n)
  g_lper <- rep(NA_character_, n); g_cur <- rep(NA_real_, n); g_kind <- rep("num", n)
  chg <- matrix(NA_real_, nrow = n, ncol = length(change_ids),
                dimnames = list(NULL, change_ids))
  store <- list()   # series kept for the wide data sheet

  for (i in seq_along(selections)) {
    sel <- selections[[i]]
    crow <- catalog[catalog$cdid == sel$cdid, , drop = FALSE]
    if (nrow(crow) == 0L) { g_series[i] <- sel$cdid; g_cdid[i] <- sel$cdid; next }
    is_rate <- .lms_is_rate(crow)
    g_series[i] <- crow$title
    g_cdid[i]   <- sel$cdid
    g_freq[i]   <- flab[[sel$freq]]
    g_kind[i]   <- if (is_rate) "rate"
                   else if (grepl(gbp_char, paste(crow$unit, crow$pre_unit), fixed = TRUE)) "gbp"
                   else "num"
    series <- lms_series(catalog, periods, path, sel$cdid, sel$freq)
    if (is.null(series) || nrow(series) == 0L) next
    store[[length(store) + 1L]] <- list(cdid = sel$cdid, title = crow$title,
        freq = sel$freq, kind = g_kind[i], df = series)
    last <- nrow(series)
    g_lper[i] <- series$period_label[last]; g_cur[i] <- series$value[last]
    for (b in change_ids) {
      if (!b %in% sel$baselines || !sel$freq %in% LMS_BASELINES[[b]]$applies) next
      base <- if (b == "precovid")
                .lms_covid_base(catalog, periods, path, sel$cdid, crow, series, sel$freq)
              else lms_baseline_value(series, sel$freq, b, crow)
      if (!is.na(base$value)) chg[i, b] <- series$value[last] - base$value
    }
  }

  grid <- data.frame(Series = g_series, CDID = g_cdid, Frequency = g_freq,
                     `Latest period` = g_lper, Current = g_cur,
                     check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(change_ids)) grid[[change_headers[j]]] <- chg[, j]

  # wide full series, grouped by frequency (mirrors the LMS source layout)
  data_blocks <- list()
  for (fr in c("M", "Q", "A")) {
    ss <- Filter(function(x) x$freq == fr, store)
    if (length(ss) == 0L) next
    per <- do.call(rbind, lapply(ss, function(x) x$df[, c("date", "period_label")]))
    per <- per[!duplicated(per$period_label), , drop = FALSE]
    per <- per[order(per$date), , drop = FALSE]
    block <- data.frame(Period = per$period_label, check.names = FALSE,
                        stringsAsFactors = FALSE)
    for (x in ss) block[[x$cdid]] <- x$df$value[match(per$period_label, x$df$period_label)]
    data_blocks[[length(data_blocks) + 1L]] <- list(
      freq = flab[[fr]], df = block,
      titles = vapply(ss, `[[`, "", "title"),
      cdids  = vapply(ss, `[[`, "", "cdid"),
      kind   = vapply(ss, `[[`, "", "kind"))
  }

  recs <- lms_summary_lines(selections, catalog, periods, path)
  if (!is.null(line_keys)) recs <- Filter(function(r) r$key %in% line_keys, recs)
  lines <- vapply(recs, `[[`, "", "text")

  rel <- if (nrow(catalog) > 0 && nzchar(catalog$release[1])) catalog$release[1] else "n/a"

  list(grid = grid, grid_kind = g_kind, change_headers = unname(change_headers),
       data_blocks = data_blocks, lines = lines,
       provenance = paste0(
         "Source: ONS Labour Market Statistics time-series (LMS), CDID-referenced. ",
         "Every figure traces to the uploaded LMS file -> CDID -> period. ",
         "LMS release: ", rel, "."))
}
