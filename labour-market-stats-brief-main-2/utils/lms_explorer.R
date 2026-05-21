# LMS Explorer \u2014 read the ONS bulk LMS time-series file, let the user pick
# any series + baselines, and append a "Custom indicators" section to the
# Word briefing. Sourced AFTER utils/word_charts.R so we can reuse
# .wc_xml_escape, .wc_read_text and .wc_write_text from there.

# ---- baseline registry ------------------------------------------------------
# Six entries. `applies` says which frequencies a baseline is valid for.
# `offset_months` is used for month/quarter/year baselines (uniformly in months
# so end-of-month arithmetic always works). Anchored baselines (pre-COVID,
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
  precovid = list(id = "precovid", label = "Since pre-COVID",  applies = c("M","Q","A"),
                  anchor = as.Date("2020-02-29")),
  election = list(id = "election", label = "Since election",   applies = c("M","Q","A"),
                  anchor = as.Date("2024-07-31"))
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
# friendly display string. Quarters are rewritten as "Mmm-Mmm YYYY" (e.g.
# "1959 Q2" -> "Apr-Jun 1959") and months as "Mmm YYYY" ("2026 MAR" ->
# "Mar 2026"); annuals stay as the bare year.
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
    end_mo   <- q * 3L
    start_mo <- end_mo - 2L
    disp <- sprintf("%s-%s %d",
                    .LMS_MONTH_NAMES_SHORT[start_mo],
                    .LMS_MONTH_NAMES_SHORT[end_mo],
                    yr)
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

# Read just one column from the LMS file, filter to the requested frequency,
# drop NA values. Returns df(date, value, period_label) sorted by date.
lms_series <- function(catalog, periods, path, cdid, freq) {
  j <- catalog$col_index[catalog$cdid == cdid]
  if (length(j) == 0L) return(NULL)
  j <- j[1]
  letter <- .lms_col_letter(j)

  vals <- tryCatch(
    suppressMessages(readxl::read_excel(
      path, sheet = "data",
      range = paste0(letter, "1:", letter, "1553"),
      col_names = FALSE, .name_repair = "minimal")),
    error = function(e) NULL)
  if (is.null(vals) || nrow(vals) == 0L) return(NULL)
  v <- suppressWarnings(as.numeric(unlist(vals[[1]])))

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
  res <- character(0)
  for (f in c("M","Q","A")) {
    s <- lms_series(lms_data$catalog, lms_data$periods, lms_data$path, cdid, f)
    if (!is.null(s) && nrow(s) > 0L) res <- c(res, f)
  }
  res
}

# ---- baseline lookup --------------------------------------------------------

# Given a (sorted) series and a baseline id, return the (date, value, label)
# of the matching period. Uses an exact match for offset baselines; falls back
# to nearest-within-tolerance for anchored baselines so cross-frequency anchors
# still hit something sensible.
lms_baseline_value <- function(series, freq, baseline_id, catalog_row = NULL) {
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

  target <- if (!is.null(spec$anchor)) spec$anchor
            else .lms_last_day_of_offset_month(latest_date, spec$offset_months)

  diffs <- abs(as.integer(series$date - target))
  nearest <- which.min(diffs)
  if (length(nearest) == 0L) return(na_out)

  tol <- switch(freq, "M" = 20L, "Q" = 50L, "A" = 200L, 20L)
  if (diffs[nearest] > tol) return(na_out)

  list(date         = series$date[nearest],
       value        = series$value[nearest],
       period_label = series$period_label[nearest])
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
         precovid = "since pre-COVID",
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
      base <- lms_baseline_value(series, sel$freq, b, crow)
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

# Build the body fragment to splice in. No new parts; no relationships; no
# content-type changes \u2014 pure WordprocessingML body content.
build_custom_indicators_xml <- function(selections, catalog, periods, path) {
  if (length(selections) == 0L) return("")
  esc <- .wc_xml_escape

  # Page break + heading
  pagebreak <- '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'
  heading <- paste0(
    '<w:p><w:pPr><w:spacing w:before="120" w:after="160"/></w:pPr>',
    '<w:r><w:rPr><w:b/><w:sz w:val="32"/></w:rPr>',
    '<w:t>Custom indicators</w:t></w:r></w:p>')

  # Bullet paragraphs (one per series x ticked baseline yielding a usable \u0394).
  # Use a literal "\u2022" glyph to avoid any numbering.xml dependency.
  bullets <- character(0)
  for (sel in selections) {
    if (!isTRUE(sel$summary)) next
    crow <- catalog[catalog$cdid == sel$cdid, , drop = FALSE]
    if (nrow(crow) == 0L) next
    series <- lms_series(catalog, periods, path, sel$cdid, sel$freq)
    if (is.null(series) || nrow(series) == 0L) next
    latest <- list(date = series$date[nrow(series)],
                   value = series$value[nrow(series)],
                   period_label = series$period_label[nrow(series)])
    for (b in setdiff(sel$baselines, "current")) {
      spec <- LMS_BASELINES[[b]]
      if (is.null(spec) || !sel$freq %in% spec$applies) next
      base <- lms_baseline_value(series, sel$freq, b, crow)
      if (is.na(base$value)) next
      line <- format_summary_line(crow, b, latest, base, sel$freq)
      if (!nzchar(line)) next
      bullets <- c(bullets, paste0(
        '<w:p><w:pPr><w:ind w:left="360" w:hanging="200"/>',
        '<w:spacing w:before="40" w:after="40"/></w:pPr>',
        '<w:r><w:rPr><w:sz w:val="22"/></w:rPr>',
        '<w:t xml:space="preserve">\u2022  </w:t></w:r>',
        '<w:r><w:rPr><w:sz w:val="22"/></w:rPr>',
        '<w:t xml:space="preserve">', esc(line), '</w:t></w:r></w:p>'))
    }
  }

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
