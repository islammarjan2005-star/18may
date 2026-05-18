# builds the excel audit workbook with live formulas and comparison rows

suppressPackageStartupMessages({
  library(openxlsx)
})

if (!exists("parse_manual_month", inherits = TRUE)) source("utils/helpers.R")
if (!exists("COVID_DATE",         inherits = TRUE)) source("utils/config.R")
source("utils/excel_helpers.R", local = FALSE)

# ons suppression markers ([x], [c], etc.) get coerced to na during numeric conversion
.safe_read <- function(path, sheet, ...) {
  if (is.null(path)) return(data.frame())
  tbl <- tryCatch(
    suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE, ...)),
    error = function(e) data.frame()
  )
  if (nrow(tbl) == 0) return(tbl)
  
  ons_markers <- c("[x]", "[c]", "[z]", "..", "-", "~", ":", "[e]", "**",
                   "[x] ", " [x]", "[c] ", " [c]")
  
  # keep raw text so we can restore suppression markers and headers later
  raw_text <- lapply(seq_len(ncol(tbl)), function(ci) as.character(tbl[[ci]]))
  
  for (ci in seq_len(ncol(tbl))) {
    vals <- tbl[[ci]]
    if (!is.character(vals)) next
    
    trimmed <- trimws(vals)
    num_vals <- suppressWarnings(as.numeric(vals))
    
    is_empty   <- is.na(vals) | nchar(trimmed) == 0
    is_marker  <- trimmed %in% ons_markers
    is_numeric <- !is.na(num_vals) & !is_empty
    
    n_non_empty <- sum(!is_empty)
    n_data_vals <- sum(is_numeric | is_marker)
    
    if (n_non_empty > 0 && n_data_vals / n_non_empty > 0.5) {
      tbl[[ci]] <- num_vals
    }
  }
  attr(tbl, "raw_text") <- raw_text
  tbl
}

.first_num_r <- function(tbl, col) {
  vals <- suppressWarnings(as.numeric(as.character(tbl[[col]])))
  idx <- which(!is.na(vals))
  if (length(idx) == 0) NA_integer_ else idx[1]
}

# row indices in `tbl` whose column-1 label looks like a date/period,
# tolerating trailing ONS revision markers such as " [p]" / " [r]"
.date_label_rows <- function(tbl) {
  if (nrow(tbl) == 0) return(integer(0))
  col1 <- tolower(trimws(as.character(tbl[[1]])))
  mon  <- "jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"
  fmon <- "january|february|march|april|may|june|july|august|september|october|november|december"
  mk   <- "(\\s*\\[[a-z]\\])?$"
  pats <- c(
    paste0("^(", mon, ")-(", mon, ")\\s+\\d{4}", mk),
    paste0("^(", fmon, ")\\s+\\d{4}", mk),
    paste0("^(", mon, ")\\s+\\d{2,4}", mk)
  )
  for (p in pats) {
    idx <- which(grepl(p, col1))
    if (length(idx) > 0) return(idx)
  }
  integer(0)
}

# output row of the first real data row. More reliable than .first_num_r,
# which is fooled when ONS metadata rows carry numeric values above the data.
.data_row <- function(tbl, write_row) {
  idx <- .date_label_rows(tbl)
  if (length(idx) > 0) return(idx[1] + write_row - 1)
  fr <- .first_num_r(tbl, 2)
  if (!is.na(fr)) fr + write_row - 1 else write_row
}

# output row of the last real data row
.data_end_row <- function(tbl, write_row) {
  idx <- .date_label_rows(tbl)
  if (length(idx) > 0) return(idx[length(idx)] + write_row - 1)
  write_row + nrow(tbl) - 1
}

.lfs_metric <- function(tbl, col, labels) {
  rows <- vapply(labels, function(l) .find_row(tbl, l), integer(1))
  vals <- vapply(seq_along(rows), function(i) .cell_num(tbl, rows[i], col), numeric(1))
  names(vals) <- c("cur", "q", "y", "covid", "elec")
  list(cur = vals["cur"], dq = vals["cur"] - vals["q"],
       dy = vals["cur"] - vals["y"], dc = vals["cur"] - vals["covid"],
       de = vals["cur"] - vals["elec"])
}

.restore_text <- function(wb, sheet_name, tbl, start_row) {
  raw_text <- attr(tbl, "raw_text")
  if (is.null(raw_text)) return(invisible(NULL))
  for (ci in seq_len(ncol(tbl))) {
    if (!is.numeric(tbl[[ci]])) next
    orig <- raw_text[[ci]]
    lost <- which(is.na(tbl[[ci]]) & !is.na(orig) & nchar(trimws(orig)) > 0)
    if (length(lost) == 0) next
    for (ri in lost) {
      writeData(wb, sheet_name, orig[ri], startRow = start_row + ri - 1, startCol = ci)
    }
  }
}

# strip ons header/footnote rows, keep only rows with actual data
.trim_source <- function(tbl) {
  if (ncol(tbl) < 2) return(tbl)
  num_cols <- which(vapply(tbl, is.numeric, logical(1)))
  if (length(num_cols) == 0) return(tbl)
  has_num <- rowSums(!is.na(tbl[, num_cols, drop = FALSE])) > 0
  rows <- which(has_num)
  if (length(rows) == 0) return(tbl)
  kept <- rows[1]:rows[length(rows)]
  out <- tbl[kept, , drop = FALSE]
  # keep raw_text aligned with the trimmed rows, else .restore_text recycles
  # the longer untrimmed vector and writes stray text past the data block
  rt <- attr(tbl, "raw_text")
  if (!is.null(rt)) attr(out, "raw_text") <- lapply(rt, function(col) col[kept])
  out
}

.hs <- function() createStyle(fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
                              fgFill = "#366092", halign = "center", textDecoration = "bold",
                              border = "TopBottomLeftRight", borderColour = "#244062")
.ts <- function() createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fontColour = "#1F4E79")
.ss <- function() createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold", fontColour = "#505050")
.pos <- function() createStyle(fontColour = "#006100", fgFill = "#C6EFCE")
.neg <- function() createStyle(fontColour = "#9C0006", fgFill = "#FFC7CE")
.sep <- function() createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#1F4E79",
                               fgFill = "#BDD7EE", halign = "center", valign = "center")

.cmp_label <- function() createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold",
                                     fgFill = "#EAF3FB")
.cmp_sep   <- function() createStyle(border = "Bottom", borderColour = "#2F5496", borderStyle = "medium",
                                     fgFill = "#EAF3FB")
.id_code   <- function() createStyle(fontName = "Arial", fontSize = 9, fontColour = "#808080",
                                     textDecoration = "italic")
.date_fmt  <- function() createStyle(numFmt = "MMM-YY")
.num_fmt   <- function() createStyle(numFmt = "#,##0")
.pct_fmt   <- function() createStyle(numFmt = "0.0%")
.pp_fmt    <- function() createStyle(numFmt = "0.0")
.pp2_fmt   <- function() createStyle(numFmt = "0.00")
.gbp_fmt   <- function() createStyle(numFmt = "\"\u00a3\"#,##0")
.data_font <- function() createStyle(fontName = "Arial", fontSize = 10)


.col_letter <- function(n) {
  result <- ""
  while (n > 0) {
    n <- n - 1
    result <- paste0(LETTERS[(n %% 26) + 1], result)
    n <- n %/% 26
  }
  result
}

.find_output_row <- function(tbl, date_col, target_label, data_start_row) {
  col_vals <- trimws(as.character(tbl[[date_col]]))
  idx <- which(tolower(col_vals) == tolower(trimws(target_label)))
  if (length(idx) == 0) return(NA_integer_)
  idx[1] + data_start_row - 1
}

.find_output_row_date <- function(tbl, date_col, target_date, data_start_row) {
  dates <- .detect_dates(tbl[[date_col]])
  idx <- which(dates == target_date)
  if (length(idx) == 0) return(NA_integer_)
  idx[1] + data_start_row - 1
}

# count ignores restored text markers
.fml_last <- function(cl, sr, off = 0) {
  rng <- sprintf("%s$%d:%s$1048576", cl, sr, cl)
  if (off == 0) sprintf("INDEX(%s,COUNT(%s))", rng, rng)
  else sprintf("INDEX(%s,COUNT(%s)%+d)", rng, rng, off)
}

.fml_avg_last <- function(cl, sr, n = 3) {
  rng <- sprintf("%s$%d:%s$1048576", cl, sr, cl)
  sprintf("AVERAGE(OFFSET(INDEX(%s,COUNT(%s)),-%d,0,%d,1))", rng, rng, n - 1, n)
}

.fml_avg_offset <- function(cl, sr, off, n = 3) {
  rng <- sprintf("%s$%d:%s$1048576", cl, sr, cl)
  sprintf("AVERAGE(OFFSET(INDEX(%s,COUNT(%s)),%d,0,%d,1))", rng, rng, off, n)
}

.fml_change_avg <- function(cl, sr, off) {
  paste0(.fml_avg_last(cl, sr), "-", .fml_avg_offset(cl, sr, off))
}

.fml_change_fixed <- function(cl, sr, r1, r2) {
  sprintf("%s-AVERAGE(%s$%d:%s$%d)", .fml_avg_last(cl, sr), cl, r1, cl, r2)
}

.fml_change_single <- function(cl, sr, r) {
  sprintf("%s-%s$%d", .fml_last(cl, sr), cl, r)
}

.fml_pct_avg <- function(cl, sr, off) {
  sprintf("IFERROR(%s/%s-1,\"\")", .fml_avg_last(cl, sr), .fml_avg_offset(cl, sr, off))
}

.fml_pct_fixed <- function(cl, sr, r1, r2) {
  sprintf("IFERROR(%s/AVERAGE(%s$%d:%s$%d)-1,\"\")", .fml_avg_last(cl, sr), cl, r1, cl, r2)
}

.fml_idx_change <- function(cl, sr, off) {
  sprintf("%s-%s", .fml_last(cl, sr), .fml_last(cl, sr, -off))
}

.fml_idx_pct <- function(cl, sr, off) {
  sprintf("IFERROR(%s/%s-1,\"\")", .fml_last(cl, sr), .fml_last(cl, sr, -off))
}

.fml_idx_change_fixed <- function(cl, sr, r) {
  sprintf("%s-%s$%d", .fml_last(cl, sr), cl, r)
}

.fml_idx_pct_fixed <- function(cl, sr, r) {
  sprintf("IFERROR(%s/%s$%d-1,\"\")", .fml_last(cl, sr), cl, r)
}

.fml_max <- function(cl, sr) {
  sprintf("MAX(%s$%d:%s$1048576)", cl, sr, cl)
}

.fml_rank <- function(cell, range_start, range_end) {
  sprintf("RANK(%s,%s:%s)", cell, range_start, range_end)
}

.fml_avg_range <- function(cl, r1, r2) {
  sprintf("AVERAGE(%s$%d:%s$%d)", cl, r1, cl, r2)
}

.wf <- function(wb, sn, formula, row, col) {
  writeFormula(wb, sn, formula, startRow = row, startCol = col)
}

# absolute-row formula builders. `er` is the data end row (from .data_end_row).
# Gap-immune, unlike the COUNT-relative .fml_* helpers: they address rows
# directly, so they are unaffected by metadata rows, internal gaps, or
# columns whose data starts late.
.af_last   <- function(cl, er)         sprintf("%s$%d", cl, er)
.af_change <- function(cl, er, n)      sprintf("%s$%d-%s$%d", cl, er, cl, er - n)
.af_chgfix <- function(cl, er, r)      sprintf("%s$%d-%s$%d", cl, er, cl, r)
.af_avg    <- function(cl, er, n = 3)  sprintf("AVERAGE(%s$%d:%s$%d)", cl, er - n + 1, cl, er)
.af_chgavg <- function(cl, er, n, lag) sprintf("%s-%s", .af_avg(cl, er, n), .af_avg(cl, er - lag, n))
.af_pctavg <- function(cl, er, n, lag) sprintf("IFERROR(%s/%s-1,\"\")", .af_avg(cl, er, n), .af_avg(cl, er - lag, n))


.write_source_sheet <- function(wb, sheet_name, source_path, source_sheet,
                                tab_colour = "#2F5496", start_row = 1,
                                date_col = NULL, date_fmt_str = "MMM-YY",
                                title = NULL) {
  # write the ONS sheet verbatim — trimming header rows here offsets the
  # output relative to the reference workbook, which keeps them
  tbl <- .safe_read(source_path, source_sheet)
  if (nrow(tbl) == 0) return(invisible(NULL))
  
  addWorksheet(wb, sheet_name, tabColour = tab_colour)

  # the reference workbook copies ONS sheets verbatim with no inserted
  # title row, so `title` is intentionally ignored here

  if (!is.null(date_col) && date_col <= ncol(tbl)) {
    tbl[[date_col]] <- .detect_dates(tbl[[date_col]])
  }
  
  writeData(wb, sheet_name, tbl, colNames = FALSE, startRow = start_row)
  .restore_text(wb, sheet_name, tbl, start_row)
  
  if (!is.null(date_col) && date_col <= ncol(tbl)) {
    date_rows <- which(!is.na(tbl[[date_col]])) + start_row - 1
    if (length(date_rows) > 0) {
      addStyle(wb, sheet_name, createStyle(numFmt = date_fmt_str),
               rows = date_rows, cols = date_col, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  if (nrow(tbl) > 0) {
    addStyle(wb, sheet_name, .data_font(),
             rows = start_row:(start_row + nrow(tbl) - 1),
             cols = 1:ncol(tbl), gridExpand = TRUE, stack = TRUE)
  }
  
  for (ci in seq_len(ncol(tbl))) {
    max_width <- max(nchar(as.character(tbl[[ci]])), na.rm = TRUE)
    max_width <- min(max(max_width, 8), 25)
    setColWidths(wb, sheet_name, cols = ci, widths = max_width + 2)
  }
  
  freezePane(wb, sheet_name, firstActiveRow = start_row)
  
  invisible(tbl)
}

.write_cmp_rows <- function(wb, sheet_name, labels, values_matrix, start_row = 1,
                            col_offset = 1) {
  for (i in seq_along(labels)) {
    r <- start_row + i - 1
    writeData(wb, sheet_name, labels[i], startRow = r, startCol = 1)
    addStyle(wb, sheet_name, .cmp_label(), rows = r, cols = 1, stack = TRUE)
    if (!is.null(values_matrix) && length(values_matrix) >= i) {
      vals <- values_matrix[[i]]
      if (!is.null(vals) && length(vals) > 0) {
        for (j in seq_along(vals)) {
          writeData(wb, sheet_name, vals[j], startRow = r, startCol = col_offset + j)
        }
      }
    }
  }
  last_row <- start_row + length(labels) - 1
  n_cols <- if (!is.null(values_matrix) && length(values_matrix) > 0) {
    col_offset + max(sapply(values_matrix, length))
  } else { 5 }
  addStyle(wb, sheet_name, .cmp_sep(), rows = last_row, cols = 1:n_cols,
           gridExpand = TRUE, stack = TRUE)
}

# ons files have inconsistent sheet names, try common variants
.detect_sheet <- function(path, candidates) {
  if (is.null(path)) return(NULL)
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (length(sheets) == 0) return(NULL)
  for (cc in candidates) {
    if (cc %in% sheets) return(cc)
  }
  for (cc in candidates) {
    idx <- grep(tolower(cc), tolower(sheets), fixed = TRUE)
    if (length(idx) > 0) return(sheets[idx[1]])
  }
  for (s in sheets) {
    tbl <- tryCatch(
      suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE, n_max = 5)),
      error = function(e) data.frame()
    )
    if (nrow(tbl) > 0 && ncol(tbl) > 1) return(s)
  }
  sheets[1]
}



create_audit_workbook <- function(
    output_path,
    file_a01 = NULL, file_hr1 = NULL, file_x09 = NULL, file_rtisa = NULL,
    file_cla01 = NULL, file_x02 = NULL,
    file_oecd_unemp = NULL, file_oecd_emp = NULL, file_oecd_inact = NULL,
    calculations_path = NULL, config_path = NULL,
    vacancies_mode = "aligned", payroll_mode = "aligned",
    manual_month_override = NULL, verbose = FALSE
) {
  
  wb <- createWorkbook()
  
  .ws <- function(src, src_sheet, tgt_sheet, tab_col = "#2F5496", date_col = NULL, title = NULL) {
    if (is.null(src)) return()
    .write_source_sheet(wb, tgt_sheet, src, src_sheet, tab_colour = tab_col, date_col = date_col, title = title)
  }
  
  .ws(file_a01, "22", "22", "#843C0C", date_col = 1, title = "Summary of regional labour market statistics")
  
  .ws(file_rtisa, "6. Employee flows (UK)", "RTI. Employee flows (UK)", "#548235", date_col = 1)
  
  # 1a gets comparison rows later, so skip it here
  hr1_titles <- list("1b" = "HR1 - Redundancy notifications by region",
                     "2a" = "HR1 - Redundancy notifications by industry",
                     "2b" = "HR1 - Redundancy notifications by industry (cont.)")
  for (s in c("1b", "2a", "2b")) .ws(file_hr1, s, s, "#C00000", date_col = 1, title = hr1_titles[[s]])
  
  cla_sheet <- .detect_sheet(file_cla01, c("1", "People SA", "People", "People_SA", "Sheet1", "CLA01"))
  if (!is.null(cla_sheet)) .ws(file_cla01, cla_sheet, "1 UK", "#2F5496")
  
  x02_sheet <- .detect_sheet(file_x02, c("LFS Labour market flows SA", "People SA", "1"))
  if (!is.null(x02_sheet)) .ws(file_x02, x02_sheet, "LFS Labour market flows SA", "#2F5496")
  
  for (oecd_info in list(
    list(file = file_oecd_unemp, name = "Unemployment"),
    list(file = file_oecd_emp,   name = "Employment"),
    list(file = file_oecd_inact, name = "Inactivity")
  )) {
    if (!is.null(oecd_info$file)) {
      # auto-fetched data arrives as a pre-parsed data.frame (country/period/value)
      if (is.data.frame(oecd_info$file)) {
        addWorksheet(wb, oecd_info$name, tabColour = "#2F5496")
        writeData(wb, oecd_info$name, oecd_info$file, headerStyle = .hs())
        next
      }
      ext <- tolower(tools::file_ext(oecd_info$file))
      if (ext == "csv") {
        # oecd csvs come in sdmx long format, need to pivot to wide
        csv_data <- tryCatch(read.csv(oecd_info$file, stringsAsFactors = FALSE),
                             error = function(e) data.frame())
        if (nrow(csv_data) > 0) {
          has_ref   <- any(c("REF_AREA", "Reference.area") %in% names(csv_data))
          has_time  <- any(c("TIME_PERIOD", "Time.period") %in% names(csv_data))
          has_value <- any(c("OBS_VALUE", "Observation.value") %in% names(csv_data))
          if (has_ref && has_time && has_value) {
            ref_col  <- intersect(c("Reference.area", "REF_AREA"), names(csv_data))[1]
            time_col <- intersect(c("Time.period", "TIME_PERIOD"), names(csv_data))[1]
            val_col  <- intersect(c("OBS_VALUE", "Observation.value"), names(csv_data))[1]
            # pivot: rows = country, cols = period
            countries <- unique(csv_data[[ref_col]])
            periods   <- sort(unique(csv_data[[time_col]]))
            wide_df <- data.frame(Country = countries, stringsAsFactors = FALSE)
            for (tp in periods) {
              tp_data <- csv_data[csv_data[[time_col]] == tp, ]
              wide_df[[tp]] <- tp_data[[val_col]][match(wide_df$Country, tp_data[[ref_col]])]
            }
            csv_data <- wide_df
          }
          addWorksheet(wb, oecd_info$name, tabColour = "#2F5496")
          writeData(wb, oecd_info$name, csv_data, headerStyle = .hs())
        }
      } else {
        oecd_sh <- .detect_sheet(oecd_info$file, c("Table", oecd_info$name, "Sheet1", "Data"))
        if (!is.null(oecd_sh)) .ws(oecd_info$file, oecd_sh, oecd_info$name, "#2F5496")
      }
    }
  }
  
  
  
  
  anchor_m <- NULL
  if (!is.null(file_a01)) {
    tbl1 <- .safe_read(file_a01, "1")
    if (nrow(tbl1) > 0) {
      col1 <- trimws(as.character(tbl1[[1]]))
      lfs_pat <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+(\\d{4})$"
      hits <- grep(lfs_pat, col1, ignore.case = TRUE)
      if (length(hits) > 0) {
        last_label <- col1[hits[length(hits)]]
        parts <- regmatches(last_label, regexec(lfs_pat, last_label, ignore.case = TRUE))[[1]]
        end_mon <- match(tools::toTitleCase(tolower(parts[3])), month.abb)
        end_yr  <- as.integer(parts[4])
        if (!is.na(end_mon) && !is.na(end_yr))
          anchor_m <- as.Date(sprintf("%04d-%02d-01", end_yr, end_mon))
      }
    }
  }
  if (is.null(anchor_m)) anchor_m <- Sys.Date() %m-% months(2)
  
  cm <- anchor_m %m+% months(2)
  lab_cur   <- .lfs_label(anchor_m)
  lab_q     <- .lfs_label(anchor_m %m-% months(3))
  lab_y     <- .lfs_label(anchor_m %m-% months(12))
  lab_covid <- .lfs_label(COVID_DATE)
  lab_elec  <- .lfs_label(ELEC24_DATE)
  all_labels <- c(lab_cur, lab_q, lab_y, lab_covid, lab_elec)
  ref_label <- format(cm, "%B %Y")
  
  tbl_1 <- .safe_read(file_a01, "1")
  tbl_2 <- .safe_read(file_a01, "2")
  tbl_19 <- .safe_read(file_a01, "19")
  
  na_m <- list(cur = NA, dq = NA, dy = NA, dc = NA, de = NA)
  
  if (nrow(tbl_1) > 0) {
    m_emp16   <- .lfs_metric(tbl_1, 4,  all_labels)
    m_emprt   <- .lfs_metric(tbl_1, 17, all_labels)
    m_unemp16 <- .lfs_metric(tbl_1, 5,  all_labels)
    m_unemprt <- .lfs_metric(tbl_1, 9,  all_labels)
    m_inact   <- .lfs_metric(tbl_1, 15, all_labels)
    m_inactrt <- .lfs_metric(tbl_1, 19, all_labels)
  } else {
    m_emp16 <- m_emprt <- m_unemp16 <- m_unemprt <- m_inact <- m_inactrt <- na_m
  }
  
  if (nrow(tbl_2) > 0 && ncol(tbl_2) >= 57) {
    m_5064 <- .lfs_metric(tbl_2, 56, all_labels)
    m_5064rt <- .lfs_metric(tbl_2, 57, all_labels)
  } else {
    m_5064 <- m_5064rt <- na_m
  }
  
  vac_m <- na_m
  if (nrow(tbl_19) > 0 && ncol(tbl_19) >= 3) {
    vac_m$cur <- .cell_num(tbl_19, .find_row(tbl_19, lab_cur), 3)
    vac_m$dq  <- vac_m$cur - .cell_num(tbl_19, .find_row(tbl_19, .lfs_label(anchor_m %m-% months(3))), 3)
    vac_m$dy  <- vac_m$cur - .cell_num(tbl_19, .find_row(tbl_19, .lfs_label(anchor_m %m-% months(12))), 3)
    vac_m$dc  <- vac_m$cur - .cell_num(tbl_19, .find_row(tbl_19, "Jan-Mar 2020"), 3)
    vac_m$de  <- vac_m$cur - .cell_num(tbl_19, .find_row(tbl_19, .lfs_label(ELEC24_DATE)), 3)
  }
  
  pay_m <- na_m
  rtisa_pay <- .safe_read(file_rtisa, "1. Payrolled employees (UK)")
  if (nrow(rtisa_pay) > 0 && ncol(rtisa_pay) >= 2) {
    rtisa_text <- trimws(as.character(rtisa_pay[[1]]))
    rtisa_parsed <- suppressWarnings(lubridate::parse_date_time(rtisa_text, orders = c("B Y", "bY", "BY")))
    rtisa_months <- floor_date(as.Date(rtisa_parsed), "month")
    rtisa_vals <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_pay[[2]]))))
    pay_df <- data.frame(m = rtisa_months, v = rtisa_vals, stringsAsFactors = FALSE)
    pay_df <- pay_df[!is.na(pay_df$m) & !is.na(pay_df$v), ]
    pay_df <- pay_df[order(pay_df$m), ]
    
    if (nrow(pay_df) > 0) {
      mc <- c(cm %m-% months(4), cm %m-% months(3), cm %m-% months(2))
      mp <- c(cm %m-% months(7), cm %m-% months(6), cm %m-% months(5))
      my <- mc %m-% months(12)
      pc <- .avg_by_dates(pay_df$m, pay_df$v, mc)
      pp <- .avg_by_dates(pay_df$m, pay_df$v, mp)
      py <- .avg_by_dates(pay_df$m, pay_df$v, my)
      pcov  <- .avg_by_dates(pay_df$m, pay_df$v, seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3))
      pelec <- .avg_by_dates(pay_df$m, pay_df$v, seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3))
      pay_m$cur <- if (!is.na(pc)) pc / 1000 else NA
      pay_m$dq  <- if (!is.na(pc) && !is.na(pp)) (pc - pp) / 1000 else NA
      pay_m$dy  <- if (!is.na(pc) && !is.na(py)) (pc - py) / 1000 else NA
      pay_m$dc  <- if (!is.na(pc) && !is.na(pcov)) (pc - pcov) / 1000 else NA
      pay_m$de  <- if (!is.na(pc) && !is.na(pelec)) (pc - pelec) / 1000 else NA
    }
  }
  
  wages_m <- na_m
  tbl_13 <- .trim_source(.safe_read(file_a01, "13"))
  if (nrow(tbl_13) > 0 && ncol(tbl_13) >= 4) {
    w13_dates <- .detect_dates(tbl_13[[1]])
    w13_pct <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[4]]))))
    w13_weekly <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[2]]))))
    wages_m$cur <- .val_by_date(w13_dates, w13_pct, anchor_m)
    win3 <- c(anchor_m, anchor_m %m-% months(1), anchor_m %m-% months(2))
    .wc <- function(a, b) {
      va <- .avg_by_dates(w13_dates, w13_weekly, a); vb <- .avg_by_dates(w13_dates, w13_weekly, b)
      if (is.na(va) || is.na(vb)) NA else (va - vb) * 52
    }
    wages_m$dq <- .wc(win3, c(anchor_m %m-% months(3), anchor_m %m-% months(4), anchor_m %m-% months(5)))
    wages_m$dy <- .wc(win3, win3 %m-% months(12))
    wages_m$dc <- .wc(win3, seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3))
    wages_m$de <- .wc(win3, seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3))
  }
  
  wages_cpi_m <- na_m
  tbl_cpi <- .safe_read(file_x09, "AWE Real_CPI")
  if (nrow(tbl_cpi) > 0 && ncol(tbl_cpi) >= 9) {
    cpi_months <- .detect_dates(tbl_cpi[[1]])
    cpi_real <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_cpi[[2]]))))
    cpi_total <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_cpi[[5]]))))
    cpi_valid <- which(!is.na(cpi_months) & !is.na(cpi_total))
    ca <- if (length(cpi_valid) > 0) cpi_months[cpi_valid[length(cpi_valid)]] else anchor_m
    wages_cpi_m$cur <- .val_by_date(cpi_months, cpi_total, ca)
    cw <- c(ca, ca %m-% months(1), ca %m-% months(2))
    .cc <- function(a, b) {
      va <- .avg_by_dates(cpi_months, cpi_real, a); vb <- .avg_by_dates(cpi_months, cpi_real, b)
      if (is.na(va) || is.na(vb)) NA else (va - vb) * 52
    }
    wages_cpi_m$dq <- .cc(cw, c(ca %m-% months(3), ca %m-% months(4), ca %m-% months(5)))
    wages_cpi_m$dy <- .cc(cw, cw %m-% months(12))
    wages_cpi_m$dc <- .cc(cw, seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3))
    wages_cpi_m$de <- .cc(cw, seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3))
  }
  
  
  # --- rtisa: payrolled employees ---
  if (!is.null(file_rtisa)) {
    tbl_rtisa <- .safe_read(file_rtisa, "1. Payrolled employees (UK)")
    if (nrow(tbl_rtisa) > 0) {
      sn <- "1. Payrolled employees (UK)"
      addWorksheet(wb, sn, tabColour = "#548235")
      
      writeData(wb, sn, tbl_rtisa, colNames = FALSE, startRow = 5)
      .restore_text(wb, sn, tbl_rtisa, 5)
      addStyle(wb, sn, .data_font(), rows = 5:(5 + nrow(tbl_rtisa)),
               cols = 1:ncol(tbl_rtisa), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 18)
      setColWidths(wb, sn, cols = 2:max(ncol(tbl_rtisa), 10), widths = 18)
      
      rtisa_text <- trimws(as.character(tbl_rtisa[[1]]))
      rtisa_date_rows <- which(grepl("^(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}$", rtisa_text, ignore.case = TRUE))
      dsr <- if (length(rtisa_date_rows) > 0) rtisa_date_rows[1] + 4 else 11  # output row
      
      covid_r <- .find_output_row(tbl_rtisa, 1, "December 2019", 5)
      # 2024 election baseline = the Apr-Jun 2024 quarter (elec_r:elec_r+2);
      # coming-into-office baseline = the single month June 2024
      elec_r   <- .find_output_row(tbl_rtisa, 1, "April 2024", 5)
      office_r <- .find_output_row(tbl_rtisa, 1, "June 2024", 5)
      cl <- "$B"
      
      hdrs <- c("Current", "Change on month (singular)", "Change on quarter",
                "Change year on year (3 month average)", "Change year on year (singular)",
                "Change since Covid-19", "Change since 2024 election",
                "Change since coming into office", "Change since start of the year", "Max")
      for (ci in seq_along(hdrs)) {
        writeData(wb, sn, hdrs[ci], startRow = 1, startCol = ci + 1)
        addStyle(wb, sn, .hs(), rows = 1, cols = ci + 1, stack = TRUE)
      }

      # row 2: number formulas, row 3: % formulas
      writeData(wb, sn, "Number", startRow = 2, startCol = 1)
      addStyle(wb, sn, .cmp_label(), rows = 2, cols = 1, stack = TRUE)
      .wf(wb, sn, .fml_avg_last(cl, dsr), 2, 2)                                    # Current
      .wf(wb, sn, sprintf("%s-%s", .fml_last(cl, dsr), .fml_last(cl, dsr, -1)), 2, 3) # Month change
      .wf(wb, sn, .fml_change_avg(cl, dsr, -5), 2, 4)                               # Quarter change
      .wf(wb, sn, .fml_change_avg(cl, dsr, -14), 2, 5)                              # YoY change (3 month average)
      .wf(wb, sn, .fml_idx_change(cl, dsr, 12), 2, 6)                               # YoY change (singular month)
      if (!is.na(covid_r)) .wf(wb, sn, .fml_change_fixed(cl, dsr, covid_r, covid_r + 2), 2, 7) # Covid
      if (!is.na(elec_r)) .wf(wb, sn, .fml_change_fixed(cl, dsr, elec_r, elec_r + 2), 2, 8)   # 2024 election
      if (!is.na(office_r)) .wf(wb, sn, .fml_change_single(cl, dsr, office_r), 2, 9)           # Coming into office
      .wf(wb, sn, sprintf('%s-_xlfn.XLOOKUP("January "&TEXT(TODAY(),"yyyy"),A$%d:A$1048576,B$%d:B$1048576)', .fml_last("B", dsr), dsr, dsr), 2, 10)
      .wf(wb, sn, .fml_max(cl, dsr), 2, 11)                                          # Max

      writeData(wb, sn, "%", startRow = 3, startCol = 1)
      addStyle(wb, sn, .cmp_label(), rows = 3, cols = 1, stack = TRUE)
      .wf(wb, sn, sprintf("IFERROR(%s/%s-1,\"\")", .fml_last(cl, dsr), .fml_last(cl, dsr, -1)), 3, 3) # Month %
      .wf(wb, sn, .fml_pct_avg(cl, dsr, -5), 3, 4)                                      # Quarter %
      .wf(wb, sn, .fml_pct_avg(cl, dsr, -14), 3, 5)                                     # YoY % (3 month average)
      .wf(wb, sn, sprintf("IFERROR(%s/%s-1,\"\")", .fml_last(cl, dsr), .fml_last(cl, dsr, -12)), 3, 6) # YoY % (singular)
      if (!is.na(covid_r)) .wf(wb, sn, sprintf("IFERROR(%s/AVERAGE(%s$%d:%s$%d)-1,\"\")", .fml_last(cl, dsr), cl, covid_r, cl, covid_r + 2), 3, 7)
      if (!is.na(elec_r)) .wf(wb, sn, .fml_pct_fixed(cl, dsr, elec_r, elec_r + 2), 3, 8)
      if (!is.na(office_r)) .wf(wb, sn, sprintf("IFERROR(%s/%s$%d-1,\"\")", .fml_last(cl, dsr), cl, office_r), 3, 9)
      .wf(wb, sn, sprintf('IFERROR(%s/_xlfn.XLOOKUP("January "&TEXT(TODAY(),"yyyy"),A$%d:A$1048576,B$%d:B$1048576)-1,"")', .fml_last("B", dsr), dsr, dsr), 3, 10)

      addStyle(wb, sn, .num_fmt(), rows = 2, cols = 2:11, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .pct_fmt(), rows = 3, cols = 3:10, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 3, cols = 1:11, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # --- rtisa: 23. employees industry ---
  if (!is.null(file_rtisa)) {
    tbl_23 <- .safe_read(file_rtisa, "23. Employees (Industry)")
    if (nrow(tbl_23) > 0 && ncol(tbl_23) >= 2) {
      sn <- "23. Employees Industry"
      addWorksheet(wb, sn, tabColour = "#548235")
      
      writeData(wb, sn, tbl_23, colNames = FALSE, startRow = 5)
      .restore_text(wb, sn, tbl_23, 5)
      addStyle(wb, sn, .data_font(), rows = 5:(5 + nrow(tbl_23)),
               cols = 1:ncol(tbl_23), gridExpand = TRUE, stack = TRUE)
      
      ind_dates <- .detect_dates(tbl_23[[1]])
      ind_valid <- which(!is.na(ind_dates))
      max_ci <- min(ncol(tbl_23), 21)  # up to col u (20 industries)
      
      if (length(ind_valid) > 0) {
        dsr <- ind_valid[1] + 4  # output row where data starts (offset by row 5 base)
        
        writeData(wb, sn, "Yearly change", startRow = 1, startCol = 1)
        addStyle(wb, sn, .hs(), rows = 1, cols = 1, stack = TRUE)
        # header row is right before data starts
        hdr_src <- ind_valid[1] - 1
        for (ci in 2:max_ci) {
          header_val <- as.character(tbl_23[[ci]][hdr_src])
          if (!is.na(header_val) && nchar(header_val) > 0) {
            writeData(wb, sn, header_val, startRow = 1, startCol = ci)
            addStyle(wb, sn, .hs(), rows = 1, cols = ci, stack = TRUE)
          }
        }
        
        # row 2: number (yearly change), row 3: % change, row 4: rank
        writeData(wb, sn, "Number", startRow = 2, startCol = 1)
        writeData(wb, sn, "%", startRow = 3, startCol = 1)
        writeData(wb, sn, "Rank", startRow = 4, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = 2:4, cols = 1, gridExpand = TRUE, stack = TRUE)
        
        for (ci in 2:max_ci) {
          cl <- .col_letter(ci)
          .wf(wb, sn, .fml_idx_change(cl, dsr, 12), 2, ci)
          .wf(wb, sn, .fml_idx_pct(cl, dsr, 12), 3, ci)
        }
        last_cl <- .col_letter(max_ci)
        for (ci in 2:max_ci) {
          cl <- .col_letter(ci)
          .wf(wb, sn, sprintf("RANK(%s2,$B$2:$%s$2)", cl, last_cl), 4, ci)
        }
        
        addStyle(wb, sn, .num_fmt(), rows = 2, cols = 2:max_ci, gridExpand = TRUE, stack = TRUE)
        addStyle(wb, sn, .pct_fmt(), rows = 3, cols = 2:max_ci, gridExpand = TRUE, stack = TRUE)
        addStyle(wb, sn, .cmp_sep(), rows = 4, cols = 1:max_ci, gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  
  # --- a01 sheet "2": age breakdown with comparisons ---
  if (!is.null(file_a01)) {
    tbl_2_full <- .trim_source(.safe_read(file_a01, "2"))
    if (nrow(tbl_2_full) > 0 && ncol(tbl_2_full) >= 10) {
      sn <- "2"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      s2_ncol <- ncol(tbl_2_full)
      
      writeData(wb, sn, "LFS: Employment, unemployment and inactivity by age group", startRow = 1, startCol = 1)
      addStyle(wb, sn, .ts(), rows = 1, cols = 1)
      setRowHeights(wb, sn, rows = 1, heights = 28)
      
      # 8 columns per age group (4 measures x level/rate)
      age_groups_2 <- list(
        list(sc = 2,  name = "Aged 16 and over"),
        list(sc = 10, name = "Aged 16-64"),
        list(sc = 18, name = "Aged 16-17"),
        list(sc = 26, name = "Aged 18-24"),
        list(sc = 34, name = "Aged 25-34"),
        list(sc = 42, name = "Aged 35-49"),
        list(sc = 50, name = "Aged 50-64")
      )
      for (ag in age_groups_2) {
        if (ag$sc <= s2_ncol) {
          writeData(wb, sn, ag$name, startRow = 2, startCol = ag$sc)
        }
      }
      
      # row 3: category headers per group
      categories_2 <- c("Employment", "Unemployment", "Activity", "Inactivity")
      for (ag in age_groups_2) {
        for (j in seq_along(categories_2)) {
          cc <- ag$sc + (j - 1) * 2
          if (cc <= s2_ncol) {
            writeData(wb, sn, categories_2[j], startRow = 3, startCol = cc)
          }
        }
      }
      
      # row 4: level/rate subheaders
      for (ci in seq(2, s2_ncol, by = 2)) {
        writeData(wb, sn, "level", startRow = 4, startCol = ci)
        if (ci + 1 <= s2_ncol) writeData(wb, sn, "rate (%)", startRow = 4, startCol = ci + 1)
      }
      addStyle(wb, sn, .hs(), rows = 2:4, cols = 2:s2_ncol, gridExpand = TRUE, stack = TRUE)
      
      # rows 5-10: comparison formulas matching feb file pattern
      s2_dsr  <- 11                          # write row for the ONS table
      s2_data <- .data_row(tbl_2_full, s2_dsr)  # row where real data begins
      cmp_labels <- c("Current", "Quarterly change", "Change year on year",
                      "Change since Covid (Dec 19-Feb 20)",
                      "Change since 2010 election", "Change since 2024 election")
      
      s2_covid_row  <- .find_output_row(tbl_2_full, 1, lab_covid,       s2_dsr)
      s2_elec10_row <- .find_output_row(tbl_2_full, 1, "Feb-Apr 2010",  s2_dsr)
      s2_elec24_row <- .find_output_row(tbl_2_full, 1, lab_elec,        s2_dsr)
      
      for (i in seq_along(cmp_labels)) {
        r <- 4 + i
        writeData(wb, sn, cmp_labels[i], startRow = r, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = r, cols = 1, stack = TRUE)
        for (j in 2:s2_ncol) {
          cl <- .col_letter(j)
          fml <- switch(as.character(i),
                        "1" = .fml_last(cl, s2_data),
                        "2" = .fml_idx_change(cl, s2_data, 3),
                        "3" = .fml_idx_change(cl, s2_data, 12),
                        "4" = if (!is.na(s2_covid_row))  .fml_idx_change_fixed(cl, s2_data, s2_covid_row)  else NULL,
                        "5" = if (!is.na(s2_elec10_row)) .fml_idx_change_fixed(cl, s2_data, s2_elec10_row) else NULL,
                        "6" = if (!is.na(s2_elec24_row)) .fml_idx_change_fixed(cl, s2_data, s2_elec24_row) else NULL
          )
          if (!is.null(fml)) .wf(wb, sn, fml, r, j)
        }
      }
      addStyle(wb, sn, .cmp_sep(), rows = 10, cols = 1:s2_ncol, gridExpand = TRUE, stack = TRUE)
      # teal background across full width of comparison rows
      addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 5:10, cols = 1:s2_ncol, gridExpand = TRUE, stack = TRUE)
      setRowHeights(wb, sn, rows = 2:10, heights = 20)
      
      writeData(wb, sn, tbl_2_full, colNames = FALSE, startRow = s2_dsr)
      .restore_text(wb, sn, tbl_2_full, s2_dsr)
      addStyle(wb, sn, .data_font(), rows = s2_dsr:(s2_dsr + nrow(tbl_2_full)),
               cols = 1:ncol(tbl_2_full), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      freezePane(wb, sn, firstActiveRow = s2_dsr, firstActiveCol = 2)
    }
  }
  
  # --- a01 sheet "3": direct copy ---
  if (!is.null(file_a01)) {
    .ws(file_a01, "3", "3", "#2F5496", date_col = 1)
  }
  
  # --- a01 sheet "5": workforce jobs ---
  if (!is.null(file_a01)) {
    tbl_5 <- .trim_source(.safe_read(file_a01, "5"))
    if (nrow(tbl_5) > 0) {
      sn <- "5"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      writeData(wb, sn, "Workforce Jobs", startRow = 1, startCol = 1)
      addStyle(wb, sn, .ts(), rows = 1, cols = 1)
      setRowHeights(wb, sn, rows = 1, heights = 28)
      
      # need data written first so formulas can reference it
      writeData(wb, sn, tbl_5, colNames = FALSE, startRow = 7)
      .restore_text(wb, sn, tbl_5, 7)
      addStyle(wb, sn, .data_font(), rows = 7:(7 + nrow(tbl_5)),
               cols = 1:ncol(tbl_5), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      setColWidths(wb, sn, cols = 2:6, widths = 18)
      
      # row 2 headers
      col_hdrs <- c("Workforce jobs", "Employee jobs", "Self-employment jobs",
                    "HM Forces", "Government- supported trainees")
      for (ci in seq_along(col_hdrs)) {
        writeData(wb, sn, col_hdrs[ci], startRow = 2, startCol = ci + 1)
        addStyle(wb, sn, .hs(), rows = 2, cols = ci + 1, stack = TRUE)
      }
      
      # find first data row (pattern: "Mon YY")
      s5_col1 <- trimws(as.character(tbl_5[[1]]))
      s5_data_rows <- which(grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{2}", s5_col1, ignore.case = TRUE))
      s5_newgov_row <- grep("^Jun\\s+24", s5_col1, ignore.case = TRUE)
      
      cmp_labels_5 <- c("Current", "Quarterly change", "Max", "Jobs created since new gov")
      for (i in seq_along(cmp_labels_5)) {
        writeData(wb, sn, cmp_labels_5[i], startRow = i + 2, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = i + 2, cols = 1, stack = TRUE)
      }
      
      if (length(s5_data_rows) > 0) {
        dsr <- s5_data_rows[1] + 6  # output row (source starts at row 7)
        newgov_output_r <- if (length(s5_newgov_row) > 0) s5_newgov_row[1] + 6 else NA
        
        for (ci in 2:min(ncol(tbl_5), 6)) {
          cl <- .col_letter(ci)
          .wf(wb, sn, .fml_last(cl, dsr), 3, ci)              # current
          .wf(wb, sn, .fml_idx_change(cl, dsr, 1), 4, ci)    # quarterly change (workforce jobs is quarterly data)
          .wf(wb, sn, .fml_max(cl, dsr), 5, ci)              # max
          if (!is.na(newgov_output_r))
            .wf(wb, sn, .fml_idx_change_fixed(cl, dsr, newgov_output_r), 6, ci)
        }
        addStyle(wb, sn, .num_fmt(), rows = 3:6, cols = 2:6, gridExpand = TRUE, stack = TRUE)
      }
      addStyle(wb, sn, .cmp_sep(), rows = 6, cols = 1:6, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 3:6, cols = 1:6, gridExpand = TRUE, stack = TRUE)
      setRowHeights(wb, sn, rows = 2:6, heights = 20)
      freezePane(wb, sn, firstActiveRow = 7, firstActiveCol = 2)
    }
  }
  
  # --- a01 sheet "10": redundancy ---
  if (!is.null(file_a01)) {
    tbl_10_full <- .trim_source(.safe_read(file_a01, "10"))
    if (nrow(tbl_10_full) > 0 && ncol(tbl_10_full) >= 6) {
      sn <- "10"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      writeData(wb, sn, "Redundancies", startRow = 1, startCol = 1)
      addStyle(wb, sn, .ts(), rows = 1, cols = 1)
      
      for (pair in list(c(2, "People"), c(4, "Men"), c(6, "Women"))) {
        writeData(wb, sn, pair[2], startRow = 2, startCol = as.integer(pair[1]))
        addStyle(wb, sn, .hs(), rows = 2, cols = as.integer(pair[1]), stack = TRUE)
      }
      for (ci in c(2, 4, 6)) {
        writeData(wb, sn, "Level", startRow = 3, startCol = ci)
        writeData(wb, sn, "Rate per thousand", startRow = 3, startCol = ci + 1)
      }
      addStyle(wb, sn, .hs(), rows = 3, cols = 2:7, gridExpand = TRUE, stack = TRUE)
      
      cmp_labels_10 <- c("Current", "Quarterly change", "year on year change",
                         "Since pandemic", "Since 2010 election")
      s10_dsr <- 10  # write row for the ONS table
      # absolute data end row: the redundancy columns have gaps in the early
      # series, so COUNT-relative formulas undershoot
      s10_der <- .data_end_row(tbl_10_full, s10_dsr)

      s10_covid_row  <- .find_output_row(tbl_10_full, 1, lab_covid,      s10_dsr)
      s10_elec10_row <- .find_output_row(tbl_10_full, 1, "Feb-Apr 2010", s10_dsr)

      for (i in seq_along(cmp_labels_10)) {
        r <- 3 + i
        writeData(wb, sn, cmp_labels_10[i], startRow = r, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = r, cols = 1, stack = TRUE)
        for (ci in 2:min(ncol(tbl_10_full), 7)) {
          cl <- .col_letter(ci)
          fml <- switch(as.character(i),
                        "1" = .af_last(cl, s10_der),
                        "2" = .af_change(cl, s10_der, 3),
                        "3" = .af_change(cl, s10_der, 12),
                        "4" = if (!is.na(s10_covid_row))  .af_chgfix(cl, s10_der, s10_covid_row)  else NULL,
                        "5" = if (!is.na(s10_elec10_row)) .af_chgfix(cl, s10_der, s10_elec10_row) else NULL
          )
          if (!is.null(fml)) .wf(wb, sn, fml, r, ci)
        }
      }
      addStyle(wb, sn, .num_fmt(), rows = 4:8, cols = c(2, 4, 6), gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .pp_fmt(), rows = 4:8, cols = c(3, 5, 7), gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 8, cols = 1:7, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 4:8, cols = 1:7, gridExpand = TRUE, stack = TRUE)
      setRowHeights(wb, sn, rows = 1:8, heights = 20)
      
      writeData(wb, sn, tbl_10_full, colNames = FALSE, startRow = 10)
      .restore_text(wb, sn, tbl_10_full, 10)
      addStyle(wb, sn, .data_font(), rows = 10:(10 + nrow(tbl_10_full)),
               cols = 1:ncol(tbl_10_full), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      freezePane(wb, sn, firstActiveRow = s10_dsr, firstActiveCol = 2)
    }
  }
  
  # --- a01 sheet "11": inactivity by reason ---
  if (!is.null(file_a01)) {
    tbl_11 <- .trim_source(.safe_read(file_a01, "11"))
    if (nrow(tbl_11) > 0 && ncol(tbl_11) >= 6) {
      sn <- "11"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      max_col_11 <- min(ncol(tbl_11), 21)
      
      # row 1: group headers
      writeData(wb, sn, "Economic inactivity by reason (thousands)", startRow = 1, startCol = 3)
      addStyle(wb, sn, .hs(), rows = 1, cols = 3, stack = TRUE)
      if (max_col_11 >= 13) {
        writeData(wb, sn, "Percentage of economically inactive (%)", startRow = 1, startCol = 13)
        addStyle(wb, sn, .hs(), rows = 1, cols = 13, stack = TRUE)
      }
      
      # row 2: column sub-headers
      reason_hdrs_11 <- c(
        "Total economically inactive aged 16-64 (thousands)4",
        "Student", "Looking after family / home", "Temp sick", "Long-term sick",
        "Discouraged workers1", "Retired", "Other2",
        "Does not want job (thousands)", "Wants a job (thousands)",
        "Total economically inactive\naged 16-64",
        "Student", "Looking after family / home", "Temp sick", "Long-term sick",
        "Discouraged workers1", "Retired", "Other2",
        "Does not want job (%age of economically inactive)",
        "Wants a job (%age of economically inactive)"
      )
      for (ci in 2:max_col_11) {
        if (ci - 1 <= length(reason_hdrs_11)) {
          writeData(wb, sn, reason_hdrs_11[ci - 1], startRow = 2, startCol = ci)
          addStyle(wb, sn, .hs(), rows = 2, cols = ci, stack = TRUE)
        }
      }
      
      
      cmp_labels_11 <- c("Current", "Quarterly change", "year on year change",
                         "Since pandemic", "Since 2010 election")
      for (i in seq_along(cmp_labels_11)) {
        writeData(wb, sn, cmp_labels_11[i], startRow = 2 + i, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = 2 + i, cols = 1, stack = TRUE)
      }
      
      # Write source data at row 9
      writeData(wb, sn, tbl_11, colNames = FALSE, startRow = 9)
      .restore_text(wb, sn, tbl_11, 9)
      
      # off_11 = write row (9) minus 1, used by .fr11 baseline lookups. The
      # sub-category columns start their data several rows below the Total
      # column, so address by absolute end row, not COUNT-relative.
      off_11 <- 8
      der_11 <- .data_end_row(tbl_11, off_11 + 1)

      .fr11 <- function(label) {
        idx <- .find_row(tbl_11, label)
        if (!is.na(idx)) idx + off_11 else NA_integer_
      }
      pandemic_11 <- .fr11("Dec-Feb 2020")
      elec2010_11 <- .fr11("Feb-Apr 2010")

      for (ci in 2:max_col_11) {
        cl <- .col_letter(ci)
        .wf(wb, sn, .af_last(cl, der_11),       3, ci)  # Current
        .wf(wb, sn, .af_change(cl, der_11, 3),  4, ci)  # Quarterly change
        .wf(wb, sn, .af_change(cl, der_11, 12), 5, ci)  # Year on year change
        if (!is.na(pandemic_11))
          .wf(wb, sn, .af_chgfix(cl, der_11, pandemic_11), 6, ci)  # Since pandemic
        if (!is.na(elec2010_11))
          .wf(wb, sn, .af_chgfix(cl, der_11, elec2010_11), 7, ci)  # Since 2010 election
      }
      
      # Formatting
      addStyle(wb, sn, .num_fmt(), rows = 3:7, cols = 2:max_col_11,
               gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 7, cols = 1:max_col_11,
               gridExpand = TRUE, stack = TRUE)
      
      # Source data styling
      addStyle(wb, sn, .data_font(), rows = 9:(9 + nrow(tbl_11)),
               cols = 1:ncol(tbl_11), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      setColWidths(wb, sn, cols = 2:max_col_11, widths = 20)
      freezePane(wb, sn, firstActiveRow = 9, firstActiveCol = 2)
    }
  }
  
  # --- A01 Sheet "13": AWE Total Pay (nominal) with comparisons ---
  if (!is.null(file_a01) && exists("tbl_13") && nrow(tbl_13) > 0) {
    sn <- "13"
    addWorksheet(wb, sn, tabColour = "#2F5496")
    
    # Row 1: title
    writeData(wb, sn, "Average Weekly Earnings \u2014 Total Pay (Nominal)", startRow = 1, startCol = 1)
    addStyle(wb, sn, .ts(), rows = 1, cols = 1)
    setRowHeights(wb, sn, rows = 1, heights = 28)
    
    # 9 sector definitions matching reference workbook
    awe_sectors <- list(
      list(name = "Whole Economy", sc = 2),
      list(name = "Private sector", sc = 5),
      list(name = "Public sector", sc = 8),
      list(name = "Services (G-S)", sc = 11),
      list(name = "Finance & business (K-N)", sc = 14),
      list(name = "Public excl. financial services", sc = 17),
      list(name = "Manufacturing (C)", sc = 20),
      list(name = "Construction (F)", sc = 23),
      list(name = "Wholesale, retail, hotels & restaurants (G & I)", sc = 26)
    )
    max_col_13 <- 28
    
    # Row 2: Sector name headers
    for (sec in awe_sectors) {
      writeData(wb, sn, sec$name, startRow = 2, startCol = sec$sc)
      addStyle(wb, sn, .hs(), rows = 2, cols = sec$sc, stack = TRUE)
    }
    # Row 3: Weekly Earnings / % changes sub-headers
    for (sec in awe_sectors) {
      writeData(wb, sn, "Weekly Earnings (\u00a3)", startRow = 3, startCol = sec$sc)
      writeData(wb, sn, "% change year on year", startRow = 3, startCol = sec$sc + 1)
    }
    addStyle(wb, sn, .hs(), rows = 3, cols = 2:max_col_13, gridExpand = TRUE, stack = TRUE)
    # Row 4: Single month / 3 month average sub-headers
    for (sec in awe_sectors) {
      writeData(wb, sn, "Single month", startRow = 4, startCol = sec$sc + 1)
      writeData(wb, sn, "3 month avg", startRow = 4, startCol = sec$sc + 2)
    }
    addStyle(wb, sn, .hs(), rows = 4, cols = 2:max_col_13, gridExpand = TRUE, stack = TRUE)
    
    # Comparison row labels (rows 5-9)
    cmp_labels_13 <- c("Current (3mo avg)", "Change on quarter", "Change year on year",
                       "Change since Covid-19", "Change since 2024 election")
    for (i in seq_along(cmp_labels_13)) {
      writeData(wb, sn, cmp_labels_13[i], startRow = 3 + i, startCol = 1)
      addStyle(wb, sn, .cmp_label(), rows = 3 + i, cols = 1, stack = TRUE)
    }

    # Write source data at row 10
    tbl_13[[1]] <- .detect_dates(tbl_13[[1]])
    writeData(wb, sn, tbl_13, colNames = FALSE, startRow = 10)
    .restore_text(wb, sn, tbl_13, 10)

    # absolute data end row — the % columns start late (they need 12 months of
    # history), so COUNT-relative formulas misfire
    off_13 <- 9  # startRow(10) - 1, used by .dr13 baseline lookups
    der_13 <- .data_end_row(tbl_13, off_13 + 1)

    # Find baseline rows dynamically from dates
    dates_13 <- .detect_dates(tbl_13[[1]])
    .dr13 <- function(d) {
      idx <- which(dates_13 == as.Date(d))
      if (length(idx)) idx[1] + off_13 else NA_integer_
    }
    covid_r1 <- .dr13("2019-12-01"); covid_r3 <- .dr13("2020-02-01")
    elec_r1  <- .dr13("2024-04-01"); elec_r3  <- .dr13("2024-06-01")

    # Write formulas for all 9 sectors (comparison rows 4-8)
    for (sec in awe_sectors) {
      cw <- .col_letter(sec$sc)      # weekly £ col letter
      cs <- .col_letter(sec$sc + 1)  # single month % col letter
      c3 <- .col_letter(sec$sc + 2)  # 3mo avg % col letter

      # Row 4: Current — 3mo avg for weekly £, latest value for %
      .wf(wb, sn, .af_avg(cw, der_13, 3), 4, sec$sc)
      .wf(wb, sn, .af_last(cs, der_13),   4, sec$sc + 1)
      .wf(wb, sn, .af_last(c3, der_13),   4, sec$sc + 2)

      # Row 5: Change on quarter
      .wf(wb, sn, sprintf("(%s)*52", .af_chgavg(cw, der_13, 3, 3)), 5, sec$sc)
      .wf(wb, sn, .af_change(cs, der_13, 3), 5, sec$sc + 1)
      .wf(wb, sn, .af_change(c3, der_13, 3), 5, sec$sc + 2)

      # Row 6: Change year on year
      .wf(wb, sn, sprintf("(%s)*52", .af_chgavg(cw, der_13, 3, 12)), 6, sec$sc)
      .wf(wb, sn, .af_change(cs, der_13, 12), 6, sec$sc + 1)
      .wf(wb, sn, .af_change(c3, der_13, 12), 6, sec$sc + 2)

      # Row 7: Change since Covid-19 (vs the 2019-12..2020-02 window)
      if (!is.na(covid_r1) && !is.na(covid_r3)) {
        .wf(wb, sn, sprintf("(%s-AVERAGE(%s$%d:%s$%d))*52",
                            .af_avg(cw, der_13, 3), cw, covid_r1, cw, covid_r3), 7, sec$sc)
        .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                            .af_last(cs, der_13), cs, covid_r1, cs, covid_r3), 7, sec$sc + 1)
        .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                            .af_last(c3, der_13), c3, covid_r1, c3, covid_r3), 7, sec$sc + 2)
      }

      # Row 8: Change since 2024 election
      if (!is.na(elec_r1) && !is.na(elec_r3)) {
        .wf(wb, sn, sprintf("(%s-AVERAGE(%s$%d:%s$%d))*52",
                            .af_avg(cw, der_13, 3), cw, elec_r1, cw, elec_r3), 8, sec$sc)
        .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                            .af_last(cs, der_13), cs, elec_r1, cs, elec_r3), 8, sec$sc + 1)
        .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                            .af_last(c3, der_13), c3, elec_r1, c3, elec_r3), 8, sec$sc + 2)
      }
    }
    
    # Formatting
    w_cols_13 <- sapply(awe_sectors, function(s) s$sc)
    p_cols_13 <- unlist(lapply(awe_sectors, function(s) c(s$sc + 1, s$sc + 2)))
    addStyle(wb, sn, .gbp_fmt(), rows = 5:9, cols = w_cols_13, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sn, .pp_fmt(), rows = 5:9, cols = p_cols_13, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sn, .cmp_sep(), rows = 9, cols = 1:max_col_13, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 5:9, cols = 1:max_col_13, gridExpand = TRUE, stack = TRUE)
    setRowHeights(wb, sn, rows = 2:9, heights = 20)
    
    # Source data styling
    date_rows_13 <- which(!is.na(tbl_13[[1]])) + 9
    if (length(date_rows_13) > 0)
      addStyle(wb, sn, .date_fmt(), rows = date_rows_13, cols = 1, stack = TRUE)
    addStyle(wb, sn, .data_font(), rows = 10:(10 + nrow(tbl_13)),
             cols = 1:ncol(tbl_13), gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, sn, cols = 1, widths = 32)
    setColWidths(wb, sn, cols = 2:max_col_13, widths = 16)
    freezePane(wb, sn, firstActiveRow = 10, firstActiveCol = 2)
  }
  
  # --- A01 Sheet "15": AWE Regular Pay (nominal) with comparisons ---
  if (!is.null(file_a01)) {
    tbl_15_full <- .trim_source(.safe_read(file_a01, "15"))
    if (nrow(tbl_15_full) > 0) {
      sn <- "15"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      # Row 1: title
      writeData(wb, sn, "Average Weekly Earnings \u2014 Regular Pay (Nominal)", startRow = 1, startCol = 1)
      addStyle(wb, sn, .ts(), rows = 1, cols = 1)
      setRowHeights(wb, sn, rows = 1, heights = 28)
      
      # 9 sector definitions
      awe_sectors_15 <- list(
        list(name = "Whole Economy", sc = 2),
        list(name = "Private sector", sc = 5),
        list(name = "Public sector", sc = 8),
        list(name = "Services (G-S)", sc = 11),
        list(name = "Finance & business (K-N)", sc = 14),
        list(name = "Public excl. financial services", sc = 17),
        list(name = "Manufacturing (C)", sc = 20),
        list(name = "Construction (F)", sc = 23),
        list(name = "Wholesale, retail, hotels & restaurants (G & I)", sc = 26)
      )
      max_col_15 <- 28
      
      # Row 2: Sector headers
      for (sec in awe_sectors_15) {
        writeData(wb, sn, sec$name, startRow = 2, startCol = sec$sc)
        addStyle(wb, sn, .hs(), rows = 2, cols = sec$sc, stack = TRUE)
      }
      # Row 3: Sub-headers
      for (sec in awe_sectors_15) {
        writeData(wb, sn, "Weekly Earnings (\u00a3)", startRow = 3, startCol = sec$sc)
        writeData(wb, sn, "% change year on year", startRow = 3, startCol = sec$sc + 1)
      }
      addStyle(wb, sn, .hs(), rows = 3, cols = 2:max_col_15, gridExpand = TRUE, stack = TRUE)
      # Row 4: Single month / 3 month average
      for (sec in awe_sectors_15) {
        writeData(wb, sn, "Single month", startRow = 4, startCol = sec$sc + 1)
        writeData(wb, sn, "3 month avg", startRow = 4, startCol = sec$sc + 2)
      }
      addStyle(wb, sn, .hs(), rows = 4, cols = 2:max_col_15, gridExpand = TRUE, stack = TRUE)
      
      # Comparison row labels (rows 5-9)
      cmp_labels_15 <- c("Current", "Quarterly change", "Year on year change",
                         "Since pandemic", "Since 2010 election")
      for (i in seq_along(cmp_labels_15)) {
        writeData(wb, sn, cmp_labels_15[i], startRow = 4 + i, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = 4 + i, cols = 1, stack = TRUE)
      }
      
      # Write source data at row 10
      tbl_15_full[[1]] <- .detect_dates(tbl_15_full[[1]])
      writeData(wb, sn, tbl_15_full, colNames = FALSE, startRow = 10)
      .restore_text(wb, sn, tbl_15_full, 10)
      
      # absolute data end row — the % columns start late, so COUNT-relative
      # formulas misfire
      off_15 <- 9  # startRow(10) - 1, used by .dr15 baseline lookups
      der_15 <- .data_end_row(tbl_15_full, off_15 + 1)

      # Baseline rows: pre-pandemic = Feb 2020, 2010 election = Apr 2010
      # (the months the reference workbook compares against)
      dates_15 <- .detect_dates(tbl_15_full[[1]])
      .dr15 <- function(d) {
        idx <- which(dates_15 == as.Date(d))
        if (length(idx)) idx[1] + off_15 else NA_integer_
      }
      pandemic_r <- .dr15("2020-02-01")
      elec2010_r <- .dr15("2010-04-01")

      # Write formulas for all 9 sectors (comparison rows 5-9)
      for (sec in awe_sectors_15) {
        cw <- .col_letter(sec$sc)
        cs <- .col_letter(sec$sc + 1)
        c3 <- .col_letter(sec$sc + 2)

        # Row 5: Current
        .wf(wb, sn, .af_last(cw, der_15), 5, sec$sc)
        .wf(wb, sn, .af_last(cs, der_15), 5, sec$sc + 1)
        .wf(wb, sn, .af_last(c3, der_15), 5, sec$sc + 2)

        # Row 6: Quarterly change
        .wf(wb, sn, .af_change(cw, der_15, 3), 6, sec$sc)
        .wf(wb, sn, .af_change(cs, der_15, 3), 6, sec$sc + 1)
        .wf(wb, sn, .af_change(c3, der_15, 3), 6, sec$sc + 2)

        # Row 7: Year on year change
        .wf(wb, sn, .af_change(cw, der_15, 12), 7, sec$sc)
        .wf(wb, sn, .af_change(cs, der_15, 12), 7, sec$sc + 1)
        .wf(wb, sn, .af_change(c3, der_15, 12), 7, sec$sc + 2)

        # Row 8: Since pandemic
        if (!is.na(pandemic_r)) {
          .wf(wb, sn, .af_chgfix(cw, der_15, pandemic_r), 8, sec$sc)
          .wf(wb, sn, .af_chgfix(cs, der_15, pandemic_r), 8, sec$sc + 1)
          .wf(wb, sn, .af_chgfix(c3, der_15, pandemic_r), 8, sec$sc + 2)
        }

        # Row 9: Since 2010 election
        if (!is.na(elec2010_r)) {
          .wf(wb, sn, .af_chgfix(cw, der_15, elec2010_r), 9, sec$sc)
          .wf(wb, sn, .af_chgfix(cs, der_15, elec2010_r), 9, sec$sc + 1)
          .wf(wb, sn, .af_chgfix(c3, der_15, elec2010_r), 9, sec$sc + 2)
        }
      }
      
      # Formatting
      p_cols_15 <- unlist(lapply(awe_sectors_15, function(s) c(s$sc + 1, s$sc + 2)))
      addStyle(wb, sn, .pp_fmt(), rows = 5:9, cols = p_cols_15, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 9, cols = 1:max_col_15, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 5:9, cols = 1:max_col_15, gridExpand = TRUE, stack = TRUE)
      setRowHeights(wb, sn, rows = 2:9, heights = 20)
      
      # Source data styling
      date_rows_15 <- which(!is.na(tbl_15_full[[1]])) + 9
      if (length(date_rows_15) > 0)
        addStyle(wb, sn, .date_fmt(), rows = date_rows_15, cols = 1, stack = TRUE)
      addStyle(wb, sn, .data_font(), rows = 10:(10 + nrow(tbl_15_full)),
               cols = 1:ncol(tbl_15_full), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      setColWidths(wb, sn, cols = 2:max_col_15, widths = 16)
      freezePane(wb, sn, firstActiveRow = 10, firstActiveCol = 2)
    }
  }
  
  # --- A01 Sheet "18": Working days lost ---
  if (!is.null(file_a01)) {
    tbl_18 <- .trim_source(.safe_read(file_a01, "18"))
    if (nrow(tbl_18) > 0) {
      sn <- "18"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      # Title
      writeData(wb, sn, "Labour Disputes summary", startRow = 1, startCol = 1)
      addStyle(wb, sn, .ts(), rows = 1, cols = 1)
      
      # Headers matching reference (shifted to row 2 for title)
      col_hdrs_18 <- c("Working days lost (thousands) ",
                       "Number of stoppages 1,2",
                       "Workers involved (thousands) 3")
      for (ci in seq_along(col_hdrs_18)) {
        writeData(wb, sn, col_hdrs_18[ci], startRow = 2, startCol = ci + 1)
        addStyle(wb, sn, .hs(), rows = 2, cols = ci + 1, stack = TRUE)
      }
      
      # Comparison row labels (rows 3-8)
      cmp_labels_18 <- c("Current  (singular month)", "Change on quarter  (3mo avg)",
                         "Change since Covid-19 (2019 average)",
                         "Change since 2024 election (3mo avg)",
                         "2019 average", "2023 average")
      for (i in seq_along(cmp_labels_18)) {
        writeData(wb, sn, cmp_labels_18[i], startRow = i + 2, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = i + 2, cols = 1, stack = TRUE)
      }
      
      # Write source data at row 10
      writeData(wb, sn, tbl_18, colNames = FALSE, startRow = 10)
      .restore_text(wb, sn, tbl_18, 10)
      
      # Locate the last data row by date label. Column 2 has a real gap (the
      # covid-era collection pause), so COUNT-relative formulas undershoot —
      # the comparison rows below use absolute row references instead.
      off_18 <- 9  # write row (10) minus 1, used by .dr18 baseline lookups
      der_18 <- .data_end_row(tbl_18, off_18 + 1)
      
      # Find baseline rows dynamically from dates
      s18_dates <- .detect_dates(tbl_18[[1]])
      .dr18 <- function(d) {
        idx <- which(s18_dates == as.Date(d))
        if (length(idx)) idx[1] + off_18 else NA_integer_
      }
      # 2019 data: Jan-Dec 2019 (12 months)
      r2019_start <- .dr18("2019-01-01"); r2019_end <- .dr18("2019-12-01")
      # 2023 data: Jan-Dec 2023 (12 months)
      r2023_start <- .dr18("2023-01-01"); r2023_end <- .dr18("2023-12-01")
      # Election baseline: Apr-Jun 2024 (3 months)
      elec18_r1 <- .dr18("2024-04-01"); elec18_r3 <- .dr18("2024-06-01")
      
      # Write formulas for cols B(2), C(3), D(4)
      # Working days lost / stoppages / workers involved are absolute counts — no *52.
      # a3() = average of the latest 3 months, by absolute row.
      a3 <- function(cl, end) sprintf("AVERAGE(%s$%d:%s$%d)", cl, end - 2, cl, end)
      for (ci in 2:4) {
        cl <- .col_letter(ci)

        # Row 3: Current (singular month) — the last data row
        .wf(wb, sn, sprintf("%s$%d", cl, der_18), 3, ci)

        # Row 4: Change on quarter (3mo avg vs prior 3mo avg)
        .wf(wb, sn, sprintf("%s-%s", a3(cl, der_18), a3(cl, der_18 - 3)), 4, ci)

        # Row 5: Change since Covid-19 (vs 2019 annual average)
        if (!is.na(r2019_start) && !is.na(r2019_end)) {
          .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                              a3(cl, der_18), cl, r2019_start, cl, r2019_end), 5, ci)
        }

        # Row 6: Change since 2024 election (3mo avg)
        if (!is.na(elec18_r1) && !is.na(elec18_r3)) {
          .wf(wb, sn, sprintf("%s-AVERAGE(%s$%d:%s$%d)",
                              a3(cl, der_18), cl, elec18_r1, cl, elec18_r3), 6, ci)
        }
        
        # Row 7: 2019 average
        if (!is.na(r2019_start) && !is.na(r2019_end))
          .wf(wb, sn, .fml_avg_range(cl, r2019_start, r2019_end), 7, ci)
        
        # Row 8: 2023 average
        if (!is.na(r2023_start) && !is.na(r2023_end))
          .wf(wb, sn, .fml_avg_range(cl, r2023_start, r2023_end), 8, ci)
      }
      
      addStyle(wb, sn, .num_fmt(), rows = 3:8, cols = 2:4, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 8, cols = 1:4, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, createStyle(fgFill = "#EAF3FB"), rows = 3:8, cols = 1:4, gridExpand = TRUE, stack = TRUE)
      setRowHeights(wb, sn, rows = 1:8, heights = 20)
      
      # Source data styling
      addStyle(wb, sn, .data_font(), rows = 10:(10 + nrow(tbl_18)),
               cols = 1:ncol(tbl_18), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 40)
      setColWidths(wb, sn, cols = 2:4, widths = 20)
      freezePane(wb, sn, firstActiveRow = 10, firstActiveCol = 2)
    }
  }
  
  # --- A01 Sheet "20": Vacancies & Unemployment ---
  if (!is.null(file_a01)) {
    tbl_20 <- .safe_read(file_a01, "20")
    if (nrow(tbl_20) > 0) {
      sn <- "20"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      
      # Left-side headers (cols 2-4) — absolute values
      col_hdrs_20 <- c("All Vacancies1 (thousands)", "Unemployment2 (thousands)",
                       "Number of unemployed people per vacancy")
      for (ci in seq_along(col_hdrs_20)) {
        writeData(wb, sn, col_hdrs_20[ci], startRow = 1, startCol = ci + 1)
        addStyle(wb, sn, .hs(), rows = 1, cols = ci + 1, stack = TRUE)
      }
      # Right-side headers (cols 7-9) — percentage changes
      for (ci in seq_along(col_hdrs_20)) {
        writeData(wb, sn, col_hdrs_20[ci], startRow = 1, startCol = ci + 6)
        addStyle(wb, sn, .hs(), rows = 1, cols = ci + 6, stack = TRUE)
      }
      
      # Left-side row labels (col 1)
      left_labels_20 <- c("Current", "Quarterly change", "Year on year change",
                          "Pre-pandemic trend (Jan-Mar)", "Since 2024 election")
      for (i in seq_along(left_labels_20)) {
        writeData(wb, sn, left_labels_20[i], startRow = i + 1, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = i + 1, cols = 1, stack = TRUE)
      }
      # Right-side row labels (col 6)
      right_labels_20 <- c("Current", "Quarterly change", "Year on year change",
                           "Pre-pandemic trend (Jan-Mar)", "Since 2010 election")
      for (i in seq_along(right_labels_20)) {
        writeData(wb, sn, right_labels_20[i], startRow = i + 1, startCol = 6)
        addStyle(wb, sn, .cmp_label(), rows = i + 1, cols = 6, stack = TRUE)
      }
      
      # Write source data at row 9
      writeData(wb, sn, tbl_20, colNames = FALSE, startRow = 9)
      .restore_text(wb, sn, tbl_20, 9)
      
      # Detect data start row by date label; col 3's first number is the
      # publication date, which would fool .first_num_r
      off_20 <- 8
      dsr_20 <- .data_row(tbl_20, off_20 + 1)
      
      # Find baseline rows dynamically 
      .fr20 <- function(label) {
        idx <- .find_row(tbl_20, label)
        if (!is.na(idx)) idx + off_20 else NA_integer_
      }
      prepandemic_r <- .fr20("Jan-Mar 2020")
      elec2024_r    <- .fr20("Apr-Jun 2024")
      elec2010_r    <- .fr20("Dec-Feb 2010")
      
      # Source data columns: C(3)=Vacancies, D(4)=Unemployment, E(5)=Ratio
      cl_c <- "C"; cl_d <- "D"; cl_e <- "E"
      
      # LEFT SIDE: absolute value formulas (written to cols B=2, C=3, D=4).
      # Unemployment (D) / Ratio (E): the latest period is often blank ("..");
      # COUNT already skips it, so no extra lag offset is needed.

      # Row 2: Current
      .wf(wb, sn, .fml_last(cl_c, dsr_20), 2, 2)  # Vacancies
      .wf(wb, sn, .fml_last(cl_d, dsr_20), 2, 3)  # Unemployment
      .wf(wb, sn, .fml_last(cl_e, dsr_20), 2, 4)  # Ratio

      # Row 3: Quarterly change
      .wf(wb, sn, .fml_idx_change(cl_c, dsr_20, 3), 3, 2)
      .wf(wb, sn, sprintf("%s-%s", .fml_last(cl_d, dsr_20), .fml_last(cl_d, dsr_20, -3)), 3, 3)
      .wf(wb, sn, sprintf("%s-%s", .fml_last(cl_e, dsr_20), .fml_last(cl_e, dsr_20, -3)), 3, 4)

      # Row 4: Year on year change
      .wf(wb, sn, .fml_idx_change(cl_c, dsr_20, 12), 4, 2)
      .wf(wb, sn, sprintf("%s-%s", .fml_last(cl_d, dsr_20), .fml_last(cl_d, dsr_20, -12)), 4, 3)
      .wf(wb, sn, sprintf("%s-%s", .fml_last(cl_e, dsr_20), .fml_last(cl_e, dsr_20, -12)), 4, 4)

      # Row 5: Pre-pandemic trend
      if (!is.na(prepandemic_r)) {
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_c, dsr_20), cl_c, prepandemic_r), 5, 2)
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_d, dsr_20), cl_d, prepandemic_r), 5, 3)
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_e, dsr_20), cl_e, prepandemic_r), 5, 4)
      }

      # Row 6: Since 2024 election
      if (!is.na(elec2024_r)) {
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_c, dsr_20), cl_c, elec2024_r), 6, 2)
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_d, dsr_20), cl_d, elec2024_r), 6, 3)
        .wf(wb, sn, sprintf("%s-%s$%d", .fml_last(cl_e, dsr_20), cl_e, elec2024_r), 6, 4)
      }
      
      # RIGHT SIDE: percentage change formulas (written to cols G=7, H=8, I=9)
      
      # Row 2: Current (mirrors left side)
      .wf(wb, sn, "B2", 2, 7)
      .wf(wb, sn, "C2", 2, 8)
      .wf(wb, sn, "D2", 2, 9)
      
      # Row 3: Quarterly % change
      .wf(wb, sn, sprintf("IFERROR(B3/%s,\"\")", .fml_last(cl_c, dsr_20, -1)), 3, 7)
      .wf(wb, sn, sprintf("IFERROR(C3/%s,\"\")", .fml_last(cl_d, dsr_20, -1)), 3, 8)
      .wf(wb, sn, sprintf("IFERROR(D3/%s,\"\")", .fml_last(cl_e, dsr_20, -1)), 3, 9)
      
      # Row 4: Year on year % change
      .wf(wb, sn, sprintf("IFERROR(B4/%s,\"\")", .fml_last(cl_c, dsr_20, -12)), 4, 7)
      .wf(wb, sn, sprintf("IFERROR(C4/%s,\"\")", .fml_last(cl_d, dsr_20, -12)), 4, 8)
      .wf(wb, sn, sprintf("IFERROR(D4/%s,\"\")", .fml_last(cl_e, dsr_20, -12)), 4, 9)
      
      # Row 5: Pre-pandemic % change
      if (!is.na(prepandemic_r)) {
        .wf(wb, sn, sprintf("IFERROR(B5/C%d,\"\")", prepandemic_r), 5, 7)
        .wf(wb, sn, sprintf("IFERROR(C5/D%d,\"\")", prepandemic_r), 5, 8)
        .wf(wb, sn, sprintf("IFERROR(D5/E%d,\"\")", prepandemic_r), 5, 9)
      }
      
      # Row 6: Since 2010 election % change
      if (!is.na(elec2010_r)) {
        .wf(wb, sn, sprintf("IFERROR(B6/C%d,\"\")", elec2010_r), 6, 7)
        .wf(wb, sn, sprintf("IFERROR(C6/D%d,\"\")", elec2010_r), 6, 8)
        .wf(wb, sn, sprintf("IFERROR(D6/E%d,\"\")", elec2010_r), 6, 9)
      }
      
      # Formatting
      addStyle(wb, sn, .num_fmt(), rows = 2:6, cols = 2:4, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .pct_fmt(), rows = 2:6, cols = 7:9, gridExpand = TRUE, stack = TRUE)
      
      # Notes
      writeData(wb, sn,
                "*note: 1. Stats for unemployment are from the period before as data unavailable - check this is still the case.",
                startRow = 7, startCol = 1)
      writeData(wb, sn,
                "2. Covid period is different (Jan-Mar) due to reporting periods",
                startRow = 8, startCol = 1)
      addStyle(wb, sn, .cmp_sep(), rows = 6, cols = 1:9, gridExpand = TRUE, stack = TRUE)
      
      # Source data styling
      addStyle(wb, sn, .data_font(), rows = 9:(9 + nrow(tbl_20)),
               cols = 1:ncol(tbl_20), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
      setColWidths(wb, sn, cols = 2:4, widths = 22)
      setColWidths(wb, sn, cols = 6, widths = 30)
      setColWidths(wb, sn, cols = 7:9, widths = 22)
    }
  }
  
  # --- A01 Sheet "21": Vacancies by industry ---
  if (!is.null(file_a01)) {
    tbl_21 <- .safe_read(file_a01, "21")
    if (nrow(tbl_21) > 0 && ncol(tbl_21) >= 3) {
      sn <- "21"
      addWorksheet(wb, sn, tabColour = "#2F5496")
      max_col_21 <- min(ncol(tbl_21), 24)
      
      # Row 1: Industry headers from source data (extend to col 24)
      for (ci in 3:max_col_21) {
        hdr <- as.character(tbl_21[[ci]][1])
        if (!is.na(hdr) && nchar(hdr) > 0) {
          writeData(wb, sn, hdr, startRow = 1, startCol = ci)
          addStyle(wb, sn, .hs(), rows = 1, cols = ci, stack = TRUE)
        }
      }
      
      # Comparison row labels (rows 2-6)
      cmp_labels_21 <- c("Current", "Quarterly change", "year on year change",
                         "pre-pandemic trend (Dec-Feb)", "Since 2010 election")
      for (i in seq_along(cmp_labels_21)) {
        writeData(wb, sn, cmp_labels_21[i], startRow = i + 1, startCol = 2)
        addStyle(wb, sn, .cmp_label(), rows = i + 1, cols = 2, stack = TRUE)
      }
      
      # Write source data at row 9
      writeData(wb, sn, tbl_21, colNames = FALSE, startRow = 9)
      .restore_text(wb, sn, tbl_21, 9)
      
      # Detect data start row by date label; col 3's first number is the
      # publication date, which would fool .first_num_r
      off_21 <- 8
      dsr_21 <- .data_row(tbl_21, off_21 + 1)
      
      # Find baseline rows
      .fr21 <- function(label) {
        idx <- .find_row(tbl_21, label)
        if (!is.na(idx)) idx + off_21 else NA_integer_
      }
      prepandemic_21 <- .fr21("Dec-Feb 2020")
      elec2010_21    <- .fr21("Feb-Apr 2010")
      
      # Write formulas for all industry columns (3 to max_col_21)
      # Col C (3) = All vacancies — uses absolute changes (subtraction)
      # All other cols = industry sub-sectors — use % changes (division-1)
      for (ci in 3:max_col_21) {
        cl <- .col_letter(ci)
        is_total <- (ci == 3)  # "All vacancies" column
        
        # Row 2: Current
        .wf(wb, sn, .fml_last(cl, dsr_21), 2, ci)
        
        # Row 3: Quarterly change
        if (is_total) {
          .wf(wb, sn, .fml_idx_change(cl, dsr_21, 3), 3, ci)
        } else {
          .wf(wb, sn, .fml_idx_pct(cl, dsr_21, 3), 3, ci)
        }
        
        # Row 4: Year on year change
        if (is_total) {
          .wf(wb, sn, .fml_idx_change(cl, dsr_21, 12), 4, ci)
        } else {
          .wf(wb, sn, .fml_idx_pct(cl, dsr_21, 12), 4, ci)
        }
        
        # Row 5: Pre-pandemic (Dec-Feb 2020)
        if (!is.na(prepandemic_21)) {
          if (is_total) {
            .wf(wb, sn, .fml_idx_change_fixed(cl, dsr_21, prepandemic_21), 5, ci)
          } else {
            .wf(wb, sn, .fml_idx_pct_fixed(cl, dsr_21, prepandemic_21), 5, ci)
          }
        }
        
        # Row 6: Since 2010 election
        if (!is.na(elec2010_21)) {
          if (is_total) {
            .wf(wb, sn, .fml_idx_change_fixed(cl, dsr_21, elec2010_21), 6, ci)
          } else {
            .wf(wb, sn, .fml_idx_pct_fixed(cl, dsr_21, elec2010_21), 6, ci)
          }
        }
      }
      
      # Formatting
      addStyle(wb, sn, .num_fmt(), rows = 2:6, cols = 3, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .pct_fmt(), rows = 3:6, cols = 4:max_col_21, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 6, cols = 1:max_col_21,
               gridExpand = TRUE, stack = TRUE)
      
      # Source data styling
      addStyle(wb, sn, .data_font(), rows = 9:(9 + nrow(tbl_21)),
               cols = 1:ncol(tbl_21), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
    }
  }
  
  # --- X09 Sheet "AWE Real_CPI" with comparisons ---
  if (!is.null(file_x09) && exists("tbl_cpi") && nrow(tbl_cpi) > 0) {
    sn <- "AWE Real_CPI"
    addWorksheet(wb, sn, tabColour = "#BF8F00")
    
    # Headers (row 1)
    for (pair in list(c(2, "Total Pay Real AWE (2015 \u00a3)"),
                      c(3, "Total Pay Real AWE (%)"),
                      c(4, "Regular Pay Real AWE (2015 \u00a3)"),
                      c(5, "Regular Pay Real AWE (%)"))) {
      writeData(wb, sn, pair[2], startRow = 1, startCol = as.integer(pair[1]))
      addStyle(wb, sn, .hs(), rows = 1, cols = as.integer(pair[1]), stack = TRUE)
    }
    
    # Comparison rows 2-8
    cmp_labels_cpi <- c("Current  (3mo avg)", "Change on quarter  (3mo avg)",
                        "Change year on year (3mo avg)",
                        "Change since Covid-19 (2019 average)",
                        "Change since 2010", "Change since financial crisis",
                        "Change since 2024 election")
    for (i in seq_along(cmp_labels_cpi)) {
      writeData(wb, sn, cmp_labels_cpi[i], startRow = i + 1, startCol = 1)
      addStyle(wb, sn, .cmp_label(), rows = i + 1, cols = 1, stack = TRUE)
    }
    
    # Write source data at row 9
    if (is.character(tbl_cpi[[1]])) tbl_cpi[[1]] <- .detect_dates(tbl_cpi[[1]])
    if (inherits(tbl_cpi[[1]], c("POSIXct", "POSIXt"))) tbl_cpi[[1]] <- as.Date(tbl_cpi[[1]])
    if (is.numeric(tbl_cpi[[1]])) tbl_cpi[[1]] <- as.Date(tbl_cpi[[1]], origin = "1899-12-30")
    writeData(wb, sn, tbl_cpi, colNames = FALSE, startRow = 9)
    .restore_text(wb, sn, tbl_cpi, 9)
    
    # Detect data start rows
    off_cpi <- 8
    dsr_cpi_b <- .first_num_r(tbl_cpi, 2) + off_cpi  # Total Pay £ (col B)
    dsr_cpi_e <- .first_num_r(tbl_cpi, 5) + off_cpi  # Total Pay YoY % (col E)
    dsr_cpi_f <- .first_num_r(tbl_cpi, 6) + off_cpi  # Regular Pay £ (col F)
    dsr_cpi_i <- .first_num_r(tbl_cpi, 9) + off_cpi  # Regular Pay YoY % (col I)
    
    # Find baseline rows
    dates_cpi <- .detect_dates(tbl_cpi[[1]])
    .drc <- function(d) {
      idx <- which(dates_cpi == as.Date(d))
      if (length(idx)) idx[1] + off_cpi else NA_integer_
    }
    # each baseline is the 3-month average ending at the reference month
    covid_cpi_r1 <- .drc("2019-12-01"); covid_cpi_r3 <- .drc("2020-02-01")
    e2010_cpi_r1 <- .drc("2010-02-01"); e2010_cpi_r3 <- .drc("2010-04-01")
    fc_cpi_r1    <- .drc("2007-10-01"); fc_cpi_r3    <- .drc("2007-12-01")
    e2024_cpi_r1 <- .drc("2024-04-01"); e2024_cpi_r3 <- .drc("2024-06-01")
    
    # Write formulas
    # Col 2: Total Pay £ (source col B), Col 4: Regular Pay £ (source col F)
    for (spec in list(list(ci = 2, cl = "B", dsr = dsr_cpi_b),
                      list(ci = 4, cl = "F", dsr = dsr_cpi_f))) {
      cl <- spec$cl; dsr <- spec$dsr; ci <- spec$ci
      
      # Row 2: Current (3mo avg)
      .wf(wb, sn, .fml_avg_last(cl, dsr), 2, ci)
      
      # Row 3: Change on quarter (3mo avg) *52
      .wf(wb, sn, sprintf("(%s)*52", .fml_change_avg(cl, dsr, -5)), 3, ci)
      
      # Row 4: Change YoY (3mo avg) *52
      .wf(wb, sn, sprintf("(%s)*52", .fml_change_avg(cl, dsr, -14)), 4, ci)
      
      # Rows 5-8: Change since fixed baselines *52
      baselines <- list(
        list(r = 5, r1 = covid_cpi_r1, r3 = covid_cpi_r3),
        list(r = 6, r1 = e2010_cpi_r1, r3 = e2010_cpi_r3),
        list(r = 7, r1 = fc_cpi_r1,    r3 = fc_cpi_r3),
        list(r = 8, r1 = e2024_cpi_r1, r3 = e2024_cpi_r3)
      )
      for (bl in baselines) {
        if (!is.na(bl$r1) && !is.na(bl$r3)) {
          .wf(wb, sn, sprintf("(%s-AVERAGE($%s$%d:$%s$%d))*52",
                              .fml_avg_last(cl, dsr), cl, bl$r1, cl, bl$r3), bl$r, ci)
        }
      }
    }
    
    # Col 3: Total Pay real-pay % — only the year-on-year growth rate is shown,
    # on its own row; Current and quarter rows stay blank (source layout).
    .wf(wb, sn, .fml_last("E", dsr_cpi_e), 4, 3)
    for (bl in list(
      list(r = 5, r1 = covid_cpi_r1, r3 = covid_cpi_r3),
      list(r = 6, r1 = e2010_cpi_r1, r3 = e2010_cpi_r3),
      list(r = 7, r1 = fc_cpi_r1,    r3 = fc_cpi_r3),
      list(r = 8, r1 = e2024_cpi_r1, r3 = e2024_cpi_r3)
    )) {
      if (!is.na(bl$r1) && !is.na(bl$r3))
        .wf(wb, sn, sprintf("IFERROR((%s/AVERAGE($%s$%d:$%s$%d))-1,\"\")",
                            .fml_avg_last("B", dsr_cpi_b), "B", bl$r1, "B", bl$r3), bl$r, 3)
    }
    
    # Col 5: Regular Pay real-pay % — year-on-year growth rate only (source col I).
    .wf(wb, sn, .fml_last("I", dsr_cpi_i), 4, 5)
    for (bl in list(
      list(r = 5, r1 = covid_cpi_r1, r3 = covid_cpi_r3),
      list(r = 6, r1 = e2010_cpi_r1, r3 = e2010_cpi_r3),
      list(r = 7, r1 = fc_cpi_r1,    r3 = fc_cpi_r3),
      list(r = 8, r1 = e2024_cpi_r1, r3 = e2024_cpi_r3)
    )) {
      if (!is.na(bl$r1) && !is.na(bl$r3))
        .wf(wb, sn, sprintf("IFERROR((%s/AVERAGE($%s$%d:$%s$%d))-1,\"\")",
                            .fml_avg_last("F", dsr_cpi_f), "F", bl$r1, "F", bl$r3), bl$r, 5)
    }
    
    # Formatting
    addStyle(wb, sn, .gbp_fmt(), rows = 2:8, cols = c(2, 4), gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sn, .pct_fmt(), rows = 2:8, cols = c(3, 5), gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sn, .cmp_sep(), rows = 8, cols = 1:9, gridExpand = TRUE, stack = TRUE)
    
    # Source data styling
    date_rows_cpi <- which(!is.na(tbl_cpi[[1]])) + 8
    if (length(date_rows_cpi) > 0)
      addStyle(wb, sn, .date_fmt(), rows = date_rows_cpi, cols = 1, stack = TRUE)
    addStyle(wb, sn, .data_font(), rows = 9:(9 + nrow(tbl_cpi)),
             cols = 1:ncol(tbl_cpi), gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, sn, cols = 1, widths = 32)
    setColWidths(wb, sn, cols = 2:min(ncol(tbl_cpi), 9), widths = 16)
  }
  
  # --- HR1 Sheet "1a" with comparisons ---
  if (!is.null(file_hr1)) {
    tbl_1a <- .safe_read(file_hr1, "1a")
    if (nrow(tbl_1a) > 0 && ncol(tbl_1a) >= 2) {
      if (!"1a" %in% names(wb)) addWorksheet(wb, "1a", tabColour = "#C00000")
      sn <- "1a"
      max_ci_1a <- min(ncol(tbl_1a), 13)
      
      # Row 1: Region headers from source data (row 4 in source = column headers)
      # Use raw_text to get original header text (may be NA in coerced numeric cols)
      raw_1a <- attr(tbl_1a, "raw_text")
      for (ci in 2:max_ci_1a) {
        hdr <- if (!is.null(raw_1a)) raw_1a[[ci]][4] else as.character(tbl_1a[[ci]][4])
        if (!is.na(hdr) && nchar(trimws(hdr)) > 0) {
          writeData(wb, sn, hdr, startRow = 1, startCol = ci)
          addStyle(wb, sn, .hs(), rows = 1, cols = ci, stack = TRUE)
        }
      }
      
      # Comparison row labels (rows 2-7)
      cmp_labels_1a <- c("Current", "Average since start of 2023",
                         "Average pre-covid (April 2019-February 2020)",
                         "Change on month", "Change on quarter", "Change on year")
      for (i in seq_along(cmp_labels_1a)) {
        writeData(wb, sn, cmp_labels_1a[i], startRow = i + 1, startCol = 1)
        addStyle(wb, sn, .cmp_label(), rows = i + 1, cols = 1, stack = TRUE)
      }
      
      # Write source data at row 9
      writeData(wb, sn, tbl_1a, colNames = FALSE, startRow = 9)
      .restore_text(wb, sn, tbl_1a, 9)
      
      # Detect data rows. der_1a = last dated row, used for absolute-row
      # formulas (region columns can have gaps that break COUNT-relative ones)
      off_1a <- 8  # write row (9) minus 1
      dsr_1a <- .first_num_r(tbl_1a, 2) + off_1a
      hr1_dates <- .detect_dates(tbl_1a[[1]])
      hr1_valid <- which(!is.na(hr1_dates))
      der_1a <- if (length(hr1_valid)) hr1_valid[length(hr1_valid)] + off_1a else dsr_1a

      .dra <- function(d) {
        idx <- which(hr1_dates == as.Date(d))
        if (length(idx)) idx[1] + off_1a else NA_integer_
      }
      precov_start <- .dra("2019-04-01")
      precov_end   <- .dra("2020-02-01")

      # Write formulas for all region columns
      for (ci in 2:max_ci_1a) {
        cl <- .col_letter(ci)
        rng <- sprintf("%s$%d:%s$1048576", cl, dsr_1a, cl)
        arng <- sprintf("$A$%d:$A$1048576", dsr_1a)

        # Row 2: Current (avg of last 3)
        .wf(wb, sn, .af_avg(cl, der_1a, 3), 2, ci)

        # Row 3: Average since start of 2023 (MATCH serial 44927 = Jan 1 2023)
        .wf(wb, sn, sprintf("AVERAGE(INDEX(%s,MATCH(44927,%s,0)):INDEX(%s,COUNTA(%s)))",
                            rng, arng, rng, rng), 3, ci)

        # Row 4: Average pre-covid (fixed range)
        if (!is.na(precov_start) && !is.na(precov_end))
          .wf(wb, sn, .fml_avg_range(cl, precov_start, precov_end), 4, ci)

        # Rows 5-7: Change on month / quarter / year (3mo-avg ratios)
        .wf(wb, sn, .af_pctavg(cl, der_1a, 3, 1),  5, ci)
        .wf(wb, sn, .af_pctavg(cl, der_1a, 3, 3),  6, ci)
        .wf(wb, sn, .af_pctavg(cl, der_1a, 3, 12), 7, ci)
      }
      
      # Formatting
      addStyle(wb, sn, .num_fmt(), rows = 2:4, cols = 2:max_ci_1a, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .pct_fmt(), rows = 5:7, cols = 2:max_ci_1a, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sn, .cmp_sep(), rows = 7, cols = 1:max_ci_1a,
               gridExpand = TRUE, stack = TRUE)
      
      # Source data styling
      addStyle(wb, sn, .data_font(), rows = 9:(9 + nrow(tbl_1a)),
               cols = 1:ncol(tbl_1a), gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, sn, cols = 1, widths = 32)
    }
  }
  
  # --- Employee levels - LFS,RTI,WFJ (cross-reference sheet) ---
  if (exists("tbl_rtisa") && nrow(tbl_rtisa) > 0 && !is.null(file_a01)) {
    el_sn <- "Employee levels - LFS,RTI,WFJ"
    addWorksheet(wb, el_sn, tabColour = "#8DB4E2")
    
    pay_sn_ref <- "'1. Payrolled employees (UK)'"
    
    
    # Payroll: tbl_rtisa written at startRow=5
    rtisa_labels_el <- trimws(as.character(tbl_rtisa[[1]]))
    pay_date_pat <- "^(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}$"
    pay_date_idx <- which(grepl(pay_date_pat, rtisa_labels_el, ignore.case = TRUE))
    pay_n_dates <- length(pay_date_idx)
    pay_first_src <- if (pay_n_dates > 0) pay_date_idx[1] else NA
    pay_first_out <- if (!is.na(pay_first_src)) pay_first_src + 4 else NA
    # El row 2 → payroll (pay_first_out - 1), El row 3 → payroll pay_first_out
    pay_off <- if (!is.na(pay_first_out)) pay_first_out - 3 else NA
    
    # WFJ: tbl_5 written at startRow=7 in sheet "5"
    wfj_n <- 0; wfj_off <- NA
    if (exists("tbl_5") && nrow(tbl_5) > 0) {
      s5_labels_el <- trimws(as.character(tbl_5[[1]]))
      wfj_date_idx <- which(grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{2}", s5_labels_el))
      wfj_n <- length(wfj_date_idx)
      if (wfj_n > 0) {
        wfj_first_out <- wfj_date_idx[1] + 6  # startRow=7
        wfj_off <- wfj_first_out - 2  # El row 2 → WFJ wfj_first_out
      }
    }
    
    # LFS: tbl_2_full at startRow=11 in sheet "2"
    lfs_n <- 0; lfs2_off <- NA; lfs3_off <- NA
    lfs_pat_el <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{4}$"
    if (exists("tbl_2_full") && nrow(tbl_2_full) > 0) {
      s2_labels_el <- trimws(as.character(tbl_2_full[[1]]))
      lfs_idx <- which(grepl(lfs_pat_el, s2_labels_el))
      lfs_n <- length(lfs_idx)
      if (lfs_n > 0) {
        lfs2_first_out <- lfs_idx[1] + 10  # startRow=11
        lfs2_off <- lfs2_first_out - 3  # El row 3 → Sheet2 lfs2_first_out
      }
    }
    
    # Sheet "3": re-read to find employee jobs data positions (written at startRow=1)
    tbl_3_el <- .safe_read(file_a01, "3")
    if (nrow(tbl_3_el) > 0 && ncol(tbl_3_el) >= 3) {
      s3_labels_el <- trimws(as.character(tbl_3_el[[1]]))
      lfs3_idx <- which(grepl(lfs_pat_el, s3_labels_el))
      if (length(lfs3_idx) > 0) {
        lfs3_first_out <- lfs3_idx[1] + 1  # startRow=2 (title in row 1)
        lfs3_off <- lfs3_first_out - 3  # El row 3 → Sheet3 lfs3_first_out
      }
    }
    
    # ROw ranges 
    pay_last_el <- if (pay_n_dates > 0) 2 + pay_n_dates else 2
    lfs_last_el <- if (lfs_n > 0) 2 + lfs_n else 2
    wfj_last_el <- if (wfj_n > 0) 1 + wfj_n else 2
    max_el_row <- max(pay_last_el, lfs_last_el, wfj_last_el, na.rm = TRUE)
    
    # - row 1: Headers
    for (pair in list(c(1, "RTI - Payroll employees"), c(6, "LFS"), c(9, "WFJ"),
                      c(14, "LFS employees"), c(15, "RTI employees"),
                      c(16, "WFJ employee jobs"))) {
      writeData(wb, el_sn, pair[2], startRow = 1, startCol = as.integer(pair[1]))
      addStyle(wb, el_sn, .hs(), rows = 1, cols = as.integer(pair[1]), stack = TRUE)
    }
    
    for (pair in list(c(3, "New date format"), c(4, "3-month rolling average"),
                      c(6, "Quarter "), c(7, "Employee jobs"))) {
      writeData(wb, el_sn, pair[2], startRow = 2, startCol = as.integer(pair[1]))
    }
    
    if (!is.na(pay_off)) {
      .wf(wb, el_sn, sprintf("%s!A%d", pay_sn_ref, 2 + pay_off), 2, 1)
      .wf(wb, el_sn, sprintf("%s!B%d", pay_sn_ref, 2 + pay_off), 2, 2)
    }
    # I2, J2: WFJ first reference
    if (!is.na(wfj_off)) {
      .wf(wb, el_sn, sprintf("'5'!A%d", 2 + wfj_off), 2, 9)
      .wf(wb, el_sn, sprintf("'5'!C%d", 2 + wfj_off), 2, 10)
    }
    
    #payroll formulas (cols A, B): rows 3 to pay_last_el ----
    if (!is.na(pay_off) && pay_last_el >= 3) {
      for (r in 3:pay_last_el) {
        pay_r <- r + pay_off
        .wf(wb, el_sn, sprintf("DATEVALUE(%s!A%d)", pay_sn_ref, pay_r), r, 1)
        .wf(wb, el_sn, sprintf("%s!B%d", pay_sn_ref, pay_r), r, 2)
      }
    }
    
    pay_dates_el <- .detect_dates(tbl_rtisa[[1]][pay_date_idx])
    if (pay_n_dates >= 3) {
      for (i in 3:pay_n_dates) {
        r <- i + 2  # date index 1→el_row 3, index 3→el_row 5
        d_end <- pay_dates_el[i]
        d_start <- d_end %m-% months(2)
        label <- sprintf("%s-%s %s", format(d_start, "%b"), format(d_end, "%b"),
                         format(d_end, "%Y"))
        writeData(wb, el_sn, label, startRow = r, startCol = 3)
        .wf(wb, el_sn, sprintf("AVERAGE(B%d:B%d)", r - 2, r), r, 4)
      }
    }
    
    # lFS formulas (cols F, G): rows 3 to lfs_last_el
    if (lfs_n > 0 && !is.na(lfs2_off)) {
      for (r in 3:lfs_last_el) {
        .wf(wb, el_sn, sprintf("'2'!A%d", r + lfs2_off), r, 6)
        if (!is.na(lfs3_off))
          .wf(wb, el_sn, sprintf("'3'!C%d", r + lfs3_off), r, 7)
      }
    }
    
    # wjf formulas 
    if (wfj_n > 0 && !is.na(wfj_off) && wfj_last_el >= 3) {
      for (r in 3:wfj_last_el) {
        .wf(wb, el_sn, sprintf("'5'!A%d", r + wfj_off), r, 9)
        .wf(wb, el_sn, sprintf("'5'!C%d", r + wfj_off), r, 10)
      }
    }
    
    
    if (lfs_n > 0 && !is.na(lfs2_off) && !is.na(lfs3_off) && pay_n_dates >= 3) {
      # Find the LFS quarter index for "Dec-Feb 2020" (pre-pandemic reference)
      if (exists("tbl_2_full")) {
        s2_labels_all <- trimws(as.character(tbl_2_full[[1]]))[lfs_idx]
        lfs_prepand_idx <- grep("Dec-Feb\\s+2020", s2_labels_all, ignore.case = TRUE)
        lfs_prepand_el <- if (length(lfs_prepand_idx) > 0) lfs_prepand_idx[1] + 2 else NA
      } else { lfs_prepand_el <- NA }
      
      # G reference row: the LFS employees value at pre-pandemic quarter
      g_ref_el <- lfs_prepand_el
      
      # D reference row: payroll rolling avg at ~Jan 2020
      pay_jan2020_idx <- which(pay_dates_el == as.Date("2020-01-01"))
      d_ref_el <- if (length(pay_jan2020_idx) > 0) pay_jan2020_idx[1] + 2 else NA
      
      # J reference row for WFJ
      if (exists("tbl_5") && wfj_n > 0) {
        s5_dates_el <- trimws(as.character(tbl_5[[1]]))[wfj_date_idx]
        wfj_prepand_idx <- grep("Dec\\s+19", s5_dates_el, ignore.case = TRUE)
        wfj_prepand_el <- if (length(wfj_prepand_idx) > 0) wfj_prepand_idx[1] + 1 else NA
      } else { wfj_prepand_el <- NA }
      
      # Determine which LFS quarter aligns with the start of payroll data
      # The LFS quarters that correspond to payroll months start ~55 quarters after first LFS
      # We compute the LFS quarter offset to align with el_row 2
      if (lfs_n > 55) {
        lfs_align_offset <- 55  # approximate; aligns LFS quarters to payroll months
        
        # L, M, N: LFS-aligned percentages
        lfs_align_end <- min(pay_last_el, 2 + lfs_n - lfs_align_offset)
        for (r in 2:min(lfs_align_end, pay_last_el)) {
          lfs_src_r <- r + lfs_align_offset
          if (lfs_src_r <= lfs_last_el) {
            .wf(wb, el_sn, sprintf("F%d", lfs_src_r), r, 12)  # L: quarter label
            .wf(wb, el_sn, sprintf("RIGHT(L%d,4)", r), r, 13)  # M: year
            if (!is.na(g_ref_el))
              .wf(wb, el_sn, sprintf("IFERROR((G%d/$G$%d)*100,\"\")", lfs_src_r, g_ref_el), r, 14)  # N: LFS %
          }
          if (!is.na(d_ref_el) && r + 3 <= pay_last_el)
            .wf(wb, el_sn, sprintf("IFERROR((D%d/$D$%d)*100,\"\")", r + 3, d_ref_el), r, 15)  # O: RTI %
        }
        
        # P: WFJ % (every 3rd row)
        if (!is.na(wfj_prepand_el) && wfj_n > 0) {
          wfj_qtr_idx <- 0
          for (r in seq(2, min(pay_last_el, wfj_last_el), by = 3)) {
            wfj_src_el <- r + lfs_align_offset
            wfj_j_r <- wfj_src_el
            if (wfj_j_r >= 2 && wfj_j_r <= wfj_last_el)
              .wf(wb, el_sn, sprintf("IFERROR(J%d/$J$%d*100,\"\")", wfj_j_r, wfj_prepand_el), r, 16)
          }
        }
      }
    }
    
    # - Formatting
    addStyle(wb, el_sn, .date_fmt(), rows = 3:pay_last_el, cols = 1, stack = TRUE)
    addStyle(wb, el_sn, .num_fmt(), rows = 2:max_el_row, cols = c(2, 4, 7, 10),
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, el_sn, .data_font(), rows = 1:max_el_row, cols = 1:16,
             gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, el_sn, cols = 1:16,
                 widths = c(14, 14, 14, 16, 3, 14, 14, 3, 14, 14, 3, 14, 8, 14, 14, 14))
  }
  
  
  #  Add generated sheets
  
  
  # --- How to update ---
  addWorksheet(wb, "How to update", tabColour = "#FFC000")
  writeData(wb, "How to update", data.frame(V1 = c(
    paste0("Labour Market Statistics Briefing \u2014 ", ref_label), "",
    "HOW TO UPDATE THIS WORKBOOK", "----------------------------",
    "This workbook is auto-generated from ONS source datasets.",
    "To update, download the latest files from ONS and upload them via the app.", "",
    "Required: A01, HR1, X09, RTISA",
    "Optional: CLA01, X02, OECD (3 files)", "",
    paste0("LFS period: ", lab_cur),
    paste0("Comparison periods: vs ", lab_q, " | vs ", lab_y, " | vs ", lab_covid, " | vs ", lab_elec)
  )), colNames = FALSE)
  addStyle(wb, "How to update", .ts(), rows = 1, cols = 1)
  setColWidths(wb, "How to update", cols = 1, widths = 80)
  
  # --- Data links ---
  addWorksheet(wb, "Data links", tabColour = "#FFC000")
  writeData(wb, "Data links", data.frame(
    Sheet = c("1. Payrolled employees (UK)", "23. Employees Industry", "2", "5",
              "10", "11", "13", "15", "18", "19", "20", "21", "22",
              "1 UK", "AWE Real_CPI", "1a/1b/2a/2b",
              "LFS Labour market flows SA", "RTI. Employee flows (UK)",
              "Unemployment/Employment/Inactivity", "Regional breakdowns"),
    Source = c("RTISA", "RTISA", rep("A01", 11), "CLA01", "X09", "HR1",
               "X02", "RTISA", "OECD", "A01"),
    stringsAsFactors = FALSE
  ), headerStyle = .hs())
  setColWidths(wb, "Data links", cols = 1:2, widths = c(40, 15))
  
  # --- Dashboard ---
  # formula-driven: every figure is a live reference into a sheet's comparison
  # block, so the dashboard updates when the source sheets do.
  addWorksheet(wb, "Dashboard", tabColour = "#00703C")
  writeData(wb, "Dashboard", paste0("Labour Market Dashboard \u2014 ", ref_label),
            startRow = 1, startCol = 2)
  addStyle(wb, "Dashboard", .ts(), rows = 1, cols = 2)
  writeData(wb, "Dashboard", paste0("LFS period: ", lab_cur),
            startRow = 2, startCol = 2)
  addStyle(wb, "Dashboard", .ss(), rows = 2, cols = 2)

  dash_hdr <- 5
  writeData(wb, "Dashboard", "Key Metrics", startRow = dash_hdr, startCol = 2)
  dash_cols <- c("Current", "Change on quarter", "Change on year",
                 "Change since Covid", "Change since election")
  for (k in seq_along(dash_cols))
    writeData(wb, "Dashboard", dash_cols[k], startRow = dash_hdr, startCol = 3 + k)
  addStyle(wb, "Dashboard", .hs(), rows = dash_hdr, cols = c(2, 4:8), gridExpand = TRUE)

  # cells = current, change-on-quarter, -on-year, -since-covid, -since-election
  dash_metrics <- list(
    list(lab = "Employment (000s) 16+",              sh = "2",  cells = c("B5","B6","B7","B8","B10"),      rate = FALSE),
    list(lab = "Employment rate (16-64)",            sh = "2",  cells = c("K5","K6","K7","K8","K10"),      rate = TRUE),
    list(lab = "Unemployment, 16+ (000s)",           sh = "2",  cells = c("D5","D6","D7","D8","D10"),      rate = FALSE),
    list(lab = "Unemployment rate, 16+",             sh = "2",  cells = c("E5","E6","E7","E8","E10"),      rate = TRUE),
    list(lab = "Economic inactivity (000s) (16-64)", sh = "2",  cells = c("P5","P6","P7","P8","P10"),      rate = FALSE),
    list(lab = "Economic inactivity rate (16-64)",   sh = "2",  cells = c("Q5","Q6","Q7","Q8","Q10"),      rate = TRUE),
    list(lab = "50-64s inactivity (000s)",           sh = "2",  cells = c("BD5","BD6","BD7","BD8","BD10"), rate = FALSE),
    list(lab = "50-64s inactivity rate",             sh = "2",  cells = c("BE5","BE6","BE7","BE8","BE10"), rate = TRUE),
    list(lab = "Payrolled employees (000s)",         sh = "1. Payrolled employees (UK)",
         cells = c("B2","D2","E2","G2","H2"), rate = FALSE),
    list(lab = "Vacancies (000s)",                   sh = "20", cells = c("B2","B3","B4","B5","B6"),       rate = FALSE),
    # wage rows: current = yearly % growth; the change columns are the £ change
    # in annual earnings (sheet col B, *52), mirroring the source-of-truth design.
    list(lab = "Wage growth in cash terms (total pay, including bonuses, yearly % change)",
         sh = "13", cells = c("D4","B5","B6","B7","B8"), rate = TRUE, chg = "gbp"),
    list(lab = "Wage growth adjusted for inflation (total pay, including bonuses, yearly % change)",
         sh = "AWE Real_CPI", cells = c("C4","B3","B4","B5","B8"), rate = TRUE, chg = "gbp")
  )

  dash_r1 <- dash_hdr + 1
  for (i in seq_along(dash_metrics)) {
    m <- dash_metrics[[i]]
    r <- dash_r1 + i - 1
    writeData(wb, "Dashboard", m$lab, startRow = r, startCol = 2)
    for (k in 1:5) {
      f <- sprintf("'%s'!%s", m$sh, m$cells[k])
      if (m$rate && k == 1) f <- paste0(f, "/100")  # current rate -> proportion
      .wf(wb, "Dashboard", f, r, 3 + k)
    }
    if (m$rate) {
      addStyle(wb, "Dashboard", .pct_fmt(), rows = r, cols = 4, stack = TRUE)
      chg_style <- if (isTRUE(m$chg == "gbp")) .gbp_fmt() else .pp2_fmt()
      addStyle(wb, "Dashboard", chg_style, rows = r, cols = 5:8, gridExpand = TRUE, stack = TRUE)
    } else {
      addStyle(wb, "Dashboard", .num_fmt(), rows = r, cols = 4:8, gridExpand = TRUE, stack = TRUE)
    }
  }
  dash_rN <- dash_r1 + length(dash_metrics) - 1

  for (ci in 5:8) for (ri in dash_r1:dash_rN) {
    conditionalFormatting(wb, "Dashboard", cols = ci, rows = ri,
                          type = "expression", rule = ">0", style = .pos())
    conditionalFormatting(wb, "Dashboard", cols = ci, rows = ri,
                          type = "expression", rule = "<0", style = .neg())
  }
  addStyle(wb, "Dashboard", .data_font(), rows = dash_r1:dash_rN, cols = 2,
           gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "Dashboard", cols = 2:8, widths = c(46, 3, 16, 16, 16, 18, 20))
  
  # (chart sheets removed — data sheets only)
  
  # --- Separator sheets ---
  for (sep_name in c("Redundancies >>>", "Labour market flows >>>",
                     "International Comparisons >>>")) {
    if (!sep_name %in% names(wb)) {
      addWorksheet(wb, sep_name, tabColour = "#8DB4E2")
      writeData(wb, sep_name, data.frame(X = sep_name), startRow = 1, colNames = FALSE)
      addStyle(wb, sep_name, .sep(), rows = 1, cols = 1)
      setColWidths(wb, sep_name, cols = 1, widths = 50)
      setRowHeights(wb, sep_name, rows = 1, heights = 60)
    }
  }
  
  # --- Placeholder sheets for missing supplementary data ---
  for (missing_info in list(
    list(name = "1 UK", file = file_cla01, msg_null = "Upload CLA01 file to populate this sheet.",
         msg_fail = "CLA01 file uploaded but data could not be read. Check the file has a sheet named '1' or 'People SA'."),
    list(name = "LFS Labour market flows SA", file = file_x02, msg_null = "Upload X02 file to populate this sheet.",
         msg_fail = "X02 file uploaded but data could not be read. Check the file has a sheet named 'LFS Labour market flows SA'.")
  )) {
    if (!missing_info$name %in% names(wb)) {
      msg <- if (is.null(missing_info$file)) missing_info$msg_null else missing_info$msg_fail
      addWorksheet(wb, missing_info$name)
      writeData(wb, missing_info$name, data.frame(Note = msg))
    }
  }
  
  has_oecd <- !is.null(file_oecd_unemp) || !is.null(file_oecd_emp) || !is.null(file_oecd_inact)
  if (!has_oecd) {
    for (sn in c("Final Table", "Unemployment", "Employment", "Inactivity")) {
      if (!sn %in% names(wb)) {
        addWorksheet(wb, sn, tabColour = "#2F5496")
        writeData(wb, sn, data.frame(Note = "Upload OECD data files to populate international comparisons."))
      }
    }
  } else {
    # --- Final Table: OECD international comparison ---
    if (!"Final Table" %in% names(wb)) addWorksheet(wb, "Final Table", tabColour = "#2F5496")
    ft <- "Final Table"

    ft_un <- NULL
    if (!is.null(file_oecd_unemp) && !is.data.frame(file_oecd_unemp)) {
      ft_sh <- .detect_sheet(file_oecd_unemp, c("Table", "Unemployment", "Sheet1", "Data"))
      if (!is.null(ft_sh)) ft_un <- tryCatch(.safe_read(file_oecd_unemp, ft_sh), error = function(e) NULL)
    }

    if (!is.null(ft_un) && nrow(ft_un) >= 9 && ncol(ft_un) >= 3) {
      # OECD export layout: country names in col 1, quarter headers in row 7,
      # data from row 9. The OECD sheets are written verbatim, so these row /
      # column positions address them directly.
      # quarter headers: read from raw_text — .safe_read coerces these data
      # columns to numeric, which would turn the row-7 labels into NA
      ft_rt <- attr(ft_un, "raw_text")
      qhdr  <- if (!is.null(ft_rt)) vapply(ft_rt, function(col) as.character(col[7]), character(1))
               else as.character(unlist(ft_un[7, ]))
      names_col <- trimws(as.character(ft_un[[1]]))
      ctry_rows <- integer(0)
      for (r in 9:nrow(ft_un)) {
        nm <- names_col[r]
        if (is.na(nm) || !nzchar(nm) ||
            grepl("©|terms|copyright", nm, ignore.case = TRUE)) break
        ctry_rows <- c(ctry_rows, r)
      }
      short_map <- c("United Kingdom" = "UK", "United States" = "US",
                     "Euro area (20 countries)" = "Euro Area")

      writeData(wb, ft, "OECD international comparison — infra-annual labour statistics",
                startRow = 1, startCol = 1)
      addStyle(wb, ft, .ts(), rows = 1, cols = 1)
      for (h in list(c(4, "Time period"), c(5, "Unemployment rate (15+, %)"),
                     c(6, "Employment rate (15-64, %)"), c(7, "Inactivity rate (15-64, %)")))
        writeData(wb, ft, h[2], startRow = 3, startCol = as.integer(h[1]))
      addStyle(wb, ft, .hs(), rows = 3, cols = 4:7, gridExpand = TRUE)

      # latest quarter present for each OECD country
      latest_q <- function(src_r) {
        vals <- suppressWarnings(as.numeric(unlist(ft_un[src_r, 3:ncol(ft_un)])))
        got  <- which(!is.na(vals))
        if (length(got)) qhdr[2 + got[length(got)]] else ""
      }
      countries <- lapply(ctry_rows, function(r) list(name = names_col[r], quarter = latest_q(r)))
      # always carry a Euro area row — blank until the OECD files include it
      if (!any(grepl("euro area", vapply(countries, `[[`, "", "name"), ignore.case = TRUE)))
        countries <- c(countries, list(list(name = "Euro area (20 countries)", quarter = "")))

      uk_row <- euro_row <- NA_integer_
      for (i in seq_along(countries)) {
        ct    <- countries[[i]]
        out_r <- 3 + i          # Final Table row (countries from row 4)
        short <- if (ct$name %in% names(short_map)) short_map[[ct$name]] else ct$name
        writeData(wb, ft, ct$name,    startRow = out_r, startCol = 1)
        writeData(wb, ft, ct$quarter, startRow = out_r, startCol = 2)
        writeData(wb, ft, short,      startRow = out_r, startCol = 3)
        .wf(wb, ft, sprintf('IFERROR(RIGHT(B%d,2)&" "&LEFT(B%d,4),"")', out_r, out_r), out_r, 4)
        # match the country by name in each OECD sheet; blank if it is absent
        for (s in list(c(5, "Unemployment"), c(6, "Employment"), c(7, "Inactivity")))
          .wf(wb, ft, sprintf(
            'IFERROR(LOOKUP(9.99999999999999E+307,INDEX(%s!$C$9:$AZ$45,MATCH($A%d,%s!$A$9:$A$45,0),0))/100,"")',
            s[2], out_r, s[2]), out_r, as.integer(s[1]))
        if (grepl("united kingdom", ct$name, ignore.case = TRUE)) uk_row   <- out_r
        if (grepl("euro area",      ct$name, ignore.case = TRUE)) euro_row <- out_r
      }
      last_r <- 3 + length(countries)
      addStyle(wb, ft, .pct_fmt(), rows = 4:last_r, cols = 5:7, gridExpand = TRUE, stack = TRUE)

      # UK relative to the Euro area (blank until Euro-area data is present)
      if (!is.na(uk_row) && !is.na(euro_row)) {
        rr <- last_r + 2
        writeData(wb, ft, "UK relative to Euro area", startRow = rr, startCol = 4)
        addStyle(wb, ft, .cmp_label(), rows = rr, cols = 4, stack = TRUE)
        for (cc in 5:7)
          .wf(wb, ft, sprintf('IFERROR(%s%d-%s%d,"")',
                              .col_letter(cc), euro_row, .col_letter(cc), uk_row), rr, cc)
        addStyle(wb, ft, .pct_fmt(), rows = rr, cols = 5:7, gridExpand = TRUE, stack = TRUE)
      }
      setColWidths(wb, ft, cols = 1:7, widths = c(26, 12, 12, 14, 24, 24, 24))
    } else {
      writeData(wb, ft, data.frame(Note = "OECD unemployment file could not be read."))
    }
  }
  
  # Regional breakdowns (pointer)
  if (!"Regional breakdowns" %in% names(wb)) {
    addWorksheet(wb, "Regional breakdowns", tabColour = "#843C0C")
    writeData(wb, "Regional breakdowns",
              data.frame(Note = "See Sheet '22' for regional Labour Force Survey data."))
  }
  
  # International Comparisons (long series pointer)
  if (!"International Comparisons" %in% names(wb)) {
    addWorksheet(wb, "International Comparisons", tabColour = "#2F5496")
    writeData(wb, "International Comparisons",
              data.frame(Note = "See Unemployment, Employment, Inactivity sheets."))
  }
  
  
  
  
  desired_order <- c(
    "How to update", "Data links", "Dashboard",
    "1. Payrolled employees (UK)", "23. Employees Industry",
    "2", "3", "5", "10", "11", "13", "15", "18", "20", "21", "22",
    "1 UK", "AWE Real_CPI",
    "Redundancies >>>", "1a", "1b", "2a", "2b",
    "Labour market flows >>>", "LFS Labour market flows SA", "RTI. Employee flows (UK)",
    "International Comparisons >>>", "Final Table", "Unemployment", "Employment", "Inactivity",
    "Employee levels - LFS,RTI,WFJ",
    "International Comparisons", "Regional breakdowns"
  )
  
  current_sheets <- names(wb)
  # Build reorder: desired sheets that exist, then any extras not in the desired list
  new_order <- c()
  for (s in desired_order) {
    idx <- which(current_sheets == s)
    if (length(idx) > 0) new_order <- c(new_order, idx[1])
  }
  extras <- setdiff(seq_along(current_sheets), new_order)
  new_order <- c(new_order, extras)
  
  worksheetOrder(wb) <- new_order
  
  # Save
  
  if (verbose) message("[audit wb] Saving ", length(current_sheets), " sheets to ", output_path)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  if (verbose) message("[audit wb] Done")
  
  invisible(output_path)
}