# reads ons excel files and populates dashboard variables (no database needed)

if (!exists("parse_manual_month", inherits = TRUE)) source("utils/helpers.R")
if (!exists("COVID_DATE",         inherits = TRUE)) source("utils/config.R")
source("utils/excel_helpers.R", local = FALSE)

# returns empty data.frame on failure so callers can check nrow
.read_sheet <- function(path, sheet) {
  tryCatch(
    suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE)),
    error = function(e) {
      warning("failed to read sheet '", sheet, "' from ", basename(path), ": ", e$message)
      data.frame()
    }
  )
}

.safe_last <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[length(x)]
}

# find column by ons dataset code (e.g. "KAC9") in the header rows
.find_col_by_code <- function(tbl, code, fallback_col = NA_integer_, search_rows = 1:min(10, nrow(tbl))) {
  if (nrow(tbl) == 0 || ncol(tbl) == 0) return(fallback_col)
  for (r in search_rows) {
    for (c in seq_len(ncol(tbl))) {
      cell <- as.character(tbl[[c]][r])
      if (!is.na(cell) && grepl(code, cell, fixed = TRUE)) return(c)
    }
  }
  fallback_col
}

# infer reference month from the last lfs period label in a01 sheet 1
.detect_manual_month_from_a01 <- function(file_a01) {
  if (is.null(file_a01)) return(NULL)
  tbl <- .read_sheet(file_a01, "1")
  if (nrow(tbl) == 0 || ncol(tbl) == 0) return(NULL)
  
  col1 <- trimws(as.character(tbl[[1]]))
  lfs_pat <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+(\\d{4})$"
  hits <- grep(lfs_pat, col1, ignore.case = TRUE)
  if (length(hits) == 0) return(NULL)
  
  last_label <- col1[hits[length(hits)]]
  parts <- regmatches(last_label, regexec(lfs_pat, last_label, ignore.case = TRUE))[[1]]
  end_mon <- match(tools::toTitleCase(tolower(parts[3])), month.abb)
  end_yr  <- as.integer(parts[4])
  if (is.na(end_mon) || is.na(end_yr)) return(NULL)
  
  lfs_end <- as.Date(sprintf("%04d-%02d-01", end_yr, end_mon))
  cm_date <- lfs_end %m+% months(2)
  tolower(paste0(format(cm_date, "%b"), format(cm_date, "%Y")))
}


run_calculations_from_excel <- function(manual_month = NULL,
                                        file_a01 = NULL,
                                        file_hr1 = NULL,
                                        file_x09 = NULL,
                                        file_rtisa = NULL,
                                        vac_end_override = NULL,
                                        payroll_end_override = NULL,
                                        target_env = globalenv()) {
  
  if (is.null(manual_month)) {
    manual_month <- .detect_manual_month_from_a01(file_a01)
    if (is.null(manual_month)) {
      stop("cannot detect reference period from a01 file — check sheet '1' has lfs period labels", call. = FALSE)
    }
  }

  cm       <- parse_manual_month(manual_month)
  anchor_m <- cm %m-% months(2)

  # lfs end dates for each comparison window
  lfs_end_cur   <- anchor_m
  lfs_end_q     <- anchor_m %m-% months(3)
  lfs_end_y     <- anchor_m %m-% months(12)
  lfs_end_covid <- COVID_DATE
  lfs_end_elec  <- ELEC24_DATE

  lab_cur   <- .lfs_label(lfs_end_cur)
  lab_q     <- .lfs_label(lfs_end_q)
  lab_y     <- .lfs_label(lfs_end_y)
  lab_covid <- .lfs_label(lfs_end_covid)
  lab_elec  <- .lfs_label(lfs_end_elec)
  
  
  # a01 sheet 1 column mapping: D=emp level, E=unemp level, I=unemp rate, O=inact level, Q=emp rate, S=inact rate
  tbl_1 <- if (!is.null(file_a01)) .read_sheet(file_a01, "1") else data.frame()
  
  .lfs_metric <- function(tbl, col, labels) {
    rows <- vapply(labels, function(l) .find_row(tbl, l), integer(1))
    vals <- vapply(seq_along(rows), function(i) .cell_num(tbl, rows[i], col), numeric(1))
    names(vals) <- c("cur", "q", "y", "covid", "elec")
    list(
      cur = vals["cur"],
      dq  = vals["cur"] - vals["q"],
      dy  = vals["cur"] - vals["y"],
      dc  = vals["cur"] - vals["covid"],
      de  = vals["cur"] - vals["elec"]
    )
  }
  
  all_labels <- c(lab_cur, lab_q, lab_y, lab_covid, lab_elec)
  
  m_emp16   <- .lfs_metric(tbl_1, 4,  all_labels)   # col D: employment 16+ level
  m_emprt   <- .lfs_metric(tbl_1, 17, all_labels)   # col Q: employment rate 16-64
  m_unemp16 <- .lfs_metric(tbl_1, 5,  all_labels)   # col E: unemployment 16+ level
  m_unemprt <- .lfs_metric(tbl_1, 9,  all_labels)   # col I: unemployment rate 16+
  m_inact   <- .lfs_metric(tbl_1, 15, all_labels)   # col O: inactivity level 16-64
  m_inactrt <- .lfs_metric(tbl_1, 19, all_labels)   # col S: inactivity rate 16-64
  
  for (prefix in c("emp16", "emp_rt", "unemp16", "unemp_rt", "inact", "inact_rt")) {
    m <- switch(prefix,
                emp16 = m_emp16, emp_rt = m_emprt,
                unemp16 = m_unemp16, unemp_rt = m_unemprt,
                inact = m_inact, inact_rt = m_inactrt
    )
    assign(paste0(prefix, "_cur"), m$cur, envir = target_env)
    assign(paste0(prefix, "_dq"),  m$dq,  envir = target_env)
    assign(paste0(prefix, "_dy"),  m$dy,  envir = target_env)
    assign(paste0(prefix, "_dc"),  m$dc,  envir = target_env)
    assign(paste0(prefix, "_de"),  m$de,  envir = target_env)
  }
  
  # a01 sheet 2: col BD(56)=inact 50-64 level, col BE(57)=inact 50-64 rate
  tbl_2 <- if (!is.null(file_a01)) .read_sheet(file_a01, "2") else data.frame()

  m_5064   <- .lfs_metric(tbl_2, 56, all_labels)
  m_5064rt <- .lfs_metric(tbl_2, 57, all_labels)
  
  for (prefix in c("inact5064", "inact5064_rt")) {
    m <- if (prefix == "inact5064") m_5064 else m_5064rt
    assign(paste0(prefix, "_cur"), m$cur, envir = target_env)
    assign(paste0(prefix, "_dq"),  m$dq,  envir = target_env)
    assign(paste0(prefix, "_dy"),  m$dy,  envir = target_env)
    assign(paste0(prefix, "_dc"),  m$dc,  envir = target_env)
    assign(paste0(prefix, "_de"),  m$de,  envir = target_env)
  }
  
  # a01 sheet 10: col B(2)=redundancy level, col C(3)=rate per 1000
  tbl_10 <- if (!is.null(file_a01)) .read_sheet(file_a01, "10") else data.frame()

  m_redund <- .lfs_metric(tbl_10, 3, all_labels)
  m_redund_level <- .lfs_metric(tbl_10, 2, all_labels)
  
  assign("redund_cur", m_redund$cur, envir = target_env)
  assign("redund_dq",  m_redund$dq,  envir = target_env)
  assign("redund_dy",  m_redund$dy,  envir = target_env)
  assign("redund_dc",  m_redund$dc,  envir = target_env)
  assign("redund_de",  m_redund$de,  envir = target_env)
  
  
  # a01 sheet 13: awe total pay - col B(2)=weekly gbp, col D(4)=3m avg % yoy
  tbl_13 <- if (!is.null(file_a01)) .read_sheet(file_a01, "13") else data.frame()
  
  if (nrow(tbl_13) > 0 && ncol(tbl_13) >= 4) {
    w13_dates <- .detect_dates(tbl_13[[1]])
    w13_weekly <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[2]]))))
    w13_pct    <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[4]]))))
    
    latest_wages <- .val_by_date(w13_dates, w13_pct, anchor_m)
    
    win3      <- seq(anchor_m %m-% months(2), by = "month", length.out = 3)
    prev3     <- seq(anchor_m %m-% months(5), by = "month", length.out = 3)
    yago3     <- win3 %m-% months(12)
    covid3    <- seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3)
    election3 <- seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3)
    
    .wage_change <- function(a_months, b_months) {
      a <- .avg_by_dates(w13_dates, w13_weekly, a_months)
      b <- .avg_by_dates(w13_dates, w13_weekly, b_months)
      if (is.na(a) || is.na(b)) NA_real_ else (a - b) * 52
    }
    
    wages_change_q <- .wage_change(win3, prev3)
    wages_change_y <- .wage_change(win3, yago3)
    wages_change_covid <- .wage_change(win3, covid3)
    wages_change_election <- .wage_change(win3, election3)
    
    prev_q_pct <- .val_by_date(w13_dates, w13_pct, anchor_m %m-% months(3))
    wages_total_qchange <- if (!is.na(latest_wages) && !is.na(prev_q_pct)) latest_wages - prev_q_pct else NA_real_
  } else {
    latest_wages <- wages_change_q <- wages_change_y <- NA_real_
    wages_change_covid <- wages_change_election <- wages_total_qchange <- NA_real_
    win3 <- c(anchor_m, anchor_m %m-% months(1), anchor_m %m-% months(2))
  }
  
  # a01 sheet 15: awe regular pay - col D(4)=3m avg % yoy
  tbl_15 <- if (!is.null(file_a01)) .read_sheet(file_a01, "15") else data.frame()
  
  if (nrow(tbl_15) > 0 && ncol(tbl_15) >= 4) {
    w15_dates <- .detect_dates(tbl_15[[1]])
    w15_pct   <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_15[[4]]))))
    
    latest_regular_cash <- .val_by_date(w15_dates, w15_pct, anchor_m)
    
    prev_q_reg <- .val_by_date(w15_dates, w15_pct, anchor_m %m-% months(3))
    wages_reg_qchange <- if (!is.na(latest_regular_cash) && !is.na(prev_q_reg)) latest_regular_cash - prev_q_reg else NA_real_
  } else {
    latest_regular_cash <- NA_real_
    wages_reg_qchange <- NA_real_
  }
  
  # public/private breakdown
  if (nrow(tbl_13) > 0 && ncol(tbl_13) >= 10 && exists("w13_dates")) {
    col_pub_total  <- .find_col_by_code(tbl_13, "KAC9", fallback_col = 10L)
    col_priv_total <- .find_col_by_code(tbl_13, "KAC6", fallback_col = 7L)
    
    w13_pub_pct  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[col_pub_total]]))))
    w13_priv_pct <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_13[[col_priv_total]]))))
    
    wages_total_public  <- .val_by_date(w13_dates, w13_pub_pct, anchor_m)
    wages_total_private <- .val_by_date(w13_dates, w13_priv_pct, anchor_m)
  } else {
    wages_total_public  <- NA_real_
    wages_total_private <- NA_real_
  }
  
  if (nrow(tbl_15) > 0 && ncol(tbl_15) >= 10 && exists("w15_dates")) {
    col_pub_reg  <- .find_col_by_code(tbl_15, "KAJ7", fallback_col = 10L)
    col_priv_reg <- .find_col_by_code(tbl_15, "KAJ4", fallback_col = 7L)
    
    w15_pub_pct  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_15[[col_pub_reg]]))))
    w15_priv_pct <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_15[[col_priv_reg]]))))
    
    wages_reg_public  <- .val_by_date(w15_dates, w15_pub_pct, anchor_m)
    wages_reg_private <- .val_by_date(w15_dates, w15_priv_pct, anchor_m)
  } else {
    wages_reg_public  <- NA_real_
    wages_reg_private <- NA_real_
  }
  
  assign("latest_wages",          latest_wages,          envir = target_env)
  assign("wages_change_q",        wages_change_q,        envir = target_env)
  assign("wages_change_y",        wages_change_y,        envir = target_env)
  assign("wages_change_covid",    wages_change_covid,    envir = target_env)
  assign("wages_change_election", wages_change_election, envir = target_env)
  assign("wages_total_public",    wages_total_public,    envir = target_env)
  assign("wages_total_private",   wages_total_private,   envir = target_env)
  assign("wages_total_qchange",   wages_total_qchange,   envir = target_env)
  assign("latest_regular_cash",   latest_regular_cash,   envir = target_env)
  assign("wages_reg_public",      wages_reg_public,      envir = target_env)
  assign("wages_reg_private",     wages_reg_private,     envir = target_env)
  assign("wages_reg_qchange",     wages_reg_qchange,     envir = target_env)

  # x09 "AWE Real_CPI" sheet: col B(2)=real awe gbp, col E(5)=total % yoy, col I(9)=regular % yoy
  tbl_cpi <- if (!is.null(file_x09)) .read_sheet(file_x09, "AWE Real_CPI") else data.frame()
  
  if (nrow(tbl_cpi) > 0 && ncol(tbl_cpi) >= 9) {
    cpi_months <- .detect_dates(tbl_cpi[[1]])
    cpi_real   <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_cpi[[2]]))))
    cpi_total  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_cpi[[5]]))))
    cpi_reg    <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_cpi[[9]]))))
    
    cpi_valid  <- which(!is.na(cpi_months) & !is.na(cpi_total))
    cpi_anchor <- if (length(cpi_valid) > 0) cpi_months[cpi_valid[length(cpi_valid)]] else anchor_m

    latest_wages_cpi   <- .val_by_date(cpi_months, cpi_total, cpi_anchor)
    latest_regular_cpi <- .val_by_date(cpi_months, cpi_reg, cpi_anchor)

    .cpi_change <- function(a_months, b_months) {
      a <- .avg_by_dates(cpi_months, cpi_real, a_months)
      b <- .avg_by_dates(cpi_months, cpi_real, b_months)
      if (is.na(a) || is.na(b)) NA_real_ else (a - b) * 52
    }

    cpi_win3      <- seq(cpi_anchor %m-% months(2), by = "month", length.out = 3)
    prev3_cpi     <- seq(cpi_anchor %m-% months(5), by = "month", length.out = 3)
    yago3_cpi     <- cpi_win3 %m-% months(12)
    covid3_cpi    <- seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3)
    election3_cpi <- seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3)

    wages_cpi_change_q        <- .cpi_change(cpi_win3, prev3_cpi)
    wages_cpi_change_y        <- .cpi_change(cpi_win3, yago3_cpi)
    wages_cpi_change_covid    <- .cpi_change(cpi_win3, covid3_cpi)
    wages_cpi_change_election <- .cpi_change(cpi_win3, election3_cpi)

    dec2007_val  <- .val_by_date(cpi_months, cpi_real, as.Date("2007-12-01"))
    cur_cpi_real <- .avg_by_dates(cpi_months, cpi_real, cpi_win3)
    wages_cpi_total_vs_dec2007 <- if (!is.na(cur_cpi_real) && !is.na(dec2007_val) && dec2007_val != 0) {
      ((cur_cpi_real - dec2007_val) / dec2007_val) * 100
    } else NA_real_

    pandemic3    <- covid3_cpi
    pandemic_avg <- .avg_by_dates(cpi_months, cpi_real, pandemic3)
    wages_cpi_total_vs_pandemic <- if (!is.na(cur_cpi_real) && !is.na(pandemic_avg) && pandemic_avg != 0) {
      ((cur_cpi_real - pandemic_avg) / pandemic_avg) * 100
    } else NA_real_
  } else {
    latest_wages_cpi <- latest_regular_cpi <- NA_real_
    wages_cpi_change_q <- wages_cpi_change_y <- wages_cpi_change_covid <- wages_cpi_change_election <- NA_real_
    wages_cpi_total_vs_dec2007 <- wages_cpi_total_vs_pandemic <- NA_real_
  }
  
  assign("latest_wages_cpi",           latest_wages_cpi,           envir = target_env)
  assign("latest_regular_cpi",         latest_regular_cpi,         envir = target_env)
  assign("wages_cpi_change_q",         wages_cpi_change_q,         envir = target_env)
  assign("wages_cpi_change_y",         wages_cpi_change_y,         envir = target_env)
  assign("wages_cpi_change_covid",     wages_cpi_change_covid,     envir = target_env)
  assign("wages_cpi_change_election",  wages_cpi_change_election,  envir = target_env)
  assign("wages_cpi_total_vs_dec2007", wages_cpi_total_vs_dec2007, envir = target_env)
  assign("wages_cpi_total_vs_pandemic", wages_cpi_total_vs_pandemic, envir = target_env)
  
  # a01 sheet 19: vacancies
  tbl_19 <- if (!is.null(file_a01)) .read_sheet(file_a01, "19") else data.frame()

  # override only comes from dashboard preview; otherwise we take the latest available
  vac_lab_covid <- "Jan-Mar 2020"
  vac_lab_elec  <- .lfs_label(ELEC24_DATE)

  if (nrow(tbl_19) > 0 && ncol(tbl_19) >= 3) {
    col1 <- trimws(as.character(tbl_19[[1]]))
    vac_dates <- vapply(col1, function(lbl) {
      mons <- regmatches(lbl, gregexpr("[A-Za-z]{3}", lbl))[[1]]
      yrs  <- regmatches(lbl, gregexpr("[0-9]{4}", lbl))[[1]]
      if (length(mons) < 2 || length(yrs) < 1) return(as.numeric(NA))
      end_m <- month_map[tolower(mons[2])]
      yr <- suppressWarnings(as.integer(yrs[1]))
      if (is.na(end_m) || is.na(yr)) return(as.numeric(NA))
      as.numeric(as.Date(sprintf("%04d-%02d-01", yr, end_m)))
    }, numeric(1))
    valid_vac <- which(!is.na(vac_dates) & !is.na(suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_19[[3]]))))))
    if (length(valid_vac) > 0) {
      if (!is.null(vac_end_override)) {
        override_idx <- valid_vac[which(as.Date(vac_dates[valid_vac], origin = "1970-01-01") == vac_end_override)]
        if (length(override_idx) >= 1) {
          vac_end <- vac_end_override
        } else {
          latest_idx <- valid_vac[which.max(vac_dates[valid_vac])]
          vac_end <- as.Date(vac_dates[latest_idx], origin = "1970-01-01")
        }
      } else {
        latest_idx <- valid_vac[which.max(vac_dates[valid_vac])]
        vac_end <- as.Date(vac_dates[latest_idx], origin = "1970-01-01")
      }
    } else {
      vac_end <- if (!is.null(vac_end_override)) vac_end_override else lfs_end_cur
    }
    
    vac_lab_cur <- .lfs_label(vac_end)
    vac_lab_q   <- .lfs_label(vac_end %m-% months(3))
    vac_lab_y   <- .lfs_label(vac_end %m-% months(12))
    
    r_cur   <- .find_row(tbl_19, vac_lab_cur)
    vac_cur <- .cell_num(tbl_19, r_cur, 3)
    vac_dq  <- vac_cur - .cell_num(tbl_19, .find_row(tbl_19, vac_lab_q), 3)
    vac_dy  <- vac_cur - .cell_num(tbl_19, .find_row(tbl_19, vac_lab_y), 3)
    vac_dc  <- vac_cur - .cell_num(tbl_19, .find_row(tbl_19, vac_lab_covid), 3)
    vac_de  <- vac_cur - .cell_num(tbl_19, .find_row(tbl_19, vac_lab_elec), 3)
  } else {
    vac_end <- if (!is.null(vac_end_override)) vac_end_override else lfs_end_cur
    vac_cur <- vac_dq <- vac_dy <- vac_dc <- vac_de <- NA_real_
  }
  
  assign("vac_cur", vac_cur, envir = target_env)
  assign("vac_dq",  vac_dq,  envir = target_env)
  assign("vac_dy",  vac_dy,  envir = target_env)
  assign("vac_dc",  vac_dc,  envir = target_env)
  assign("vac_de",  vac_de,  envir = target_env)
  assign("vac", list(cur = vac_cur, dq = vac_dq, dy = vac_dy,
                     dc = vac_dc, de = vac_de, end = vac_end), envir = target_env)
  
  # a01 sheet 18: days lost
  tbl_18 <- if (!is.null(file_a01)) .read_sheet(file_a01, "18") else data.frame()

  if (nrow(tbl_18) > 0 && ncol(tbl_18) >= 2) {
    # ons marks revisions with [r], [p], [x] which break date parsing
    dl_raw <- gsub("\\s*\\[.*?\\]\\s*", "", as.character(tbl_18[[1]]))
    dl_dates <- .detect_dates(dl_raw)
    dl_vals  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl_18[[2]]))))
    valid_idx <- which(!is.na(dl_dates) & !is.na(dl_vals))
    if (length(valid_idx) > 0) {
      last_idx <- valid_idx[length(valid_idx)]
      days_lost_cur   <- dl_vals[last_idx]
      days_lost_label <- format(dl_dates[last_idx], "%B %Y")
    } else {
      days_lost_cur <- NA_real_
      days_lost_label <- ""
    }
  } else {
    days_lost_cur <- NA_real_
    days_lost_label <- ""
  }
  
  assign("days_lost_cur",   days_lost_cur,   envir = target_env)
  assign("days_lost_label", days_lost_label, envir = target_env)
  
  
  rtisa_pay <- if (!is.null(file_rtisa)) {
    .read_sheet(file_rtisa, "1. Payrolled employees (UK)")
  } else data.frame()
  
  rtisa_latest <- anchor_m

  if (nrow(rtisa_pay) > 0 && ncol(rtisa_pay) >= 2) {
    rtisa_text   <- trimws(as.character(rtisa_pay[[1]]))
    rtisa_parsed <- suppressWarnings(lubridate::parse_date_time(rtisa_text, orders = c("B Y", "bY", "BY")))
    rtisa_months <- floor_date(as.Date(rtisa_parsed), "month")
    rtisa_vals   <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_pay[[2]]))))

    pay_df <- data.frame(m = rtisa_months, v = rtisa_vals, stringsAsFactors = FALSE)
    pay_df <- pay_df[!is.na(pay_df$m) & !is.na(pay_df$v), ]
    pay_df <- pay_df[order(pay_df$m), ]

    rtisa_latest <- if (!is.null(payroll_end_override) && payroll_end_override %in% pay_df$m) {
      payroll_end_override
    } else if (nrow(pay_df) > 0) {
      pay_df$m[nrow(pay_df)]
    } else anchor_m
    rtisa_cm <- rtisa_latest %m+% months(2)

    months_cur  <- seq(rtisa_cm %m-% months(4), by = "month", length.out = 3)
    months_prev <- seq(rtisa_cm %m-% months(7), by = "month", length.out = 3)
    months_yago <- months_cur %m-% months(12)
    
    pay_cur_raw <- .avg_by_dates(pay_df$m, pay_df$v, months_cur)
    pay_prev3   <- .avg_by_dates(pay_df$m, pay_df$v, months_prev)
    pay_yago3   <- .avg_by_dates(pay_df$m, pay_df$v, months_yago)
    
    payroll_cur <- if (!is.na(pay_cur_raw)) pay_cur_raw / 1000 else NA_real_
    payroll_dq  <- if (!is.na(pay_cur_raw) && !is.na(pay_prev3)) (pay_cur_raw - pay_prev3) / 1000 else NA_real_
    payroll_dy  <- if (!is.na(pay_cur_raw) && !is.na(pay_yago3)) (pay_cur_raw - pay_yago3) / 1000 else NA_real_
    
    covid_base <- .avg_by_dates(pay_df$m, pay_df$v, seq(COVID_DATE  %m-% months(2), by = "month", length.out = 3))
    payroll_dc <- if (!is.na(pay_cur_raw) && !is.na(covid_base)) (pay_cur_raw - covid_base) / 1000 else NA_real_

    elec_base  <- .avg_by_dates(pay_df$m, pay_df$v, seq(ELEC24_DATE %m-% months(2), by = "month", length.out = 3))
    payroll_de <- if (!is.na(payroll_cur) && !is.na(elec_base)) payroll_cur - (elec_base / 1000) else NA_real_

    # flash uses the absolute latest date regardless of any override
    flash_anchor <- pay_df$m[nrow(pay_df)]
    flash_val    <- .val_by_date(pay_df$m, pay_df$v, flash_anchor)
    flash_prev_m <- .val_by_date(pay_df$m, pay_df$v, flash_anchor %m-% months(1))
    flash_prev_y <- .val_by_date(pay_df$m, pay_df$v, flash_anchor %m-% months(12))
    flash_elec   <- .val_by_date(pay_df$m, pay_df$v, ELEC24_DATE)
    
    payroll_flash_cur <- if (!is.na(flash_val)) flash_val / 1e6 else NA_real_
    payroll_flash_dm  <- if (!is.na(flash_val) && !is.na(flash_prev_m)) (flash_val - flash_prev_m) / 1000 else NA_real_
    payroll_flash_dy  <- if (!is.na(flash_val) && !is.na(flash_prev_y)) (flash_val - flash_prev_y) / 1000 else NA_real_
    payroll_flash_de  <- if (!is.na(flash_val) && !is.na(flash_elec)) (flash_val - flash_elec) / 1000 else NA_real_
  } else {
    payroll_cur <- payroll_dq <- payroll_dy <- payroll_dc <- payroll_de <- NA_real_
    payroll_flash_cur <- payroll_flash_dm <- payroll_flash_dy <- payroll_flash_de <- NA_real_
    flash_anchor <- anchor_m
  }
  
  assign("payroll_cur", payroll_cur, envir = target_env)
  assign("payroll_dq",  payroll_dq,  envir = target_env)
  assign("payroll_dy",  payroll_dy,  envir = target_env)
  assign("payroll_dc",  payroll_dc,  envir = target_env)
  assign("payroll_de",  payroll_de,  envir = target_env)
  assign("payroll_flash_cur", payroll_flash_cur, envir = target_env)
  assign("payroll_flash_dm",  payroll_flash_dm,  envir = target_env)
  assign("payroll_flash_dy",  payroll_flash_dy,  envir = target_env)
  assign("payroll_flash_de",  payroll_flash_de,  envir = target_env)
  
  
  
  rtisa_sec <- if (!is.null(file_rtisa)) {
    .read_sheet(file_rtisa, "23. Employees (Industry)")
  } else data.frame()

  if (nrow(rtisa_sec) > 0 && ncol(rtisa_sec) >= 18) {
    sec_text   <- trimws(as.character(rtisa_sec[[1]]))
    sec_parsed <- suppressWarnings(lubridate::parse_date_time(sec_text, orders = c("B Y", "bY", "BY")))
    sec_months <- floor_date(as.Date(sec_parsed), "month")

    sec_retail <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_sec[[8]]))))
    sec_hosp   <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_sec[[10]]))))
    sec_health <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_sec[[18]]))))

    sec_valid  <- which(!is.na(sec_months) & !is.na(sec_retail))
    sec_anchor <- if (length(sec_valid) > 0) sec_months[sec_valid[length(sec_valid)]] else cm %m-% months(1)

    .sector_full <- function(vals) {
      now   <- .val_by_date(sec_months, vals, sec_anchor)
      prev  <- .val_by_date(sec_months, vals, sec_anchor %m-% months(1))
      yago  <- .val_by_date(sec_months, vals, sec_anchor %m-% months(12))
      covid <- .val_by_date(sec_months, vals, COVID_DATE)
      elec  <- .val_by_date(sec_months, vals, ELEC24_DATE)
      list(
        cur = if (!is.na(now)) now / 1000 else NA_real_,
        dm  = if (!is.na(now) && !is.na(prev)) (now - prev) / 1000 else NA_real_,
        dy  = if (!is.na(now) && !is.na(yago)) (now - yago) / 1000 else NA_real_,
        dc  = if (!is.na(now) && !is.na(covid)) (now - covid) / 1000 else NA_real_,
        de  = if (!is.na(now) && !is.na(elec)) (now - elec) / 1000 else NA_real_
      )
    }
    
    s_retail <- .sector_full(sec_retail)
    s_hosp   <- .sector_full(sec_hosp)
    s_health <- .sector_full(sec_health)
  } else {
    na_sector <- list(cur = NA_real_, dm = NA_real_, dy = NA_real_, dc = NA_real_, de = NA_real_)
    s_retail <- s_hosp <- s_health <- na_sector
  }
  
  for (prefix in c("hosp", "retail", "health")) {
    s <- switch(prefix, hosp = s_hosp, retail = s_retail, health = s_health)
    assign(paste0(prefix, "_cur"), s$cur, envir = target_env)
    assign(paste0(prefix, "_dm"),  s$dm,  envir = target_env)
    assign(paste0(prefix, "_dy"),  s$dy,  envir = target_env)
    assign(paste0(prefix, "_dc"),  s$dc,  envir = target_env)
    assign(paste0(prefix, "_de"),  s$de,  envir = target_env)
  }
  
  # hr1 sheet 1a: col M(13) = gb total
  hr1_tbl <- if (!is.null(file_hr1)) .read_sheet(file_hr1, "1a") else data.frame()

  if (nrow(hr1_tbl) > 0 && ncol(hr1_tbl) >= 13) {
    hr1_dates <- .detect_dates(hr1_tbl[[1]])
    hr1_vals  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(hr1_tbl[[13]]))))

    valid_hr1 <- which(!is.na(hr1_dates) & !is.na(hr1_vals))
    if (length(valid_hr1) > 0) {
      last_hr1 <- valid_hr1[length(valid_hr1)]
      hr1_cur  <- hr1_vals[last_hr1]
      hr1_month_label <- format(hr1_dates[last_hr1], "%B %Y")

      prev_hr1 <- if (length(valid_hr1) >= 2) hr1_vals[valid_hr1[length(valid_hr1) - 1]] else NA_real_
      hr1_dm   <- if (!is.na(hr1_cur) && !is.na(prev_hr1)) hr1_cur - prev_hr1 else NA_real_

      hr1_cur_date <- hr1_dates[last_hr1]
      hr1_yago  <- .val_by_date(hr1_dates, hr1_vals, hr1_cur_date %m-% months(12))
      hr1_covid <- .val_by_date(hr1_dates, hr1_vals, COVID_DATE)
      hr1_elec  <- .val_by_date(hr1_dates, hr1_vals, ELEC24_DATE)
      
      hr1_dy <- if (!is.na(hr1_cur) && !is.na(hr1_yago)) hr1_cur - hr1_yago else NA_real_
      hr1_dc <- if (!is.na(hr1_cur) && !is.na(hr1_covid)) hr1_cur - hr1_covid else NA_real_
      hr1_de <- if (!is.na(hr1_cur) && !is.na(hr1_elec)) hr1_cur - hr1_elec else NA_real_
    } else {
      hr1_cur <- NA_real_
      hr1_dm <- hr1_dy <- hr1_dc <- hr1_de <- NA_real_
      hr1_month_label <- ""
    }
  } else {
    hr1_cur <- NA_real_
    hr1_dm <- hr1_dy <- hr1_dc <- hr1_de <- NA_real_
    hr1_month_label <- ""
  }
  
  assign("hr1_cur", hr1_cur, envir = target_env)
  assign("hr1_dm",  hr1_dm,  envir = target_env)
  assign("hr1_dy",  hr1_dy,  envir = target_env)
  assign("hr1_dc",  hr1_dc,  envir = target_env)
  assign("hr1_de",  hr1_de,  envir = target_env)
  assign("hr1_month_label", hr1_month_label, envir = target_env)
  
  # period labels for the word template
  lfs_period_label       <- lfs_label_narrative(lfs_end_cur)
  lfs_period_short_label <- make_lfs_label(lfs_end_cur)
  vacancies_period_label <- lfs_label_narrative(vac_end)
  vacancies_period_short_label <- make_lfs_label(vac_end)
  payroll_flash_label_val <- format(flash_anchor, "%B %Y")
  sector_month_label <- format(sec_anchor, "%B %Y")
  
  assign("lfs_period_label",             lfs_period_label,             envir = target_env)
  assign("lfs_period_short_label",       lfs_period_short_label,       envir = target_env)
  assign("vacancies_period_label",       vacancies_period_label,       envir = target_env)
  assign("vacancies_period_short_label", vacancies_period_short_label, envir = target_env)
  assign("payroll_flash_label",          payroll_flash_label_val,      envir = target_env)
  assign("payroll_month_label",          format(rtisa_latest, "%B %Y"),    envir = target_env)
  assign("payroll_period_short_label",  make_lfs_label(rtisa_latest),     envir = target_env)
  assign("hr1_month_label",             hr1_month_label,              envir = target_env)
  assign("sector_month_label",          sector_month_label,           envir = target_env)
  assign("manual_month",                manual_month,                 envir = target_env)
  
  assign("inact_driver_text", "", envir = target_env)

  # payroll list
  assign("payroll", list(
    cur = payroll_cur, dq = payroll_dq, dy = payroll_dy,
    dc = payroll_dc, de = payroll_de,
    flash_cur = payroll_flash_cur, flash_dm = payroll_flash_dm,
    flash_dy = payroll_flash_dy, flash_de = payroll_flash_de,
    flash_anchor = flash_anchor, anchor = anchor_m
  ), envir = target_env)
  
  # wages nominal list
  assign("wages_nom", list(
    total = list(cur = latest_wages, dq = wages_change_q,
                 dy = wages_change_y, dc = wages_change_covid,
                 de = wages_change_election,
                 public = wages_total_public, private = wages_total_private,
                 qchange = wages_total_qchange),
    regular = list(cur = latest_regular_cash,
                   public = wages_reg_public, private = wages_reg_private,
                   qchange = wages_reg_qchange),
    anchor = anchor_m
  ), envir = target_env)
  
  invisible(manual_month)
}