# generate word output from uploaded excel files (no database needed)

suppressPackageStartupMessages({
  library(readxl)
})

source("utils/word_helpers.R", local = FALSE)

.read_oecd_latest <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  ext <- tolower(tools::file_ext(path))
  
  # prefer the wide "Table" sheet if it exists
  if (ext %in% c("xlsx", "xls")) {
    sheets    <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    tbl_sheet <- if ("Table" %in% sheets) "Table" else NULL
    
    if (!is.null(tbl_sheet)) {
      raw <- suppressMessages(readxl::read_excel(path, sheet = tbl_sheet, col_names = FALSE))
      
      # find header row by looking for 3+ period-like strings
      header_row <- NULL
      for (ri in 1:min(15, nrow(raw))) {
        row_text    <- as.character(unlist(raw[ri, ]))
        period_hits <- sum(grepl("(Q[1-4]|20[0-9]{2}|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)", row_text, ignore.case = TRUE))
        if (period_hits >= 3) { header_row <- ri; break }
      }
      if (is.null(header_row)) return(NULL)
      
      headers   <- as.character(unlist(raw[header_row, ]))
      data_rows <- raw[(header_row + 1):nrow(raw), , drop = FALSE]
      results   <- data.frame(country = character(), period = character(), value = numeric(), stringsAsFactors = FALSE)
      
      for (target in names(.oecd_country_aliases)) {
        aliases <- .oecd_country_aliases[[target]]
        for (ri in seq_len(nrow(data_rows))) {
          if (tolower(trimws(as.character(data_rows[[1]][ri]))) %in% tolower(aliases)) {
            for (ci in ncol(data_rows):2) {
              val <- suppressWarnings(as.numeric(as.character(data_rows[[ci]][ri])))
              if (!is.na(val)) {
                results <- rbind(results, data.frame(country = target, period = trimws(headers[ci]), value = val, stringsAsFactors = FALSE))
                break
              }
            }
            break
          }
        }
      }
      if (nrow(results) > 0) return(results)
    }
    
    # fallback to sdmx long format
    if (length(sheets) > 0) return(.parse_oecd_sdmx(suppressMessages(readxl::read_excel(path, sheet = sheets[1], n_max = 5000))))
  }
  
  # csv is always sdmx long format
  if (ext == "csv") {
    raw <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
    if (nrow(raw) > 0) return(.parse_oecd_sdmx(raw))
  }
  
  NULL
}

.parse_oecd_sdmx <- function(df) {
  # prefer code columns — label columns can be empty in filtered SDMX exports
  ref_col  <- intersect(c("REF_AREA", "Reference.area"), names(df))[1]
  time_col <- intersect(c("TIME_PERIOD", "Time.period"), names(df))[1]
  val_col  <- intersect(c("OBS_VALUE", "Observation.value"), names(df))[1]
  
  if (is.na(ref_col) || is.na(time_col) || is.na(val_col)) return(NULL)
  
  results <- data.frame(country = character(), period = character(),
                        value = numeric(), stringsAsFactors = FALSE)
  
  for (target in names(.oecd_country_aliases)) {
    aliases <- tolower(.oecd_country_aliases[[target]])
    rows <- df[tolower(trimws(df[[ref_col]])) %in% aliases, , drop = FALSE]
    if (nrow(rows) == 0) next
    rows <- rows[order(rows[[time_col]], decreasing = TRUE), ]
    # skip rows with NA values to fall back to next available quarter
    for (ri in seq_len(nrow(rows))) {
      val <- suppressWarnings(as.numeric(rows[[val_col]][ri]))
      if (!is.na(val)) {
        results <- rbind(results, data.frame(
          country = target, period = trimws(rows[[time_col]][ri]), value = val,
          stringsAsFactors = FALSE
        ))
        break
      }
    }
  }
  if (nrow(results) > 0) return(results) else NULL
}

generate_manual_word_output <- function(
    manual_month = NULL,
    file_a01  = NULL,
    file_x09  = NULL,
    file_rtisa = NULL,
    file_hr1  = NULL,
    file_oecd_unemp = NULL,
    file_oecd_emp   = NULL,
    file_oecd_inact = NULL,
    vac_end_override = NULL,
    payroll_end_override = NULL,
    summary_override = NULL,
    top_ten_override = NULL,
    template_path = "utils/ManualDB.docx",
    output_path   = "utils/ManualDBoutput.docx",
    contact_names = NULL,
    verbose = TRUE
) {
  
  if (!is.null(manual_month)) manual_month <- tolower(manual_month)
  
  source("utils/helpers.R", local = FALSE)
  source("utils/calculations_from_excel.R", local = FALSE)
  manual_month <- run_calculations_from_excel(manual_month,
                                              file_a01 = file_a01, file_hr1 = file_hr1,
                                              file_x09 = file_x09, file_rtisa = file_rtisa,
                                              vac_end_override = vac_end_override,
                                              payroll_end_override = payroll_end_override)
  
  if (verbose) message("[manual] calculations complete for ", manual_month)
  
  fallback_lines <- function() { stats <- list(); for (i in 1:10) stats[[paste0("line", i)]] <- ""; stats }
  
  if (!is.null(summary_override) && !is.null(top_ten_override)) {
    summary <- summary_override
    top10   <- top_ten_override
  } else {
    source("sheets/summary.R", local = FALSE)
    source("sheets/top_ten_stats.R", local = FALSE)
    summary <- tryCatch(generate_summary(),  error = function(e) { if (verbose) warning("generate_summary() failed: ", e$message);  fallback_lines() })
    top10   <- tryCatch(generate_top_ten(),  error = function(e) { if (verbose) warning("generate_top_ten() failed: ", e$message);  fallback_lines() })
  }
  
  doc <- read_docx(template_path)
  
  contact <- if (!is.null(contact_names) && nzchar(contact_names)) contact_names else ""
  doc <- replace_all(doc, "qvzcontact", contact)
  
  doc <- replace_all(doc, "qvzmonthlabel", manual_month_to_label(manual_month))
  doc <- replace_all(doc, "qvzrenderdate", format(Sys.Date(), "%d %B %Y"))
  doc <- replace_all(doc, "qvzlfsperiod",  sv("lfs_period_label", ""))
  doc <- replace_all(doc, "qvzlfsquarter", sv("lfs_period_short_label", ""))
  doc <- replace_all(doc, "qvzvacquarter", sv("vacancies_period_short_label", ""))
  doc <- replace_all(doc, "qvzvacperiod",  sv("vacancies_period_short_label", ""))
  doc <- replace_all(doc, "qozpayperiod",  sv("payroll_period_short_label", ""))
  
  for (i in 1:10) doc <- replace_all(doc, sprintf("qvzsl%02d", i), summary[[paste0("line", i)]])
  for (i in 1:10) doc <- replace_all(doc, sprintf("qvztt%02d", i), top10[[paste0("line", i)]])
  
  # current column
  doc <- replace_all(doc, "qvzempcur",  fmt_count_000s_current(sv("emp16_cur")))
  doc <- replace_all(doc, "qvzertcur",  .format_pct(sv("emp_rt_cur")))
  doc <- replace_all(doc, "qvzunecur",  fmt_count_000s_current(sv("unemp16_cur")))
  doc <- replace_all(doc, "qvzurtcur",  .format_pct(sv("unemp_rt_cur")))
  doc <- replace_all(doc, "qvzinacur",  fmt_count_000s_current(sv("inact_cur")))
  doc <- replace_all(doc, "qvzifecur",  fmt_count_000s_current(sv("inact5064_cur")))
  doc <- replace_all(doc, "qvzirtcur",  .format_pct(sv("inact_rt_cur")))
  doc <- replace_all(doc, "qvzifrcur",  .format_pct(sv("inact5064_rt_cur")))
  doc <- replace_all(doc, "qvzpaycur",  fmt_exempt_current(sv("payroll_cur")))
  doc <- fill_conditional(doc, "qvzvaccur", fmt_exempt_current(sv("vac_cur")), 0, neutral = TRUE)
  doc <- replace_all(doc, "qvzwnocur",  .format_pct(sv("latest_wages")))
  doc <- replace_all(doc, "qvzwcpcur",  .format_pct(sv("latest_wages_cpi")))
  
  # change on quarter
  doc <- fill_conditional(doc, "qvzempdq",  fmt_count_000s_change(sv("emp16_dq")),        sv("emp16_dq"))
  doc <- fill_conditional(doc, "qvzertdq",  .format_pp(sv("emp_rt_dq")),                  sv("emp_rt_dq"))
  doc <- fill_conditional(doc, "qvzunedq",  fmt_count_000s_change(sv("unemp16_dq")),      sv("unemp16_dq"),      invert = TRUE)
  doc <- fill_conditional(doc, "qvzurtdq",  .format_pp(sv("unemp_rt_dq")),                sv("unemp_rt_dq"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzinadq",  fmt_count_000s_change(sv("inact_dq")),         sv("inact_dq"),        invert = TRUE)
  doc <- fill_conditional(doc, "qvzifedq",  fmt_count_000s_change(sv("inact5064_dq")),     sv("inact5064_dq"),    invert = TRUE)
  doc <- fill_conditional(doc, "qvzirtdq",  .format_pp(sv("inact_rt_dq")),                 sv("inact_rt_dq"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzifrdq",  .format_pp(sv("inact5064_rt_dq")),             sv("inact5064_rt_dq"), invert = TRUE)
  doc <- fill_conditional(doc, "qvzpaydq",  fmt_exempt_change(sv("payroll_dq")),            sv("payroll_dq"))
  doc <- fill_conditional(doc, "qvzvacdq",  fmt_exempt_change(sv("vac_dq")),               0, neutral = TRUE)
  doc <- fill_conditional(doc, "qvzwnodq",  .format_gbp_signed0(sv("wages_change_q")),     sv("wages_change_q"))
  doc <- fill_conditional(doc, "qvzwcpdq",  .format_gbp_signed0(sv("wages_cpi_change_q")), sv("wages_cpi_change_q"))
  
  # change on year
  doc <- fill_conditional(doc, "qvzempdy",  fmt_count_000s_change(sv("emp16_dy")),        sv("emp16_dy"))
  doc <- fill_conditional(doc, "qvzertdy",  .format_pp(sv("emp_rt_dy")),                  sv("emp_rt_dy"))
  doc <- fill_conditional(doc, "qvzunedy",  fmt_count_000s_change(sv("unemp16_dy")),      sv("unemp16_dy"),      invert = TRUE)
  doc <- fill_conditional(doc, "qvzurtdy",  .format_pp(sv("unemp_rt_dy")),                sv("unemp_rt_dy"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzinady",  fmt_count_000s_change(sv("inact_dy")),         sv("inact_dy"),        invert = TRUE)
  doc <- fill_conditional(doc, "qvzifedy",  fmt_count_000s_change(sv("inact5064_dy")),     sv("inact5064_dy"),    invert = TRUE)
  doc <- fill_conditional(doc, "qvzirtdy",  .format_pp(sv("inact_rt_dy")),                 sv("inact_rt_dy"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzifrdy",  .format_pp(sv("inact5064_rt_dy")),             sv("inact5064_rt_dy"), invert = TRUE)
  doc <- fill_conditional(doc, "qvzpaydy",  fmt_exempt_change(sv("payroll_dy")),            sv("payroll_dy"))
  doc <- fill_conditional(doc, "qvzvacdy",  fmt_exempt_change(sv("vac_dy")),               0, neutral = TRUE)
  doc <- fill_conditional(doc, "qvzwnody",  .format_gbp_signed0(sv("wages_change_y")),     sv("wages_change_y"))
  doc <- fill_conditional(doc, "qvzwcpdy",  .format_gbp_signed0(sv("wages_cpi_change_y")), sv("wages_cpi_change_y"))
  
  # change since covid
  doc <- fill_conditional(doc, "qvzempdc",  fmt_count_000s_change(sv("emp16_dc")),        sv("emp16_dc"))
  doc <- fill_conditional(doc, "qvzertdc",  .format_pp(sv("emp_rt_dc")),                  sv("emp_rt_dc"))
  doc <- fill_conditional(doc, "qvzunedc",  fmt_count_000s_change(sv("unemp16_dc")),      sv("unemp16_dc"),      invert = TRUE)
  doc <- fill_conditional(doc, "qvzurtdc",  .format_pp(sv("unemp_rt_dc")),                sv("unemp_rt_dc"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzinadc",  fmt_count_000s_change(sv("inact_dc")),         sv("inact_dc"),        invert = TRUE)
  doc <- fill_conditional(doc, "qvzifedc",  fmt_count_000s_change(sv("inact5064_dc")),     sv("inact5064_dc"),    invert = TRUE)
  doc <- fill_conditional(doc, "qvzirtdc",  .format_pp(sv("inact_rt_dc")),                 sv("inact_rt_dc"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzifrdc",  .format_pp(sv("inact5064_rt_dc")),             sv("inact5064_rt_dc"), invert = TRUE)
  doc <- fill_conditional(doc, "qvzpaydc",  fmt_exempt_change(sv("payroll_dc")),            sv("payroll_dc"))
  doc <- fill_conditional(doc, "qvzvacdc",  fmt_exempt_change(sv("vac_dc")),               0, neutral = TRUE)
  doc <- fill_conditional(doc, "qvzwnodc",  .format_gbp_signed0(sv("wages_change_covid")),     sv("wages_change_covid"))
  doc <- fill_conditional(doc, "qvzwcpdc",  .format_gbp_signed0(sv("wages_cpi_change_covid")), sv("wages_cpi_change_covid"))
  
  # change since 2024 election
  doc <- fill_conditional(doc, "qvzempde",  fmt_count_000s_change(sv("emp16_de")),        sv("emp16_de"))
  doc <- fill_conditional(doc, "qvzertde",  .format_pp(sv("emp_rt_de")),                  sv("emp_rt_de"))
  doc <- fill_conditional(doc, "qvzunede",  fmt_count_000s_change(sv("unemp16_de")),      sv("unemp16_de"),      invert = TRUE)
  doc <- fill_conditional(doc, "qvzurtde",  .format_pp(sv("unemp_rt_de")),                sv("unemp_rt_de"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzinade",  fmt_count_000s_change(sv("inact_de")),         sv("inact_de"),        invert = TRUE)
  doc <- fill_conditional(doc, "qvzifede",  fmt_count_000s_change(sv("inact5064_de")),     sv("inact5064_de"),    invert = TRUE)
  doc <- fill_conditional(doc, "qvzirtde",  .format_pp(sv("inact_rt_de")),                 sv("inact_rt_de"),     invert = TRUE)
  doc <- fill_conditional(doc, "qvzifrde",  .format_pp(sv("inact5064_rt_de")),             sv("inact5064_rt_de"), invert = TRUE)
  doc <- fill_conditional(doc, "qvzpayde",  fmt_exempt_change(sv("payroll_de")),            sv("payroll_de"))
  doc <- fill_conditional(doc, "qvzvacde",  fmt_exempt_change(sv("vac_de")),               0, neutral = TRUE)
  doc <- fill_conditional(doc, "qvzwnode",  .format_gbp_signed0(sv("wages_change_election")),     sv("wages_change_election"))
  doc <- fill_conditional(doc, "qvzwcpde",  .format_gbp_signed0(sv("wages_cpi_change_election")), sv("wages_cpi_change_election"))
  
  # file_oecd_* may be a file path (uploaded) or a pre-fetched data.frame
  # (auto-fetched from db). Data frames pass through unchanged.
  .coerce <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    .read_oecd_latest(x)
  }
  oecd_unemp_data <- tryCatch(.coerce(file_oecd_unemp), error = function(e) NULL)
  oecd_emp_data   <- tryCatch(.coerce(file_oecd_emp),   error = function(e) NULL)
  oecd_inact_data <- tryCatch(.coerce(file_oecd_inact), error = function(e) NULL)
  
  if (!is.null(oecd_unemp_data) || !is.null(oecd_emp_data) || !is.null(oecd_inact_data)) {
    doc <- .fill_oecd_placeholders(doc, oecd_unemp_data, oecd_emp_data, oecd_inact_data)
    if (verbose) message("[manual] OECD comparison table populated")
  }
  
  # replace any unfilled qvz placeholders with em-dash
  body_xml   <- doc$doc_obj$get()
  ns         <- xml2::xml_ns(body_xml)
  text_nodes <- xml2::xml_find_all(body_xml, ".//w:t", ns = ns)
  for (node in text_nodes) {
    txt <- xml2::xml_text(node)
    if (grepl("qvz", txt, fixed = TRUE)) {
      cleaned <- gsub("qvz[a-z0-9_]+", "\u2014", txt)
      xml2::xml_text(node) <- cleaned
    }
  }
  
  print(doc, target = output_path)
  if (verbose) message("[manual] written to ", output_path)
  invisible(output_path)
}