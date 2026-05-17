# shared formatters, replacers, and oecd helpers for word output
# sourced by both word_output.R and manual_word_output.R

suppressPackageStartupMessages({
  library(officer)
  library(xml2)
  library(scales)
})

# -- formatters --
# fall back to local logic if helpers.R hasn't been sourced yet

fmt_one_dec <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  if (x == 0) return(format(0, nsmall = 1, trim = TRUE))
  for (d in 1:4) {
    vr <- round(x, d)
    if (vr != 0) return(format(vr, nsmall = d, trim = TRUE))
  }
  format(round(x, 4), nsmall = 4, trim = TRUE)
}

.format_int <- function(x) {
  if (exists("format_int_unsigned", inherits = TRUE)) return(get("format_int_unsigned", inherits = TRUE)(x))
  if (exists("format_int", inherits = TRUE)) return(get("format_int", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  scales::comma(round(x), accuracy = 1)
}

.format_pct <- function(x) {
  if (exists("format_pct", inherits = TRUE)) return(get("format_pct", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  paste0(fmt_one_dec(x), "%")
}

.format_pp <- function(x) {
  if (exists("format_pp", inherits = TRUE)) return(get("format_pp", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  sign <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(sign, fmt_one_dec(abs(x)), "pp")
}

.format_gbp_signed0 <- function(x) {
  if (exists("format_gbp_signed0", inherits = TRUE)) return(get("format_gbp_signed0", inherits = TRUE)(x))
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  sign <- if (x > 0) "+" else if (x < 0) "-" else ""
  paste0(sign, "\u00A3", scales::comma(round(abs(x)), accuracy = 1))
}

fmt_int_signed <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("")
  s <- scales::comma(abs(round(x)), accuracy = 1)
  if (x > 0) paste0("+", s) else if (x < 0) paste0("-", s) else "0"
}

# lfs counts are persons; we display in 000s
fmt_count_000s_current <- function(x) .format_int(x / 1000)
fmt_count_000s_change  <- function(x) fmt_int_signed(x / 1000)

# payroll/vacancies are already in 000s so no division needed
fmt_exempt_current <- function(x) .format_int(x)
fmt_exempt_change  <- function(x) fmt_int_signed(x)

# -- month label --
# e.g. "mar2026" or "2025-10" -> "March 2026"

manual_month_to_label <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  x <- tolower(as.character(x))
  if (grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    d <- as.Date(sprintf("%s-%s-01", parts[1], parts[2]))
    return(format(d, "%B %Y"))
  }
  if (grepl("^[a-z]{3}[0-9]{4}$", x)) {
    mon <- substr(x, 1, 3)
    yr <- substr(x, 4, 7)
    month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
    if (mon %in% names(month_map)) {
      d <- as.Date(sprintf("%s-%02d-01", yr, month_map[[mon]]))
      return(format(d, "%B %Y"))
    }
  }
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# -- docx replacement --
# replaces placeholders in body, headers, and footers

replace_all <- function(doc, key, val) {
  if (is.null(val) || length(val) == 0 || is.na(val)) val <- ""
  val <- as.character(val)

  body_xml <- doc$doc_obj$get()
  ns <- xml2::xml_ns(body_xml)
  text_nodes <- xml2::xml_find_all(body_xml, ".//w:t", ns = ns)
  for (node in text_nodes) {
    txt <- xml2::xml_text(node)
    if (grepl(key, txt, fixed = TRUE)) {
      new_txt <- gsub(key, val, txt, fixed = TRUE)
      xml2::xml_text(node) <- new_txt
      xml2::xml_attr(node, "xml:space") <- "preserve"
    }
  }

  doc <- tryCatch(headers_replace_all_text(doc, key, val, fixed = TRUE), error = function(e) doc)
  doc <- tryCatch(footers_replace_all_text(doc, key, val, fixed = TRUE), error = function(e) doc)
  doc
}

# picks the p/n/z placeholder variant based on sign of value_num
fill_conditional <- function(doc, base, value_text, value_num, invert = FALSE, neutral = FALSE) {
  value_num <- suppressWarnings(as.numeric(value_num))

  p <- n <- z <- ""

  if (is.na(value_num)) {
    z <- "\u2014"
  } else if (isTRUE(neutral)) {
    z <- value_text
  } else {
    if (value_num > 0) p <- value_text
    if (value_num < 0) n <- value_text
    if (value_num == 0) z <- value_text
    if (isTRUE(invert)) { tmp <- p; p <- n; n <- tmp }
  }

  doc <- replace_all(doc, paste0(base, "p"), p)
  doc <- replace_all(doc, paste0(base, "n"), n)
  doc <- replace_all(doc, paste0(base, "z"), z)
  doc
}

# safe global variable lookup with default
sv <- function(name, default = NA_real_) {
  if (exists(name, inherits = TRUE)) get(name, inherits = TRUE) else default
}

# -- oecd international comparison data --

.oecd_countries <- c(
  "United Kingdom", "United States", "France", "Germany",
  "Italy", "Spain", "Canada", "Japan", "Euro area"
)

.oecd_country_aliases <- list(
  "United Kingdom" = c("United Kingdom", "GBR"),
  "United States"  = c("United States", "USA"),
  "France"         = c("France", "FRA"),
  "Germany"        = c("Germany", "DEU"),
  "Italy"          = c("Italy", "ITA"),
  "Spain"          = c("Spain", "ESP"),
  "Canada"         = c("Canada", "CAN"),
  "Japan"          = c("Japan", "JPN"),
  "Euro area"      = c("Euro area", "Euro area (20 countries)",
                       "Euro area (19 countries)", "EA20", "EA19", "EA")
)

.g7_members <- c("United Kingdom", "United States", "France", "Germany",
                 "Italy", "Canada", "Japan")

# compute g7 average for one metric
.compute_g7_average <- function(data) {
  if (is.null(data)) return(list(period = "", value = NA_real_))
  g7 <- data[data$country %in% .g7_members, , drop = FALSE]
  if (nrow(g7) == 0) return(list(period = "", value = NA_real_))
  periods <- sort(unique(g7$period), decreasing = TRUE)
  list(period = periods[1], value = mean(g7$value, na.rm = TRUE))
}

# generate three oecd comparison bullet points
.generate_oecd_bullets <- function(unemp_data, emp_data, inact_data) {
  .val <- function(data, country) {
    if (is.null(data)) return(NA_real_)
    idx <- match(country, data$country)
    if (is.na(idx)) NA_real_ else data$value[idx]
  }

  .ordinal <- function(n) {
    if (n == 1) return("the")
    sfx <- c("st", "nd", "rd", rep("th", 17))
    paste0("the ", n, if (n <= 20) sfx[n] else "th")
  }

  .list_countries <- function(countries) {
    display <- c("United Kingdom" = "the UK", "United States" = "the US",
                 "France" = "France", "Germany" = "Germany", "Italy" = "Italy",
                 "Canada" = "Canada", "Japan" = "Japan")
    nms <- vapply(countries, function(n) if (n %in% names(display)) display[[n]] else n, "")
    if (length(nms) == 0) return("")
    if (length(nms) == 1) return(nms)
    paste0(paste(nms[-length(nms)], collapse = ", "), " and ", nms[length(nms)])
  }

  .pp <- function(x) paste0(fmt_one_dec(abs(x)), "ppts")

  uk_ur <- .val(unemp_data, "United Kingdom")
  uk_er <- .val(emp_data,   "United Kingdom")
  uk_ir <- .val(inact_data, "United Kingdom")
  ea_ur <- .val(unemp_data, "Euro area")
  ea_er <- .val(emp_data,   "Euro area")
  ea_ir <- .val(inact_data, "Euro area")
  g7_ur <- .compute_g7_average(unemp_data)$value
  g7_er <- .compute_g7_average(emp_data)$value
  g7_ir <- .compute_g7_average(inact_data)$value

  # bullet 1: unemployment (lower is better)
  b1 <- ""
  if (!is.na(uk_ur)) {
    g7_vals <- vapply(.g7_members, function(c) .val(unemp_data, c), 0.0)
    valid   <- .g7_members[!is.na(g7_vals)]
    vals    <- g7_vals[!is.na(g7_vals)]
    rank    <- sum(vals < uk_ur) + 1
    worse   <- valid[vals > uk_ur]
    b1 <- paste0("The UK has ", .ordinal(rank), " lowest unemployment rate in the G7")
    if (length(worse) > 0) b1 <- paste0(b1, ": lower than ", .list_countries(worse))
    b1 <- paste0(b1, ".")
    if (!is.na(ea_ur)) {
      diff_ea <- uk_ur - ea_ur
      dir <- if (diff_ea < 0) "below" else "above"
      b1 <- paste0(b1, " The UK unemployment rate is ", .pp(diff_ea), " ", dir,
                   " the Euro Area average*.")
    }
  }

  # bullet 2: employment (higher is better)
  b2 <- ""
  if (!is.na(uk_er)) {
    g7_vals <- vapply(.g7_members, function(c) .val(emp_data, c), 0.0)
    valid   <- .g7_members[!is.na(g7_vals)]
    vals    <- g7_vals[!is.na(g7_vals)]
    rank    <- sum(vals > uk_er) + 1
    worse   <- valid[vals < uk_er]
    b2 <- paste0("The UK has ", .ordinal(rank), " highest employment rate among the G7")
    if (length(worse) > 0) b2 <- paste0(b2, ": higher than ", .list_countries(worse))
    b2 <- paste0(b2, ".")
    if (!is.na(ea_er) && !is.na(g7_er)) {
      diff_ea <- uk_er - ea_er
      diff_g7 <- uk_er - g7_er
      dir_ea <- if (diff_ea > 0) "above" else "below"
      b2 <- paste0(b2, " The UK employment rate is ", .pp(diff_ea), " and ", .pp(diff_g7),
                   " ", dir_ea, " the Euro Area and G7 averages respectively*.")
    } else if (!is.na(ea_er)) {
      diff_ea <- uk_er - ea_er
      dir <- if (diff_ea > 0) "above" else "below"
      b2 <- paste0(b2, " The UK employment rate is ", .pp(diff_ea), " ", dir,
                   " the Euro Area average*.")
    }
  }

  # bullet 3: inactivity (lower is better)
  b3 <- ""
  if (!is.na(uk_ir)) {
    g7_vals <- vapply(.g7_members, function(c) .val(inact_data, c), 0.0)
    valid   <- .g7_members[!is.na(g7_vals)]
    vals    <- g7_vals[!is.na(g7_vals)]
    rank    <- sum(vals < uk_ir) + 1
    worse   <- valid[vals > uk_ir]
    b3 <- paste0("The UK has ", .ordinal(rank), " lowest inactivity rate among the G7")
    if (length(worse) > 0) b3 <- paste0(b3, ": lower than ", .list_countries(worse))
    b3 <- paste0(b3, ".")
    if (!is.na(ea_ir)) {
      diff_ea <- uk_ir - ea_ir
      dir <- if (diff_ea < 0) "below" else "above"
      b3 <- paste0(b3, " The UK inactivity rate is ", .pp(diff_ea), " ", dir,
                   " the Euro area average*.")
    }
  }

  list(bullet1 = b1, bullet2 = b2, bullet3 = b3)
}

# convert manual_month (e.g. "dec2025") to oecd quarter format (e.g. "2025-Q4")
.manual_month_to_quarter <- function(mm) {
  if (is.null(mm) || !nzchar(mm)) return("")
  mm <- tolower(gsub("[[:space:]]+", "", as.character(mm)))
  mon3 <- substr(gsub("[^a-z]", "", mm), 1, 3)
  yr <- as.integer(substr(gsub("[^0-9]", "", mm), 1, 4))
  m <- match(mon3, tolower(month.abb))
  if (is.na(m) || is.na(yr)) return("")
  q <- ceiling(m / 3)
  paste0(yr, "-Q", q)
}

# override oecd uk row with ons-derived rates when available
.get_uk_ons_overrides <- function() {
  ur <- sv("unemp_rt_cur")
  er <- sv("emp_rt_cur")
  ir <- sv("inact_rt_cur")
  mm <- sv("manual_month", default = "")
  tp <- .manual_month_to_quarter(mm)
  if (is.na(ur) && is.na(er) && is.na(ir)) return(NULL)
  list(unemp = ur, emp = er, inact = ir, period = tp)
}

# fills oecd placeholders for all countries, g7 average, and bullets
.fill_oecd_placeholders <- function(doc, unemp_data, emp_data, inact_data) {
  country_codes <- c("United Kingdom" = "uk", "United States" = "us",
                     "France" = "fr", "Germany" = "de", "Italy" = "it",
                     "Spain" = "es", "Canada" = "ca", "Japan" = "jp",
                     "Euro area" = "ea")

  .get_val <- function(data, country) {
    if (is.null(data)) return(list(period = "", value = NA_real_))
    idx <- match(country, data$country)
    if (is.na(idx)) return(list(period = "", value = NA_real_))
    list(period = data$period[idx], value = data$value[idx])
  }

  # override uk values with ons data so oecd table uses domestic source
  uk_ons <- .get_uk_ons_overrides()
  if (!is.null(uk_ons)) {
    .override_uk <- function(data, ons_val, tp) {
      if (is.na(ons_val)) return(data)
      uk_row <- data.frame(country = "United Kingdom", period = tp, value = ons_val,
                           stringsAsFactors = FALSE)
      if (is.null(data)) return(uk_row)
      data <- data[data$country != "United Kingdom", , drop = FALSE]
      rbind(data, uk_row)
    }
    unemp_data <- .override_uk(unemp_data, uk_ons$unemp, uk_ons$period)
    emp_data   <- .override_uk(emp_data,   uk_ons$emp,   uk_ons$period)
    inact_data <- .override_uk(inact_data, uk_ons$inact, uk_ons$period)
  }

  stale_parts <- character(0)

  for (country in names(country_codes)) {
    cc <- country_codes[[country]]
    u <- .get_val(unemp_data, country)
    e <- .get_val(emp_data, country)
    ia <- .get_val(inact_data, country)

    # row period = latest available across the three metrics
    avail <- c(u$period, e$period, ia$period)
    avail <- avail[nzchar(avail)]
    tp <- if (length(avail) > 0) sort(avail, decreasing = TRUE)[1] else ""

    # format values, appending ** when from an older period
    u_str  <- if (is.na(u$value))  "" else paste0(fmt_one_dec(u$value), "%")
    e_str  <- if (is.na(e$value))  "" else paste0(fmt_one_dec(e$value), "%")
    ia_str <- if (is.na(ia$value)) "" else paste0(fmt_one_dec(ia$value), "%")

    if (nzchar(tp)) {
      if (nzchar(u$period) && u$period != tp) {
        u_str <- paste0(u_str, "**")
        stale_parts <- c(stale_parts, paste0(country, " unemployment rate from ", u$period))
      }
      if (nzchar(e$period) && e$period != tp) {
        e_str <- paste0(e_str, "**")
        stale_parts <- c(stale_parts, paste0(country, " employment rate from ", e$period))
      }
      if (nzchar(ia$period) && ia$period != tp) {
        ia_str <- paste0(ia_str, "**")
        stale_parts <- c(stale_parts, paste0(country, " inactivity rate from ", ia$period))
      }
    }

    if (cc == "uk" && nzchar(tp)) tp <- paste0(tp, "*")

    doc <- replace_all(doc, paste0("qvzoecd", cc, "tp"), tp)
    doc <- replace_all(doc, paste0("qvzoecd", cc, "ur"), u_str)
    doc <- replace_all(doc, paste0("qvzoecd", cc, "er"), e_str)
    doc <- replace_all(doc, paste0("qvzoecd", cc, "ir"), ia_str)
  }

  # g7 average row
  g7u  <- .compute_g7_average(unemp_data)
  g7e  <- .compute_g7_average(emp_data)
  g7ia <- .compute_g7_average(inact_data)
  g7tp <- if (nzchar(g7u$period)) g7u$period else if (nzchar(g7e$period)) g7e$period else g7ia$period
  doc <- replace_all(doc, "qvzoecdg7tp", if (is.na(g7tp) || !nzchar(g7tp)) "" else g7tp)
  doc <- replace_all(doc, "qvzoecdg7ur", if (is.na(g7u$value)) "" else paste0(fmt_one_dec(g7u$value), "%"))
  doc <- replace_all(doc, "qvzoecdg7er", if (is.na(g7e$value)) "" else paste0(fmt_one_dec(g7e$value), "%"))
  doc <- replace_all(doc, "qvzoecdg7ir", if (is.na(g7ia$value)) "" else paste0(fmt_one_dec(g7ia$value), "%"))

  # ** footnote for older data
  stale_note <- if (length(stale_parts) > 0) {
    paste0("**Latest ", paste(stale_parts, collapse = ". "), ".")
  } else ""
  doc <- replace_all(doc, "qvzoecdstalenote", stale_note)

  # bullet points
  bullets <- .generate_oecd_bullets(unemp_data, emp_data, inact_data)
  doc <- replace_all(doc, "qvzoecdbullet1", bullets$bullet1)
  doc <- replace_all(doc, "qvzoecdbullet2", bullets$bullet2)
  doc <- replace_all(doc, "qvzoecdbullet3", bullets$bullet3)

  doc
}
