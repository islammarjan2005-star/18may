# Configurable "Key Charts" page.
# Extracts labour-market time series, renders an in-app preview, and appends
# native (editable) Word charts to a finished briefing .docx. Sourced by app.R.

# ---- metric registry --------------------------------------------------------
# Each entry defines one selectable metric. Add a metric = add one entry plus
# its branch in .wc_extract().
CHART_METRICS <- list(
  list(id = "unemp16",        label = "Unemployment rate (16+, SA)",
       type = "line", colour = "C80678", unit = "%",
       source = "ONS Labour Force Survey"),
  list(id = "emp_rate",       label = "Employment rate (16-64, SA)",
       type = "line", colour = "00285F", unit = "%",
       source = "ONS Labour Force Survey"),
  list(id = "inact",          label = "Inactivity rate (16-64, SA)",
       type = "line", colour = "C80678", unit = "%",
       source = "ONS Labour Force Survey"),
  list(id = "youth_unemp",    label = "Youth unemployment rate (16-24, SA)",
       type = "line", colour = "C80678", unit = "%",
       source = "ONS Labour Force Survey"),
  list(id = "payroll_change", label = "Payrolled employees, annual change",
       type = "bar",  colour = "00285F", unit = "",
       source = "Earnings and employment from PAYE RTI"),
  list(id = "payroll_level",  label = "Payrolled employees (level)",
       type = "line", colour = "00285F", unit = "",
       source = "Earnings and employment from PAYE RTI"),
  list(id = "vacancies",      label = "Vacancies (thousands)",
       type = "line", colour = "00285F", unit = "",
       source = "ONS Vacancy Survey"),
  list(id = "redundancies",   label = "Redundancies (level)",
       type = "line", colour = "C80678", unit = "",
       source = "ONS Labour Force Survey"),
  list(id = "wage_growth",    label = "Total pay growth (YoY %)",
       type = "line", colour = "00285F", unit = "%",
       source = "ONS Average Weekly Earnings"),
  list(id = "wage_growth_reg", label = "Regular pay growth (YoY %)",
       type = "line", colour = "C80678", unit = "%",
       source = "ONS Average Weekly Earnings")
)

# named vector for selectInput(choices=): names shown, ids are the values
chart_metric_choices <- function() {
  stats::setNames(vapply(CHART_METRICS, `[[`, "", "id"),
                  vapply(CHART_METRICS, `[[`, "", "label"))
}

.chart_metric_def <- function(id) {
  for (m in CHART_METRICS) if (identical(m$id, id)) return(m)
  NULL
}

# ---- small IO helpers -------------------------------------------------------
.wc_read <- function(path, sheet) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE)),
           error = function(e) NULL)
}

.wc_read_text <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  txt <- readChar(con, file.info(p)$size, useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  txt
}

.wc_write_text <- function(txt, p) {
  writeBin(charToRaw(enc2utf8(txt)), p)
}

# ---- LFS period-label parsing ----------------------------------------------
.WC_LFS_PAT <- paste0("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-",
                      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+(\\d{4})$")

# trailing 4-digit year of an LFS rolling-quarter label ("Oct-Dec 2025" -> "2025")
.wc_lfs_year <- function(labels) {
  labels <- trimws(as.character(labels))
  m <- regmatches(labels, regexec(.WC_LFS_PAT, labels, ignore.case = TRUE))
  vapply(m, function(x) if (length(x) == 4) x[4] else NA_character_, "")
}

# end-month date of an LFS label, for chronological ordering
.wc_lfs_end_date <- function(labels) {
  labels <- trimws(as.character(labels))
  m <- regmatches(labels, regexec(.WC_LFS_PAT, labels, ignore.case = TRUE))
  out <- rep(as.Date(NA), length(labels))
  for (i in seq_along(m)) {
    if (length(m[[i]]) == 4) {
      mo <- match(tolower(m[[i]][3]), tolower(month.abb))
      yr <- suppressWarnings(as.integer(m[[i]][4]))
      if (!is.na(mo) && !is.na(yr)) out[i] <- as.Date(sprintf("%04d-%02d-01", yr, mo))
    }
  }
  out
}

# ---- series extractors: manual (uploaded A01 / RTI files) -------------------
# A full LFS series from one column of an A01 sheet table.
.wc_series_lfs_sheet <- function(tbl, value_col) {
  if (is.null(tbl) || nrow(tbl) == 0 || ncol(tbl) < value_col) return(NULL)
  yr   <- .wc_lfs_year(tbl[[1]])
  keep <- which(!is.na(yr))
  if (length(keep) == 0) return(NULL)
  list(cat  = yr[keep],
       val  = suppressWarnings(as.numeric(tbl[[value_col]][keep])),
       year = suppressWarnings(as.integer(yr[keep])))
}

# A full series from a sheet whose column 1 holds dates (Excel serials or a
# date-typed column) rather than LFS rolling-quarter labels — e.g. the AWE
# wage sheets. Category label is the calendar year.
.wc_series_dated_sheet <- function(tbl, value_col) {
  if (is.null(tbl) || nrow(tbl) == 0 || ncol(tbl) < value_col) return(NULL)
  col1 <- tbl[[1]]
  if (inherits(col1, "Date") || inherits(col1, "POSIXct")) {
    d <- as.Date(col1)
  } else {
    serial <- suppressWarnings(as.numeric(col1))
    d <- rep(as.Date(NA), length(serial))
    plaus <- !is.na(serial) & serial > 20000 & serial < 80000
    d[plaus] <- as.Date(serial[plaus], origin = "1899-12-30")
  }
  val  <- suppressWarnings(as.numeric(tbl[[value_col]]))
  keep <- which(!is.na(d) & !is.na(val))
  if (length(keep) == 0) return(NULL)
  yr <- as.integer(format(d[keep], "%Y"))
  list(cat = as.character(yr), val = val[keep], year = yr)
}

# Youth 16-24 rate is not a column: derive from the 16-17 and 18-24 bands
# of A01 sheet 2 (unemployment level / economic-activity level).
.wc_series_youth_manual <- function(file_a01) {
  tbl <- .wc_read(file_a01, "2")
  if (is.null(tbl) || ncol(tbl) < 30) return(NULL)
  yr   <- .wc_lfs_year(tbl[[1]])
  keep <- which(!is.na(yr))
  if (length(keep) == 0) return(NULL)
  num <- function(col) suppressWarnings(as.numeric(tbl[[col]][keep]))
  u <- num(20) + num(28)   # unemployment level: 16-17 + 18-24
  a <- num(22) + num(30)   # economically active level: 16-17 + 18-24
  list(cat = yr[keep], val = 100 * u / a,
       year = suppressWarnings(as.integer(yr[keep])))
}

.wc_payroll_df_manual <- function(file_rtisa) {
  tbl <- .wc_read(file_rtisa, "1. Payrolled employees (UK)")
  if (is.null(tbl) || nrow(tbl) == 0 || ncol(tbl) < 2) return(NULL)
  d <- suppressWarnings(lubridate::parse_date_time(trimws(as.character(tbl[[1]])),
                                                   orders = c("B Y", "bY", "BY")))
  d <- as.Date(d)
  v <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(tbl[[2]]))))
  ok <- !is.na(d) & !is.na(v)
  if (!any(ok)) return(NULL)
  df <- data.frame(d = d[ok], v = v[ok])
  df[order(df$d), ]
}

# turn a (date, value) payroll frame into a level or annual-change series
.wc_payroll_series <- function(df, kind) {
  if (is.null(df) || nrow(df) < 2) return(NULL)
  if (identical(kind, "level")) {
    return(list(cat  = format(df$d, "%Y"), val = df$v,
                year = as.integer(format(df$d, "%Y"))))
  }
  n <- nrow(df)
  if (n <= 12) return(NULL)
  d2 <- df$d[13:n]
  list(cat  = format(d2, "%b-%y"),
       val  = df$v[13:n] - df$v[1:(n - 12)],
       year = as.integer(format(d2, "%Y")))
}

# ---- series extractors: auto (database tibbles) -----------------------------
.wc_series_lfs_db <- function(pg, code) {
  if (is.null(pg) || nrow(pg) == 0) return(NULL)
  d <- pg[!is.na(pg$dataset_identifier_code) & pg$dataset_identifier_code == code, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  yr   <- .wc_lfs_year(d$time_period)
  keep <- which(!is.na(yr))
  if (length(keep) == 0) return(NULL)
  ord  <- order(.wc_lfs_end_date(d$time_period[keep]))
  list(cat  = yr[keep][ord],
       val  = suppressWarnings(as.numeric(d$value[keep]))[ord],
       year = suppressWarnings(as.integer(yr[keep]))[ord])
}

# A full series from a database table whose time_period is a "%Y-%m-%d" date
# (the AWE wage tables) rather than an LFS rolling-quarter label.
.wc_series_dated_db <- function(pg, code) {
  if (is.null(pg) || nrow(pg) == 0) return(NULL)
  d <- pg[!is.na(pg$dataset_identifier_code) & pg$dataset_identifier_code == code, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  dt <- as.Date(substr(as.character(d$time_period), 1, 10))
  v  <- suppressWarnings(as.numeric(d$value))
  ok <- !is.na(dt) & !is.na(v)
  if (!any(ok)) return(NULL)
  dt <- dt[ok]; v <- v[ok]
  ord <- order(dt)
  yr  <- as.integer(format(dt[ord], "%Y"))
  list(cat = as.character(yr), val = v[ord], year = yr)
}

.wc_series_youth_db <- function(pg) {
  if (is.null(pg) || nrow(pg) == 0) return(NULL)
  by_code <- function(code) {
    d <- pg[!is.na(pg$dataset_identifier_code) & pg$dataset_identifier_code == code, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    stats::setNames(suppressWarnings(as.numeric(d$value)), as.character(d$time_period))
  }
  u1 <- by_code("YBVH"); u2 <- by_code("YBVN")   # unemployment level 16-17 / 18-24
  a1 <- by_code("YBZL"); a2 <- by_code("YBZO")   # activity level 16-17 / 18-24
  if (is.null(u1) || is.null(u2) || is.null(a1) || is.null(a2)) return(NULL)
  common <- Reduce(intersect, list(names(u1), names(u2), names(a1), names(a2)))
  common <- common[!is.na(.wc_lfs_year(common))]
  if (length(common) == 0) return(NULL)
  common <- common[order(.wc_lfs_end_date(common))]
  list(cat  = .wc_lfs_year(common),
       val  = as.numeric(100 * (u1[common] + u2[common]) / (a1[common] + a2[common])),
       year = suppressWarnings(as.integer(.wc_lfs_year(common))))
}

.wc_payroll_df_db <- function(pg) {
  if (is.null(pg) || nrow(pg) == 0) return(NULL)
  d <- pg[!is.na(pg$unit_type) & pg$unit_type == "Payrolled employees", , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  dt <- as.Date(paste0("01 ", d$time_period), format = "%d %B %Y")
  v  <- suppressWarnings(as.numeric(d$value))
  ok <- !is.na(dt) & !is.na(v)
  if (!any(ok)) return(NULL)
  df <- data.frame(d = dt[ok], v = v[ok])
  df[order(df$d), ]
}

# ---- dispatch ---------------------------------------------------------------
.wc_extract <- function(id, flow, sources) {
  if (identical(flow, "manual")) {
    a01 <- sources$file_a01; rti <- sources$file_rtisa
    switch(id,
      unemp16         = .wc_series_lfs_sheet(.wc_read(a01, "1"), 9),
      emp_rate        = .wc_series_lfs_sheet(.wc_read(a01, "1"), 17),
      inact           = .wc_series_lfs_sheet(.wc_read(a01, "1"), 19),
      youth_unemp     = .wc_series_youth_manual(a01),
      payroll_level   = .wc_payroll_series(.wc_payroll_df_manual(rti), "level"),
      payroll_change  = .wc_payroll_series(.wc_payroll_df_manual(rti), "change"),
      vacancies       = .wc_series_lfs_sheet(.wc_read(a01, "19"), 3),
      redundancies    = .wc_series_lfs_sheet(.wc_read(a01, "10"), 2),
      wage_growth     = .wc_series_dated_sheet(.wc_read(a01, "13"), 4),
      wage_growth_reg = .wc_series_dated_sheet(.wc_read(a01, "15"), 4),
      NULL)
  } else {
    lfs <- sources$pg_lfs; pay <- sources$pg_payroll
    switch(id,
      unemp16         = .wc_series_lfs_db(lfs, "MGSX"),
      emp_rate        = .wc_series_lfs_db(lfs, "LF24"),
      inact           = .wc_series_lfs_db(lfs, "LF2S"),
      youth_unemp     = .wc_series_youth_db(lfs),
      payroll_level   = .wc_payroll_series(.wc_payroll_df_db(pay), "level"),
      payroll_change  = .wc_payroll_series(.wc_payroll_df_db(pay), "change"),
      vacancies       = .wc_series_lfs_db(sources$pg_vac, "AP2Y"),
      redundancies    = .wc_series_lfs_db(sources$pg_redund, "BEAO"),
      wage_growth     = .wc_series_dated_db(sources$pg_wages_total, "KAC3"),
      wage_growth_reg = .wc_series_dated_db(sources$pg_wages_reg, "KAI9"),
      NULL)
  }
}

# Build a list of chart objects (one per selected metric), each clipped to the
# [year_from, year_to] window. Metrics with no data yield an empty chart so the
# preview can flag them; append_key_charts_page() drops those from the Word doc.
build_chart_series <- function(metric_ids, year_from, year_to, flow, sources) {
  charts <- list()
  for (id in metric_ids) {
    md <- .chart_metric_def(id)
    if (is.null(md)) next
    s <- tryCatch(.wc_extract(id, flow, sources), error = function(e) NULL)
    if (is.null(s) || length(s$val) == 0) {
      charts[[length(charts) + 1]] <- c(md, list(cat = character(0), val = numeric(0)))
      next
    }
    keep <- which(!is.na(s$year) & s$year >= year_from & s$year <= year_to)
    charts[[length(charts) + 1]] <- c(md, list(cat = s$cat[keep], val = s$val[keep]))
  }
  charts
}

# Database flow: fetch only the tables the selected metrics need (one query
# each). Requires sheets/lfs.R, payroll.R, vacancies.R, redundancy.R and
# wages_nominal.R to have been sourced so the fetch_* functions exist.
.WC_AUTO_FETCH <- list(
  unemp16 = "lfs", emp_rate = "lfs", inact = "lfs", youth_unemp = "lfs",
  payroll_level = "payroll", payroll_change = "payroll",
  vacancies = "vac", redundancies = "redund",
  wage_growth = "wages_total", wage_growth_reg = "wages_reg"
)

build_auto_sources <- function(metric_ids) {
  need <- unique(unlist(.WC_AUTO_FETCH[metric_ids]))
  src  <- list()
  if ("lfs"         %in% need) src$pg_lfs         <- fetch_lfs()
  if ("payroll"     %in% need) src$pg_payroll     <- fetch_payroll()
  if ("vac"         %in% need) src$pg_vac         <- fetch_vacancies()
  if ("redund"      %in% need) src$pg_redund      <- fetch_redundancy()
  if ("wages_total" %in% need) src$pg_wages_total <- fetch_wages_total()
  if ("wages_reg"   %in% need) src$pg_wages_reg   <- fetch_wages_regular()
  src
}

# ---- chart XML generation ---------------------------------------------------
.wc_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.wc_num1 <- function(x) {
  if (is.na(x)) return("0")
  if (x == round(x) && abs(x) < 1e15) return(sprintf("%.0f", x))
  formatC(x, format = "f", digits = 6, drop0trailing = TRUE)
}

.wc_cat_cache <- function(cat) {
  n   <- length(cat)
  pts <- paste0('<c:pt idx="', seq_len(n) - 1, '"><c:v>',
                .wc_xml_escape(as.character(cat)), '</c:v></c:pt>', collapse = "")
  paste0('<c:strLit><c:ptCount val="', n, '"/>', pts, '</c:strLit>')
}

.wc_val_cache <- function(val) {
  n   <- length(val)
  idx <- seq_len(n) - 1
  ok  <- !is.na(val)
  pts <- paste0('<c:pt idx="', idx[ok], '"><c:v>',
                vapply(val[ok], .wc_num1, ""), '</c:v></c:pt>', collapse = "")
  paste0('<c:numLit><c:formatCode>General</c:formatCode><c:ptCount val="', n, '"/>',
         pts, '</c:numLit>')
}


# ---- chart XML templates (inlined to avoid committing .xml files) ----
.WC_LINE_TEMPLATE <- r"----(<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:c16r2="http://schemas.microsoft.com/office/drawing/2015/06/chart"><c:date1904 val="0"/><c:lang val="en-US"/><c:roundedCorners val="0"/><mc:AlternateContent xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"><mc:Choice Requires="c14" xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart"><c14:style val="102"/></mc:Choice><mc:Fallback><c:style val="2"/></mc:Fallback></mc:AlternateContent><c:clrMapOvr bg1="lt1" tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" accent2="accent2" accent3="accent3" accent4="accent4" accent5="accent5" accent6="accent6" hlink="hlink" folHlink="folHlink"/><c:chart><c:autoTitleDeleted val="1"/><c:plotArea><c:layout><c:manualLayout><c:layoutTarget val="inner"/><c:xMode val="edge"/><c:yMode val="edge"/><c:x val="5.4667542067634335E-2"/><c:y val="6.7071117866877847E-2"/><c:w val="0.91404554559663576"/><c:h val="0.8655432408218322"/></c:manualLayout></c:layout><c:lineChart><c:grouping val="standard"/><c:varyColors val="0"/><c:ser><c:idx val="0"/><c:order val="0"/><c:tx><c:v>__SERIES_NAME__</c:v></c:tx><c:spPr><a:ln w="28575" cap="rnd"><a:solidFill><a:srgbClr val="__SERIES_COLOUR__"/></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr><c:marker><c:symbol val="none"/></c:marker><c:cat>__CAT_CACHE__</c:cat><c:val>__VAL_CACHE__</c:val><c:smooth val="0"/><c:extLst><c:ext uri="{C3380CC4-5D6E-409C-BE32-E72D297353CC}" xmlns:c16="http://schemas.microsoft.com/office/drawing/2014/chart"><c16:uniqueId val="{00000002-19AC-4AF5-BCD2-C102580C4CA2}"/></c:ext></c:extLst></c:ser><c:dLbls><c:showLegendKey val="0"/><c:showVal val="0"/><c:showCatName val="0"/><c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbls><c:smooth val="0"/><c:axId val="467368880"/><c:axId val="467368400"/></c:lineChart><c:catAx><c:axId val="467368880"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:numFmt formatCode="General" sourceLinked="1"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="nextTo"/><c:spPr><a:noFill/><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:schemeClr val="tx1"><a:lumMod val="15000"/><a:lumOff val="85000"/></a:schemeClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot="-5400000" spcFirstLastPara="1" vertOverflow="ellipsis" vert="horz" wrap="square" anchor="ctr" anchorCtr="1"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="800" b="0" i="0" u="none" strike="noStrike" kern="1200" baseline="0"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:ea typeface="+mn-ea"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr><c:crossAx val="467368400"/><c:crosses val="autoZero"/><c:auto val="1"/><c:lblAlgn val="ctr"/><c:lblOffset val="100"/><c:tickLblSkip val="12"/><c:noMultiLvlLbl val="0"/></c:catAx><c:valAx><c:axId val="467368400"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="l"/><c:majorGridlines><c:spPr><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:schemeClr val="tx1"><a:lumMod val="15000"/><a:lumOff val="85000"/></a:schemeClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr></c:majorGridlines><c:numFmt formatCode="General" sourceLinked="1"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="nextTo"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot="-60000000" spcFirstLastPara="1" vertOverflow="ellipsis" vert="horz" wrap="square" anchor="ctr" anchorCtr="1"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="900" b="0" i="0" u="none" strike="noStrike" kern="1200" baseline="0"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:ea typeface="+mn-ea"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr><c:crossAx val="467368880"/><c:crosses val="autoZero"/><c:crossBetween val="between"/></c:valAx></c:plotArea><c:plotVisOnly val="1"/><c:dispBlanksAs val="gap"/><c:showDLblsOverMax val="0"/><c:extLst/></c:chart><c:spPr><a:solidFill><a:schemeClr val="bg1"/></a:solidFill><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:noFill/><a:round/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr></c:chartSpace>)----"
.WC_BAR_TEMPLATE  <- r"----(<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:c16r2="http://schemas.microsoft.com/office/drawing/2015/06/chart"><c:date1904 val="0"/><c:lang val="en-US"/><c:roundedCorners val="0"/><mc:AlternateContent xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"><mc:Choice Requires="c14" xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart"><c14:style val="102"/></mc:Choice><mc:Fallback><c:style val="2"/></mc:Fallback></mc:AlternateContent><c:clrMapOvr bg1="lt1" tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" accent2="accent2" accent3="accent3" accent4="accent4" accent5="accent5" accent6="accent6" hlink="hlink" folHlink="folHlink"/><c:chart><c:autoTitleDeleted val="1"/><c:plotArea><c:layout><c:manualLayout><c:layoutTarget val="inner"/><c:xMode val="edge"/><c:yMode val="edge"/><c:x val="0.14635489725000228"/><c:y val="5.9741215254997371E-2"/><c:w val="0.79362141102119832"/><c:h val="0.86684838768949646"/></c:manualLayout></c:layout><c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:varyColors val="0"/><c:ser><c:idx val="0"/><c:order val="0"/><c:spPr><a:solidFill><a:srgbClr val="__SERIES_COLOUR__"/></a:solidFill><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr><c:invertIfNegative val="1"/><c:cat>__CAT_CACHE__</c:cat><c:val>__VAL_CACHE__</c:val><c:extLst><c:ext uri="{6F2FDCE9-48DA-4B69-8628-5D25D57E5C99}" xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart"><c14:invertSolidFillFmt><c14:spPr xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart"><a:solidFill><a:srgbClr val="7B005B"/></a:solidFill><a:ln><a:noFill/></a:ln><a:effectLst/></c14:spPr></c14:invertSolidFillFmt></c:ext><c:ext uri="{C3380CC4-5D6E-409C-BE32-E72D297353CC}" xmlns:c16="http://schemas.microsoft.com/office/drawing/2014/chart"><c16:uniqueId val="{00000000-C1DA-43E9-BF6E-B193466C17A5}"/></c:ext></c:extLst></c:ser><c:dLbls><c:showLegendKey val="0"/><c:showVal val="0"/><c:showCatName val="0"/><c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbls><c:gapWidth val="50"/><c:overlap val="-27"/><c:axId val="1330111023"/><c:axId val="1330093743"/></c:barChart><c:catAx><c:axId val="1330111023"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:numFmt formatCode="[$-F800]dddd\,\ mmmm\ dd\,\ yyyy" sourceLinked="0"/><c:majorTickMark val="out"/><c:minorTickMark val="none"/><c:tickLblPos val="low"/><c:spPr><a:noFill/><a:ln w="12700" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot="-60000000" spcFirstLastPara="1" vertOverflow="ellipsis" vert="horz" wrap="square" anchor="ctr" anchorCtr="1"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="700" b="0" i="0" u="none" strike="noStrike" kern="1200" baseline="0"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:ea typeface="+mn-ea"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr><c:crossAx val="1330093743"/><c:crosses val="autoZero"/><c:auto val="1"/><c:lblAlgn val="ctr"/><c:lblOffset val="100"/><c:tickLblSkip val="12"/><c:noMultiLvlLbl val="0"/></c:catAx><c:valAx><c:axId val="1330093743"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="l"/><c:majorGridlines><c:spPr><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:solidFill><a:schemeClr val="tx1"><a:lumMod val="15000"/><a:lumOff val="85000"/></a:schemeClr></a:solidFill><a:round/></a:ln><a:effectLst/></c:spPr></c:majorGridlines><c:numFmt formatCode="_-* #,##0_-;\-* #,##0_-;_-* &quot;-&quot;??_-;_-@_-" sourceLinked="1"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="nextTo"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr rot="-60000000" spcFirstLastPara="1" vertOverflow="ellipsis" vert="horz" wrap="square" anchor="ctr" anchorCtr="1"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="700" b="0" i="0" u="none" strike="noStrike" kern="1200" baseline="0"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:ea typeface="+mn-ea"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr><c:crossAx val="1330111023"/><c:crosses val="autoZero"/><c:crossBetween val="between"/><c:dispUnits><c:builtInUnit val="thousands"/><c:dispUnitsLbl><c:layout><c:manualLayout><c:xMode val="edge"/><c:yMode val="edge"/><c:x val="8.5253868083726911E-3"/><c:y val="1.2039458857755527E-2"/></c:manualLayout></c:layout><c:tx><c:rich><a:bodyPr rot="-5400000" spcFirstLastPara="1" vertOverflow="ellipsis" vert="horz" wrap="square" anchor="ctr" anchorCtr="1"/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="1000" b="0" i="0" u="none" strike="noStrike" kern="1200" baseline="0"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:ea typeface="+mn-ea"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:r><a:rPr lang="en-GB" sz="700"/><a:t>Thousands</a:t></a:r></a:p></c:rich></c:tx><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln><a:effectLst/></c:spPr></c:dispUnitsLbl></c:dispUnits></c:valAx></c:plotArea><c:plotVisOnly val="1"/><c:dispBlanksAs val="gap"/><c:showDLblsOverMax val="0"/><c:extLst/></c:chart><c:spPr><a:solidFill><a:schemeClr val="bg1"/></a:solidFill><a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"><a:noFill/><a:round/></a:ln><a:effectLst/></c:spPr><c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="1000"><a:solidFill><a:sysClr val="windowText" lastClr="000000"/></a:solidFill><a:latin typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/><a:cs typeface="Arial" panose="020B0604020202020204" pitchFamily="34" charset="0"/></a:defRPr></a:pPr><a:endParaRPr lang="en-US"/></a:p></c:txPr></c:chartSpace>)----"

build_chart_xml <- function(chart) {
  out <- if (identical(chart$type, "bar")) .WC_BAR_TEMPLATE else .WC_LINE_TEMPLATE
  out <- gsub("__SERIES_NAME__",   .wc_xml_escape(chart$label), out, fixed = TRUE)
  out <- gsub("__SERIES_COLOUR__", chart$colour,                out, fixed = TRUE)
  out <- gsub("__CAT_CACHE__",     .wc_cat_cache(chart$cat),    out, fixed = TRUE)
  out <- gsub("__VAL_CACHE__",     .wc_val_cache(chart$val),    out, fixed = TRUE)
  out
}

# WordprocessingML for the Key Charts page: page break + heading + a 2-column
# table holding N charts (caption / chart / source per cell).
build_charts_page_xml <- function(charts, rids) {
  esc <- .wc_xml_escape
  cell <- function(i) {
    if (i > length(charts)) {
      return('<w:tc><w:tcPr><w:tcW w:w="5233" w:type="dxa"/></w:tcPr><w:p/></w:tc>')
    }
    ch  <- charts[[i]]
    cap <- paste0("Figure ", i, ": ", ch$label)
    src <- paste0("Source: ", ch$source, ".")
    drawing <- paste0(
      '<w:p><w:pPr><w:jc w:val="center"/></w:pPr><w:r><w:drawing>',
      '<wp:inline distT="0" distB="0" distL="0" distR="0" ',
      'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">',
      '<wp:extent cx="3000000" cy="2550000"/>',
      '<wp:effectExtent l="0" t="0" r="0" b="0"/>',
      '<wp:docPr id="', 1000 + i, '" name="Chart ', i, '"/>',
      '<wp:cNvGraphicFramePr/>',
      '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
      '<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/chart">',
      '<c:chart xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
      'r:id="', rids[i], '"/>',
      '</a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>')
    paste0(
      '<w:tc><w:tcPr><w:tcW w:w="5233" w:type="dxa"/></w:tcPr>',
      '<w:p><w:pPr><w:spacing w:before="120" w:after="40"/></w:pPr>',
      '<w:r><w:rPr><w:b/><w:sz w:val="20"/></w:rPr>',
      '<w:t xml:space="preserve">', esc(cap), '</w:t></w:r></w:p>',
      drawing,
      '<w:p><w:pPr><w:spacing w:after="200"/></w:pPr>',
      '<w:r><w:rPr><w:i/><w:sz w:val="16"/><w:color w:val="595959"/></w:rPr>',
      '<w:t xml:space="preserve">', esc(src), '</w:t></w:r></w:p>',
      '</w:tc>')
  }
  n     <- length(charts)
  nrows <- ceiling(n / 2)
  trs   <- vapply(seq_len(nrows), function(r)
    paste0("<w:tr>", cell(2 * r - 1), cell(2 * r), "</w:tr>"), "")
  tbl <- paste0(
    '<w:tbl><w:tblPr><w:tblW w:w="10466" w:type="dxa"/><w:tblLayout w:type="fixed"/>',
    '<w:tblLook w:val="0000" w:firstRow="0" w:lastRow="0" w:firstColumn="0" ',
    'w:lastColumn="0" w:noHBand="1" w:noVBand="1"/></w:tblPr>',
    '<w:tblGrid><w:gridCol w:w="5233"/><w:gridCol w:w="5233"/></w:tblGrid>',
    paste0(trs, collapse = ""), '</w:tbl>')
  heading <- paste0(
    '<w:p><w:pPr><w:spacing w:before="120" w:after="160"/></w:pPr>',
    '<w:r><w:rPr><w:b/><w:sz w:val="32"/></w:rPr><w:t>Key Charts</w:t></w:r></w:p>')
  paste0('<w:p><w:r><w:br w:type="page"/></w:r></w:p>', heading, tbl, '<w:p/>')
}

# Append the Key Charts page (native editable charts) to a finished .docx.
append_key_charts_page <- function(docx_path, charts) {
  charts <- Filter(function(c) !is.null(c) && length(c$val) >= 2, charts)
  if (length(charts) == 0) return(invisible(docx_path))

  tmp <- tempfile("wc_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(docx_path, exdir = tmp)

  # relationship ids that do not collide with officer's existing ones
  rels_path <- file.path(tmp, "word", "_rels", "document.xml.rels")
  rels <- .wc_read_text(rels_path)
  ids  <- as.integer(sub("rId", "", regmatches(rels, gregexpr("rId[0-9]+", rels))[[1]]))
  base <- if (length(ids)) max(ids) + 1 else 100
  rids <- paste0("rId", base + seq_along(charts) - 1)

  # chart parts
  dir.create(file.path(tmp, "word", "charts"), showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(charts)) {
    .wc_write_text(build_chart_xml(charts[[i]]),
                   file.path(tmp, "word", "charts", paste0("chart", i, ".xml")))
  }

  # [Content_Types].xml
  ct_path <- file.path(tmp, "[Content_Types].xml")
  ct <- .wc_read_text(ct_path)
  ov <- paste0('<Override PartName="/word/charts/chart', seq_along(charts),
               '.xml" ContentType="application/vnd.openxmlformats-officedocument.',
               'drawingml.chart+xml"/>', collapse = "")
  .wc_write_text(sub("</Types>", paste0(ov, "</Types>"), ct, fixed = TRUE), ct_path)

  # document relationships
  nr <- paste0('<Relationship Id="', rids,
               '" Type="http://schemas.openxmlformats.org/officeDocument/2006/',
               'relationships/chart" Target="charts/chart', seq_along(charts),
               '.xml"/>', collapse = "")
  .wc_write_text(sub("</Relationships>", paste0(nr, "</Relationships>"), rels, fixed = TRUE),
                 rels_path)

  # document body: insert before the last (body-level) sectPr
  doc_path <- file.path(tmp, "word", "document.xml")
  doc <- .wc_read_text(doc_path)
  page <- build_charts_page_xml(charts, rids)
  hits <- gregexpr("<w:sectPr", doc, fixed = TRUE)[[1]]
  pos  <- hits[length(hits)]
  doc  <- paste0(substr(doc, 1, pos - 1), page, substr(doc, pos, nchar(doc)))
  .wc_write_text(doc, doc_path)

  # rezip in place
  out_abs <- normalizePath(docx_path, mustWork = FALSE)
  if (file.exists(out_abs)) unlink(out_abs)
  zip::zip(zipfile = out_abs, files = list.files(tmp), root = tmp)
  invisible(docx_path)
}

# ---- in-app preview (base graphics) -----------------------------------------
.wc_draw_line <- function(ch, title) {
  y <- ch$val; x <- seq_along(y)
  plot(x, y, type = "n", xaxt = "n", xlab = "", ylab = ch$unit,
       main = "", bty = "n", las = 1)
  abline(h = axTicks(2), col = "grey90", lwd = 0.8)
  lines(x, y, lwd = 2.5, col = paste0("#", ch$colour))
  title(main = title, cex.main = 0.98, font.main = 2, adj = 0)
  yr  <- ch$cat
  chg <- which(c(TRUE, yr[-1] != yr[-length(yr)]))
  if (length(chg) > 8) chg <- chg[round(seq(1, length(chg), length.out = 8))]
  axis(1, at = x[chg], labels = yr[chg], cex.axis = 0.8)
  mtext(paste0("Source: ", ch$source), side = 1, line = 2.3, cex = 0.6, col = "grey50")
}

.wc_draw_bar <- function(ch, title) {
  y    <- ch$val
  cols <- ifelse(y < 0, "#7B005B", paste0("#", ch$colour))
  bp   <- barplot(y, col = cols, border = NA, xaxt = "n", las = 1, main = "")
  title(main = title, cex.main = 0.98, font.main = 2, adj = 0)
  abline(h = 0, col = "grey40")
  yy  <- sub("^[A-Za-z]{3}-", "", ch$cat)
  chg <- which(c(TRUE, yy[-1] != yy[-length(yy)]))
  if (length(chg) > 8) chg <- chg[round(seq(1, length(chg), length.out = 8))]
  axis(1, at = bp[chg], labels = paste0("'", yy[chg]), cex.axis = 0.8)
  mtext(paste0("Source: ", ch$source), side = 1, line = 2.3, cex = 0.6, col = "grey50")
}

render_key_charts_preview <- function(charts) {
  charts <- Filter(Negate(is.null), charts)
  n <- length(charts)
  if (n == 0) {
    plot.new(); text(0.5, 0.5, "Select metrics and click Charts to preview.", col = "grey40")
    return(invisible())
  }
  op <- par(mfrow = c(ceiling(n / 2), min(n, 2)),
            mar = c(4, 4, 2.6, 1), mgp = c(2.4, 0.7, 0))
  on.exit(par(op))
  for (i in seq_len(n)) {
    ch    <- charts[[i]]
    title <- paste0("Figure ", i, ": ", ch$label)
    if (length(ch$val) < 2 || all(is.na(ch$val))) {
      plot.new(); title(main = title, cex.main = 0.98, font.main = 2, adj = 0)
      text(0.5, 0.5, "Data unavailable", col = "grey45")
      next
    }
    if (identical(ch$type, "bar")) .wc_draw_bar(ch, title) else .wc_draw_line(ch, title)
  }
}
