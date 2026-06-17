# working days lost to labour disputes - a01 sheet 18

DAYS_LOST_CODE <- "BBFW"

fetch_days_lost <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())
  tryCatch({
    result <- DBI::dbGetQuery(conn, 'SELECT time_period, dataset_identifier_code, value
FROM "ons"."labour_market__disputes"')
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("fetch days lost failed: ", e$message)
    tibble::tibble(time_period = character(), dataset_identifier_code = character(), value = numeric())
  },
  finally = DBI::dbDisconnect(conn))
}

compute_days_lost <- function(pg_data, manual_mm) {
  cm <- parse_manual_month(manual_mm)
  anchor <- cm %m-% months(2)
  lab_cur <- make_payroll_label(anchor)

  match_row <- pg_data %>%
    filter(dataset_identifier_code == DAYS_LOST_CODE, startsWith(time_period, lab_cur))

  cur <- if (nrow(match_row) == 0) NA_real_ else suppressWarnings(as.numeric(match_row$value[1]))

  # 2019 monthly average, to contextualise the latest figure
  rows_2019 <- pg_data %>%
    filter(dataset_identifier_code == DAYS_LOST_CODE, grepl("2019", time_period))
  vals_2019 <- suppressWarnings(as.numeric(rows_2019$value))
  vals_2019 <- vals_2019[!is.na(vals_2019)]
  avg_2019 <- if (length(vals_2019) == 0) NA_real_ else mean(vals_2019)

  list(cur = cur, avg_2019 = avg_2019, label = lab_cur, anchor = anchor)
}

calculate_days_lost <- function(manual_mm) {
  compute_days_lost(fetch_days_lost(), manual_mm)
}
