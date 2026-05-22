library(shiny)

# Raise upload cap from Shiny's 5 MB default — the ONS LMS bulk time-series
# file is ~10 MB and may grow over time. 100 MB headroom covers all inputs.
options(shiny.maxRequestSize = 100 * 1024^2)

source("utils/word_charts.R")
source("utils/lms_explorer.R")

ui <- fluidPage(
  
  tags$head(
    tags$title("Labour Market Statistical Briefing"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600;700&display=swap');

      *, *::before, *::after { box-sizing: border-box; }
      html, body { margin: 0; padding: 0; min-height: 100vh; }

      body {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 19px;
        line-height: 1.31579;
        color: #0b0c0c;
        background-color: #f3f2f1;
      }

      .govuk-header {
        border-bottom: 10px solid #1d70b8;
        color: #ffffff;
        background: #0b0c0c;
      }

      .govuk-header__container {
        position: relative;
        margin-bottom: -10px;
        padding-top: 10px;
        border-bottom: 10px solid #1d70b8;
        max-width: 960px;
        margin-left: auto;
        margin-right: auto;
        padding-left: 15px;
        padding-right: 15px;
      }

      .govuk-header__logotype-text {
        font-weight: 700;
        font-size: 30px;
        line-height: 1;
        color: #ffffff;
      }

      .govuk-header__link {
        text-decoration: none;
        color: #ffffff;
      }

      .govuk-header__service-name {
        display: inline-block;
        margin-bottom: 10px;
        font-weight: 700;
        font-size: 24px;
        color: #ffffff;
      }

      .govuk-width-container {
        max-width: 960px;
        margin: 0 auto;
        padding: 0 15px;
      }

      .govuk-main-wrapper {
        padding: 40px 0;
      }

      .govuk-heading-xl {
        font-weight: 700;
        font-size: 48px;
        margin: 0 0 50px 0;
      }

      .govuk-heading-m {
        font-weight: 700;
        font-size: 24px;
        margin: 0 0 20px 0;
      }

      .govuk-body {
        font-size: 19px;
        margin: 0 0 20px 0;
      }

      .govuk-button {
        font-weight: 400;
        font-size: 19px;
        line-height: 1;
        display: inline-block;
        position: relative;
        padding: 8px 10px 7px;
        border: 2px solid transparent;
        border-radius: 0;
        color: #ffffff;
        background-color: #00703c;
        box-shadow: 0 2px 0 #002d18;
        text-align: center;
        cursor: pointer;
        margin-right: 15px;
        margin-bottom: 15px;
      }

      .govuk-button:hover { background-color: #005a30; }
      .govuk-button:focus {
        border-color: #ffdd00;
        outline: 3px solid transparent;
        box-shadow: inset 0 0 0 1px #ffdd00;
        background-color: #ffdd00;
        color: #0b0c0c;
      }

      .govuk-button--blue {
        background-color: #1d70b8;
        box-shadow: 0 2px 0 #003078;
      }
      .govuk-button--blue:hover { background-color: #003078; }

      .govuk-button--secondary {
        background-color: #f3f2f1;
        box-shadow: 0 2px 0 #929191;
        color: #0b0c0c;
      }
      .govuk-button--secondary:hover { background-color: #dbdad9; }

      .govuk-button--warning {
        background-color: #d4351c;
        box-shadow: 0 2px 0 #6e1509;
      }
      .govuk-button--warning:hover { background-color: #aa2a16; }

      .govuk-button.shiny-download-link { text-decoration: none; }

      .govuk-form-group { margin-bottom: 30px; }

      .govuk-label {
        font-weight: 400;
        font-size: 19px;
        display: block;
        margin-bottom: 5px;
      }

      .govuk-hint {
        font-size: 19px;
        margin-bottom: 15px;
        color: #505a5f;
      }

      .govuk-input {
        font-size: 19px;
        width: 100%;
        max-width: 200px;
        height: 40px;
        padding: 5px;
        border: 2px solid #0b0c0c;
        border-radius: 0;
      }

      .govuk-input:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        box-shadow: inset 0 0 0 2px;
      }

      .govuk-select {
        font-size: 19px;
        width: 100%;
        max-width: 320px;
        height: 40px;
        padding: 5px;
        border: 2px solid #0b0c0c;
        border-radius: 0;
        background-color: #ffffff;
      }

      .govuk-select:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        box-shadow: inset 0 0 0 2px;
      }

      .govuk-width-container--wide {
        max-width: 1400px;
      }

      .preview-scroll {
        max-height: 460px;
        overflow: auto;
      }

      .govuk-phase-banner {
        padding: 10px 0;
        border-bottom: 1px solid #b1b4b6;
      }

      .govuk-tag {
        font-weight: 700;
        font-size: 16px;
        display: inline-block;
        padding: 6px 10px 5px;
        color: #ffffff;
        background-color: #1d70b8;
        letter-spacing: 1px;
        text-transform: uppercase;
        margin-right: 10px;
      }

      .file-tag-row {
        display: flex;
        flex-wrap: wrap;
        gap: 8px 10px;
        margin: 12px 0 4px;
      }
      .file-tag-row .govuk-tag { margin: 0; }

      .govuk-tag--green {
        background-color: #00703c;
      }

      .dashboard-card {
        background-color: #ffffff;
        border: 1px solid #b1b4b6;
        margin-bottom: 20px;
      }

      .dashboard-card__header {
        background-color: #1d70b8;
        color: #ffffff;
        padding: 15px 20px;
        font-weight: 700;
        font-size: 19px;
      }

      .dashboard-card__content { padding: 24px 28px; }

      .govuk-section-break {
        margin: 36px 0;
        border: 0;
        border-bottom: 1px solid #b1b4b6;
      }

      .period-block { margin-bottom: 24px; }
      .period-block > .govuk-label { margin-bottom: 10px; }
      .period-block:last-child { margin-bottom: 0; }

      .govuk-grid-row {
        display: flex;
        flex-wrap: wrap;
        margin: 0 -15px;
      }

      .govuk-grid-column-one-half {
        width: 50%;
        padding: 0 15px;
      }

      @media (max-width: 768px) {
        .govuk-grid-column-one-half { width: 100%; }
      }

      .govuk-footer {
        padding: 25px 0;
        border-top: 1px solid #b1b4b6;
        background: #f3f2f1;
        text-align: center;
        color: #505a5f;
      }

      .container-fluid { padding: 0 !important; margin: 0 !important; max-width: none !important; }

      .month-status {
        display: inline-block;
        padding: 5px 10px;
        margin-left: 10px;
        font-size: 16px;
        border-radius: 3px;
      }

      .month-status--confirmed {
        background-color: #00703c;
        color: #ffffff;
      }

      .month-status--pending {
        background-color: #f47738;
        color: #ffffff;
      }

      .input-row {
        display: flex;
        align-items: flex-end;
        gap: 15px;
        flex-wrap: wrap;
      }

      .input-row .govuk-form-group {
        margin-bottom: 0;
      }

      .stats-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 14px;
      }

      .stats-table th {
        background-color: #0b0c0c;
        color: #ffffff;
        font-weight: 700;
        padding: 10px 8px;
        text-align: left;
        border: 1px solid #0b0c0c;
        font-size: 12px;
      }

      .stats-table td {
        padding: 8px;
        border: 1px solid #b1b4b6;
        background-color: #ffffff;
      }

      .stats-table tr:nth-child(even) td { background-color: #f8f8f8; }
      .stats-table tr:hover td { background-color: #f3f2f1; }

      .stat-positive { color: #00703c; font-weight: 700; }
      .stat-negative { color: #d4351c; font-weight: 700; }
      .stat-neutral { color: #505a5f; }

      .top-ten-list {
        list-style: none;
        padding: 0;
        margin: 0;
        counter-reset: item;
      }

      .top-ten-list li {
        padding: 12px 12px 12px 50px;
        margin-bottom: 8px;
        background-color: #ffffff;
        border-left: 4px solid #1d70b8;
        position: relative;
        font-size: 15px;
        line-height: 1.4;
      }

      .top-ten-list li::before {
        counter-increment: item;
        content: counter(item);
        position: absolute;
        left: 12px;
        top: 12px;
        font-weight: 700;
        font-size: 18px;
        color: #1d70b8;
      }

      .govuk-list { padding-left: 20px; }
      .govuk-list li { margin-bottom: 5px; }

      .shiny-notification {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 400px;
        background: #ffffff;
        border: 3px solid #1d70b8;
        border-radius: 0;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        padding: 20px;
        z-index: 99999;
      }

      .shiny-notification-message {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 16px;
        color: #0b0c0c;
        margin-bottom: 15px;
      }

      .shiny-notification .progress {
        height: 10px;
        background-color: #f3f2f1;
        border-radius: 0;
        margin-top: 10px;
      }

      .shiny-notification .progress-bar {
        background-color: #00703c;
        border-radius: 0;
      }

      .shiny-notification-close {
        display: none;
      }

      /* spinner */
      .loader {
        border: 4px solid #f3f2f1;
        border-top: 4px solid #1d70b8;
        border-radius: 50%;
        width: 28px;
        height: 28px;
        animation: spin 0.9s linear infinite;
        display: inline-block;
        vertical-align: middle;
        margin-right: 12px;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      #vacancies_period, #payroll_period,
      #manual_vacancies_period, #manual_payroll_period {
        font-size: 19px;
        width: 100%;
        max-width: 320px;
        height: 40px;
        padding: 5px;
        border: 2px solid #0b0c0c;
        border-radius: 0;
        background-color: #ffffff;
      }
      #vacancies_period:focus, #payroll_period:focus,
      #manual_vacancies_period:focus, #manual_payroll_period:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        box-shadow: inset 0 0 0 2px;
      }

      /* tabs */
      .nav-pills { display: flex; list-style: none; margin: 0 0 20px 0; padding: 0; border-bottom: 2px solid #b1b4b6; }
      .nav-pills > li { margin: 0; }
      .nav-pills > li > a {
        display: block; padding: 10px 20px; font-size: 19px; font-weight: 700;
        color: #1d70b8; text-decoration: none; border-bottom: 4px solid transparent; margin-bottom: -2px;
        background: none; border-radius: 0;
      }
      .nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus {
        color: #0b0c0c; background: none; border-bottom-color: #1d70b8;
      }
      .nav-pills > li > a:hover { color: #003078; background: none; }

      .govuk-tag--grey { background-color: #b1b4b6; color: #ffffff; }
      .govuk-tag--amber { background-color: #f47738; }

      /* period toggle buttons */
      .period-toggle-group { display: inline-flex; border: 2px solid #0b0c0c; margin-right: 20px; margin-bottom: 15px; }
      .period-toggle-group .govuk-button {
        margin: 0; box-shadow: none; border: none; border-right: 1px solid #b1b4b6; border-radius: 0;
      }
      .period-toggle-group .govuk-button:last-child { border-right: none; }
      .period-toggle-group .govuk-button.active { background-color: #1d70b8; color: #ffffff; }
      .period-toggle-group .govuk-button:not(.active) { background-color: #f3f2f1; color: #0b0c0c; box-shadow: none; }
    ")),
    tags$script(HTML("
      $(document).on('click', '#preview_dashboard', function() {
        $('#dashboard_preview').html(
          '<div style=\"padding: 12px;\">' +
          '<div class=\"loader\"></div>' +
          '<strong>Loading dashboard\u2026</strong>' +
          '</div>'
        );
      });
    "))
  ),
  
  tags$header(class = "govuk-header",
              div(class = "govuk-header__container",
                  div(style = "margin-bottom: 10px;",
                      a(href = "#", class = "govuk-header__link",
                        span(class = "govuk-header__logotype-text", "GOV.UK")
                      )
                  ),
                  span(class = "govuk-header__service-name", "Labour Market Statistical Briefing")
              )
  ),
  
  div(class = "govuk-width-container",
      
      div(class = "govuk-phase-banner",
          span(class = "govuk-tag", "BETA"),
          span("This is a new service.")
      ),
      
      tags$main(class = "govuk-main-wrapper",
                
                h1(class = "govuk-heading-xl", "Labour Market Statistical Briefing Automation"),
                
                # manual vs automatic mode tabs
                tabsetPanel(id = "mode_tabs", type = "pills", selected = "manual",
                            
                            # manual tab
                            tabPanel("Manual", value = "manual",
                                     div(class = "dashboard-card", style = "margin-top: 20px;",
                                         div(class = "dashboard-card__header", "Manual (Excel Upload)"),
                                         div(class = "dashboard-card__content",
                                             
                                             div(class = "govuk-form-group",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Reference month"),
                                                 div(class = "govuk-hint", "Type the publication month (e.g. March 2026). Used for ONS download links."),
                                                 textInput("ref_month_input", label = NULL,
                                                           placeholder = "e.g. March 2026", width = "320px")
                                             ),
                                             div(class = "govuk-form-group", style = "display: flex; align-items: center; gap: 8px; flex-wrap: nowrap;",
                                                 span(style = "font-size: 14px; white-space: nowrap;",
                                                      "OFFICIAL-SENSITIVE \u2013 Internal Briefing \u2013 For external lines please contact"),
                                                 textInput("contact_names", label = NULL,
                                                           value = "",
                                                           placeholder = "e.g. John Smith and Jane Doe", width = "280px")
                                             ),
                                             
                                             tags$hr(class = "govuk-section-break"),
                                             
                                             div(class = "govuk-hint", style = "font-size: 17px; line-height: 1.4;",
                                                 "Click a grey tag below to open its ONS download page. OECD data is fetched automatically."),
                                             uiOutput("oecd_auto_status"),
                                             uiOutput("upload_status"),
                                             
                                             div(class = "govuk-form-group",
                                                 fileInput("upload_files", "Upload ONS Excel files",
                                                           accept = c(".xlsx", ".csv"), multiple = TRUE, width = "100%"),
                                                 div(class = "govuk-hint",
                                                     "Drag or select files. Auto-detected by name: A01, HR1, X09, RTISA, CLA01, X02, OECD.")
                                             ),
                                             
                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Period selection"),
                                             div(class = "period-block",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Vacancies"),
                                                 uiOutput("manual_vac_period_buttons")
                                             ),
                                             div(class = "period-block",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Payroll employees"),
                                                 uiOutput("manual_pay_period_buttons")
                                             ),
                                             
                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Preview"),
                                             actionButton("manual_preview_dashboard", "Dashboard", class = "govuk-button govuk-button--blue"),
                                             actionButton("manual_preview_topten", "Top Ten", class = "govuk-button govuk-button--blue"),
                                             actionButton("manual_preview_summary", "Summary", class = "govuk-button govuk-button--blue"),
                                             actionButton("manual_preview_oecd", "OECD", class = "govuk-button govuk-button--blue"),
                                             actionButton("manual_jump_charts", "Charts",
                                                          class = "govuk-button govuk-button--blue",
                                                          onclick = "document.getElementById('charts_card').scrollIntoView({behavior:'smooth', block:'start'})"),
                                             actionButton("manual_jump_custom", "Custom",
                                                          class = "govuk-button govuk-button--blue",
                                                          onclick = "document.getElementById('custom_indicators_card').scrollIntoView({behavior:'smooth', block:'start'})"),

                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Download"),
                                             downloadButton("manual_download_word", "Download Word", class = "govuk-button govuk-button--blue"),
                                             downloadButton("manual_download_excel", "Download Excel", class = "govuk-button")
                                         )
                                     )
                            ),
                            
                            # automatic tab
                            tabPanel("Automatic", value = "automatic",
                                     div(class = "dashboard-card", style = "margin-top: 20px;",
                                         div(class = "dashboard-card__header", "Automatic (Database)"),
                                         div(class = "dashboard-card__content",
                                             
                                             div(class = "govuk-form-group",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Reference month"),
                                                 div(class = "govuk-hint", "Auto-detected from database. Edit to override."),
                                                 uiOutput("month_status")
                                             ),
                                             div(class = "govuk-form-group", style = "display: flex; align-items: center; gap: 8px; flex-wrap: nowrap;",
                                                 span(style = "font-size: 14px; white-space: nowrap;",
                                                      "OFFICIAL-SENSITIVE \u2013 Internal Briefing \u2013 For external lines please contact"),
                                                 textInput("auto_contact_names", label = NULL,
                                                           value = "",
                                                           placeholder = "e.g. John Smith and Jane Doe", width = "280px")
                                             ),
                                             
                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Period selection"),
                                             div(class = "period-block",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Vacancies"),
                                                 uiOutput("auto_vac_period_buttons")
                                             ),
                                             div(class = "period-block",
                                                 tags$label(class = "govuk-label", style = "font-weight:700;", "Payroll employees"),
                                                 uiOutput("auto_pay_period_buttons")
                                             ),
                                             
                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Preview"),
                                             actionButton("preview_dashboard", "Dashboard", class = "govuk-button govuk-button--blue"),
                                             actionButton("preview_topten", "Top Ten", class = "govuk-button govuk-button--blue"),
                                             actionButton("auto_preview_summary", "Summary", class = "govuk-button govuk-button--blue"),
                                             actionButton("auto_preview_oecd", "OECD", class = "govuk-button govuk-button--blue"),
                                             actionButton("auto_jump_charts", "Charts",
                                                          class = "govuk-button govuk-button--blue",
                                                          onclick = "document.getElementById('charts_card').scrollIntoView({behavior:'smooth', block:'start'})"),
                                             actionButton("auto_jump_custom", "Custom",
                                                          class = "govuk-button govuk-button--blue",
                                                          onclick = "document.getElementById('custom_indicators_card').scrollIntoView({behavior:'smooth', block:'start'})"),

                                             tags$hr(class = "govuk-section-break"),
                                             
                                             h2(class = "govuk-heading-m", "Download"),
                                             downloadButton("download_word", "Download Word", class = "govuk-button govuk-button--blue"),
                                             downloadButton("download_excel", "Download Excel", class = "govuk-button")
                                         )
                                     )
                            )
                )
      )
  ),
  
  # preview area
  div(class = "govuk-width-container govuk-width-container--wide",
      tags$main(class = "govuk-main-wrapper", style = "padding-top: 0;",
                div(class = "dashboard-card",
                    div(class = "dashboard-card__header", "Dashboard Preview"),
                    div(class = "dashboard-card__content preview-scroll", uiOutput("dashboard_preview"))
                ),
                div(class = "dashboard-card",
                    div(class = "dashboard-card__header", "Top Ten Statistics Preview"),
                    div(class = "dashboard-card__content", uiOutput("topten_preview"))
                ),
                div(class = "dashboard-card",
                    div(class = "dashboard-card__header", "Summary Preview"),
                    div(class = "dashboard-card__content", uiOutput("summary_preview"))
                ),
                div(class = "dashboard-card",
                    div(class = "dashboard-card__header", "OECD International Comparisons"),
                    div(class = "dashboard-card__content preview-scroll", uiOutput("oecd_preview"))
                ),
                div(class = "dashboard-card", id = "charts_card",
                    div(class = "dashboard-card__header", "Key Charts Preview"),
                    div(class = "dashboard-card__content",
                        # Quick presets — apply From/To to every currently-ticked chart.
                        div(style = "margin-bottom:18px;",
                            tags$label(class = "govuk-label govuk-label--s", "Quick presets"),
                            div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:6px;",
                                actionButton("preset_last5",       "Last 5 years",  class = "govuk-button govuk-button--blue"),
                                actionButton("preset_since_covid", "Since COVID",   class = "govuk-button govuk-button--blue"),
                                actionButton("preset_since_2010",  "Since 2010",    class = "govuk-button govuk-button--blue"),
                                actionButton("preset_full",        "Full history",  class = "govuk-button govuk-button--blue")),
                            div(class = "govuk-hint", style = "margin-top:6px;",
                                "Applies From/To to every chart currently ticked below.")),
                        # Per-metric rows. INVARIANT: every metric's inputs are rendered
                        # up-front (visibility controlled by conditionalPanel) so preset
                        # buttons can call updateSelectInput on them even when collapsed.
                        uiOutput("chart_metric_rows"),
                        # Chart any LMS series (same search as the explorer)
                        conditionalPanel(
                          condition = "output.lms_loaded == true",
                          div(style = "border-top:2px solid #1F4E79; margin-top:16px; padding-top:12px;",
                              tags$strong("Chart any LMS series"),
                              div(class = "govuk-hint", style = "margin:2px 0 6px;",
                                  "Search the uploaded LMS file and add any series as a native Word chart."),
                              selectizeInput("chart_lms_pick", NULL,
                                             choices = NULL, multiple = TRUE, width = "100%",
                                             options = list(placeholder = "Type a CDID or title...",
                                                            maxOptions = 25)),
                              uiOutput("chart_lms_rows"))),
                        # Generate + preview
                        div(style = "margin-top:18px;",
                            actionButton("regenerate_charts", "Generate charts",
                                         class = "govuk-button govuk-button--blue",
                                         style = "width:100%;"),
                            div(class = "govuk-hint", style = "margin-top:10px;",
                                "Click after changing any selection. The same charts are appended to the Word briefing.")),
                        plotOutput("charts_preview", height = "auto")
                    )
                ),
                div(class = "dashboard-card", id = "custom_indicators_card",
                    div(class = "dashboard-card__header", "Custom Indicators Preview"),
                    div(class = "dashboard-card__content",
                        conditionalPanel(
                          condition = "output.lms_loaded == false",
                          div(class = "govuk-hint",
                              "Upload the LMS bulk time-series file to enable. ",
                              "Use the file upload above (any filename containing 'lms' is auto-detected).")),
                        conditionalPanel(
                          condition = "output.lms_loaded == true",
                          selectizeInput("lms_pick",
                                         "Add a series (search by CDID or title)",
                                         choices = NULL, multiple = TRUE, width = "100%",
                                         options = list(placeholder = "Type to search...",
                                                        maxOptions = 25)),
                          selectizeInput("lms_family_pick",
                                         "Or add a whole breakdown (search by metric, e.g. \"vacancies\")",
                                         choices = NULL, multiple = FALSE, width = "100%",
                                         options = list(placeholder = "Type a metric to find its breakdown families...",
                                                        maxOptions = 25)),
                          conditionalPanel(
                            condition = "input.lms_pick && input.lms_pick.length > 0",
                            div(style = "text-align:right; margin:-4px 0 6px;",
                                actionLink("lms_clear", "Clear all selected series"))),
                          uiOutput("lms_settings_rows"),
                          div(style = "margin-top:18px;",
                              actionButton("regenerate_custom", "Refresh preview",
                                           class = "govuk-button govuk-button--blue",
                                           style = "width:100%;"),
                              div(class = "govuk-hint", style = "margin-top:10px;",
                                  "Click after changing any selection. These rows are appended to the Word briefing.")),
                          h3(class = "govuk-heading-s", style = "margin-top:18px;",
                             "Table preview"),
                          uiOutput("lms_table_preview"),
                          h3(class = "govuk-heading-s", style = "margin-top:18px;",
                             "Summary lines"),
                          uiOutput("lms_summary_preview"))))
      )
  ),
  
  tags$footer(class = "govuk-footer",
              div(class = "govuk-width-container",
                  "Labour Market Statistics Brief Generator | Department for Business and Trade"
              )
  )
)

server <- function(input, output, session) {
  
  # script and template paths
  config_path       <- "utils/config.R"
  calculations_path <- "utils/calculations.R"
  excel_calc_path   <- "utils/calculations_from_excel.R"
  word_script_path  <- "utils/word_output.R"
  excel_script_path <- "sheets/excel_audit_workbook.R"
  summary_path      <- "sheets/summary.R"
  top_ten_path      <- "sheets/top_ten_stats.R"
  template_path     <- "utils/ManualDB.docx"
  
  # a01 is the minimum required upload
  has_uploads <- function() {
    !is.null(uploaded_files$a01)
  }
  
  has_any_upload <- function() {
    !is.null(uploaded_files$a01) || !is.null(uploaded_files$hr1) ||
      !is.null(uploaded_files$x09) || !is.null(uploaded_files$rtisa) ||
      !is.null(uploaded_files$cla01) || !is.null(uploaded_files$x02) ||
      !is.null(uploaded_files$oecd_unemp)
  }
  
  dashboard_data <- reactiveVal(NULL)
  topten_data <- reactiveVal(NULL)
  
  # uploaded file paths, null means not yet uploaded
  uploaded_files <- reactiveValues(
    a01 = NULL,
    hr1 = NULL,
    x09 = NULL,
    rtisa = NULL,
    cla01 = NULL,
    x02 = NULL,
    oecd_unemp = NULL,
    oecd_emp = NULL,
    oecd_inact = NULL,
    lms = NULL
  )
  
  # oecd data auto-fetched from the oecd.* postgres tables (same source
  # auto mode uses - see .fetch_oecd_table in the auto preview block).
  # uploaded files still take precedence; see .oecd_source() resolver.
  oecd_auto <- reactiveValues(
    unemp_df = NULL,
    emp_df = NULL,
    inact_df = NULL,
    fetched_at = NULL,
    failed_any = FALSE
  )
  
  # resolves a user-uploaded override path, else a pre-fetched data.frame
  # (country/period/value). Downstream handlers accept either format.
  .oecd_source <- function(metric) {
    uploaded <- switch(metric,
                       unemp = uploaded_files$oecd_unemp,
                       emp   = uploaded_files$oecd_emp,
                       inact = uploaded_files$oecd_inact,
                       NULL)
    if (!is.null(uploaded)) return(uploaded)
    switch(metric,
           unemp = oecd_auto$unemp_df,
           emp   = oecd_auto$emp_df,
           inact = oecd_auto$inact_df,
           NULL)
  }
  
  # warn if uploaded file doesn't have expected sheets
  .validate_excel <- function(path, expected_sheets, file_label) {
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    missing <- setdiff(expected_sheets, sheets)
    if (length(missing) > 0) {
      showNotification(
        paste0("This doesn't look like a ", file_label, " file. Missing sheets: ",
               paste(missing, collapse = ", ")),
        type = "warning", duration = 10
      )
    }
  }
  
  # detect file type from filename or sheet contents
  .detect_file_type <- function(name, path) {
    nm <- tolower(name)
    if (grepl("cla01", nm)) return("cla01")
    if (grepl("rtisa", nm)) return("rtisa")
    if (grepl("oecd", nm)) return(.detect_oecd_type(nm, path))
    if (grepl("a01", nm)) return("a01")
    if (grepl("hr1", nm)) return("hr1")
    if (grepl("x09", nm)) return("x09")
    if (grepl("x02", nm)) return("x02")
    if (grepl("lms", nm)) return("lms")
    sheets <- tryCatch(tolower(readxl::excel_sheets(path)), error = function(e) character(0))
    if (any(sheets %in% c("1", "10", "13", "15", "19"))) return("a01")
    if (any(sheets %in% c("1a"))) return("hr1")
    if (any(grepl("awe real_cpi", sheets))) return("x09")
    if (any(grepl("payrolled employees", sheets))) return("rtisa")
    if (any(grepl("claimant", sheets)) || any(grepl("people sa", sheets))) return("cla01")
    if (any(grepl("labour market flows", sheets))) return("x02")
    # LMS bulk file content check: sheet "data" + row-2/col-A literal "CDID"
    if ("data" %in% sheets) {
      cdid_cell <- tryCatch(suppressMessages(readxl::read_excel(
        path, sheet = "data", range = "A2:A2", col_names = FALSE,
        .name_repair = "minimal"))[[1]][1], error = function(e) NA)
      if (!is.na(cdid_cell) && toupper(trimws(as.character(cdid_cell))) == "CDID")
        return("lms")
    }
    # last resort: check contents for oecd files with non-standard names
    oecd_result <- .detect_oecd_metric_from_content(path)
    if (!is.null(oecd_result)) return(oecd_result)
    NULL
  }
  
  # oecd downloads often have identical filenames, so check content to distinguish
  .detect_oecd_metric_from_content <- function(path) {
    tryCatch({
      ext <- tolower(tools::file_ext(path))
      if (ext %in% c("xlsx", "xls")) {
        sheets <- readxl::excel_sheets(path)
        tbl_sheet <- if ("Table" %in% sheets) "Table" else NULL
        if (!is.null(tbl_sheet)) {
          tbl <- suppressMessages(readxl::read_excel(path, sheet = tbl_sheet,
                                                     col_names = FALSE, n_max = 10))
          all_text <- tolower(paste(unlist(tbl), collapse = " "))
          if (grepl("unemployment", all_text)) return("oecd_unemp")
          if (grepl("inactivity", all_text))   return("oecd_inact")
          if (grepl("employment", all_text))   return("oecd_emp")
        }
        # try sdmx measure column
        check_sheet <- if (length(sheets) > 0) sheets[1] else NULL
        if (!is.null(check_sheet)) {
          raw <- suppressMessages(readxl::read_excel(path, sheet = check_sheet, n_max = 5))
          measure_cols <- intersect(c("MEASURE", "Measure"), names(raw))
          if (length(measure_cols) > 0) {
            vals <- tolower(paste(unique(unlist(raw[measure_cols])), collapse = " "))
            if (grepl("une", vals))          return("oecd_unemp")
            if (grepl("inact|olf", vals))    return("oecd_inact")
            if (grepl("emp_wap|emp", vals))  return("oecd_emp")
          }
        }
      } else if (ext == "csv") {
        raw <- read.csv(path, nrows = 50, stringsAsFactors = FALSE)
        # check both code and label measure columns for SDMX detection
        measure_cols <- intersect(c("MEASURE", "Measure"), names(raw))
        if (length(measure_cols) > 0) {
          vals <- tolower(paste(unique(unlist(raw[measure_cols])), collapse = " "))
          if (grepl("une", vals))          return("oecd_unemp")
          if (grepl("inact|olf", vals))    return("oecd_inact")
          if (grepl("emp_wap|emp", vals))  return("oecd_emp")
        }
        all_text <- tolower(paste(c(names(raw), unlist(raw[1:min(5, nrow(raw)), ])), collapse = " "))
        if (grepl("unemployment", all_text)) return("oecd_unemp")
        if (grepl("inactivity", all_text))   return("oecd_inact")
        if (grepl("employment", all_text))   return("oecd_emp")
      }
      NULL
    }, error = function(e) NULL)
  }
  
  # oecd sub-type: filename first, then content
  .detect_oecd_type <- function(nm, path = NULL) {
    if (grepl("unemp", nm))                      return("oecd_unemp")
    if (grepl("emp", nm) && !grepl("unemp", nm)) return("oecd_emp")
    if (grepl("inact", nm))                      return("oecd_inact")
    if (!is.null(path)) {
      result <- .detect_oecd_metric_from_content(path)
      if (!is.null(result)) return(result)
    }
    NULL
  }
  
  # handle file uploads
  observeEvent(input$upload_files, {
    files <- input$upload_files
    if (is.null(files)) return()
    
    detected_types <- character(0)
    
    for (i in seq_len(nrow(files))) {
      ftype <- .detect_file_type(files$name[i], files$datapath[i])
      if (is.null(ftype)) {
        showNotification(
          paste0("Could not identify file: ", files$name[i],
                 ". OECD files are auto-detected by content; other files need ",
                 "recognisable names (A01, HR1, X09, RTISA)."),
          type = "warning", duration = 10
        )
        next
      }
      
      # store by type
      if (ftype == "a01") {
        .validate_excel(files$datapath[i], c("1", "10", "13", "15", "19"), "A01")
        uploaded_files$a01 <- files$datapath[i]
        # try to detect reference month from a01
        tryCatch({
          if (!exists(".detect_manual_month_from_a01", inherits = TRUE)) {
            source("utils/calculations_from_excel.R", local = FALSE)
          }
          detected <- .detect_manual_month_from_a01(files$datapath[i])
          if (!is.null(detected)) .update_ref_month(detected)
        }, error = function(e) NULL)
      } else if (ftype == "hr1") {
        .validate_excel(files$datapath[i], c("1a"), "HR1")
        uploaded_files$hr1 <- files$datapath[i]
      } else if (ftype == "x09") {
        .validate_excel(files$datapath[i], c("AWE Real_CPI"), "X09")
        uploaded_files$x09 <- files$datapath[i]
      } else if (ftype == "rtisa") {
        .validate_excel(files$datapath[i], c("1. Payrolled employees (UK)", "23. Employees (Industry)"), "RTISA")
        uploaded_files$rtisa <- files$datapath[i]
      } else if (ftype == "cla01") {
        uploaded_files$cla01 <- files$datapath[i]
      } else if (ftype == "x02") {
        uploaded_files$x02 <- files$datapath[i]
      } else if (ftype == "oecd_unemp") {
        uploaded_files$oecd_unemp <- files$datapath[i]
      } else if (ftype == "oecd_emp") {
        uploaded_files$oecd_emp <- files$datapath[i]
      } else if (ftype == "oecd_inact") {
        uploaded_files$oecd_inact <- files$datapath[i]
      } else if (ftype == "lms") {
        uploaded_files$lms <- files$datapath[i]
        # Parse the catalog synchronously (rows 1-7 + col A only, sub-second).
        res <- tryCatch(parse_lms_catalog(files$datapath[i]),
                        error = function(e) list(ok = FALSE, warn = conditionMessage(e)))
        if (isTRUE(res$ok)) {
          # read the whole sheet into memory once so later lookups are instant
          withProgress(message = "Loading LMS data into memory...", value = 0.5,
                       lms_preload(files$datapath[i]))
          fams <- tryCatch(lms_build_families(res$catalog),
                           error = function(e) list(families = list(), fam_of = NULL))
          lms_catalog_data(list(catalog = res$catalog, periods = res$periods,
                                path = files$datapath[i], families = fams))
          cat0 <- res$catalog
          choices <- setNames(cat0$cdid, paste0(cat0$cdid, " | ", cat0$title))
          updateSelectizeInput(session, "lms_pick",
                               choices = choices, server = TRUE,
                               selected = character(0))
          updateSelectizeInput(session, "chart_lms_pick",
                               choices = choices, server = TRUE,
                               selected = character(0))
          fam_choices <- if (length(fams$families))
            setNames(as.character(seq_along(fams$families)),
                     vapply(fams$families,
                            function(g) sprintf("%s  (%d series)", g$label, g$n), "")) else NULL
          updateSelectizeInput(session, "lms_family_pick",
                               choices = fam_choices, server = TRUE,
                               selected = character(0))
          if (length(fams$families))
            showNotification(sprintf("%d breakdown families detected.", length(fams$families)),
                             type = "message", duration = 4)
          for (w in res$warn) showNotification(w, type = "warning", duration = 10)
        } else {
          showNotification(
            paste0("LMS file rejected: ", paste(res$warn, collapse = "; ")),
            type = "warning", duration = 10)
        }
      }

      .oecd_friendly <- c(oecd_unemp = "OECD Unemployment Rate",
                          oecd_emp = "OECD Employment Rate",
                          oecd_inact = "OECD Inactivity Rate",
                          lms = "LMS Bulk Series")
      display <- if (ftype %in% names(.oecd_friendly)) .oecd_friendly[[ftype]] else toupper(ftype)
      detected_types <- c(detected_types, display)
    }
    
    if (length(detected_types) > 0) {
      showNotification(
        paste0("Detected: ", paste(detected_types, collapse = ", ")),
        type = "message", duration = 5
      )
    }
  })
  
  # auto-fetch oecd from the postgres oecd.* tables once on session start.
  # This is the same data source the Automatic tab uses, so as long as the
  # DB is reachable the fetch cannot fail (no external HTTP dependency).
  observe({
    if (!is.null(oecd_auto$fetched_at)) return()  # already attempted
    if (!exists("fetch_oecd_from_db", inherits = TRUE)) {
      source("utils/helpers.R", local = FALSE)
    }
    withProgress(message = "Fetching OECD data from database\u2026", value = 0.1, {
      res <- tryCatch(fetch_oecd_from_db(verbose = TRUE),
                      error = function(e) list(unemp = NULL, emp = NULL, inact = NULL))
      incProgress(0.9, detail = "done")
      oecd_auto$unemp_df <- res$unemp
      oecd_auto$emp_df   <- res$emp
      oecd_auto$inact_df <- res$inact
    })
    oecd_auto$fetched_at <- Sys.time()
    oecd_auto$failed_any <- is.null(oecd_auto$unemp_df) ||
      is.null(oecd_auto$emp_df)   ||
      is.null(oecd_auto$inact_df)
  })
  
  # status banner: green tick with timestamp if ok, amber warning on partial/total failure
  output$oecd_auto_status <- renderUI({
    ts <- oecd_auto$fetched_at
    if (is.null(ts)) {
      return(div(class = "govuk-hint", style = "font-size: 16px;",
                 "\u23F3 Fetching OECD data\u2026"))
    }
    stamp <- format(ts, "%H:%M")
    if (isTRUE(oecd_auto$failed_any)) {
      div(class = "govuk-inset-text", style = "border-left-color: #f47738; padding: 10px 14px; margin: 14px 0;",
          tags$strong("OECD auto-fetch failed."),
          " Upload the 3 OECD CSVs manually to override.")
    } else {
      div(class = "govuk-inset-text", style = "border-left-color: #00703c; padding: 10px 14px; margin: 14px 0;",
          tags$strong("\u2713 OECD data auto-fetched"),
          paste0(" at ", stamp, " \u2014 upload a CSV only to override."))
    }
  })
  
  output$upload_status <- renderUI({
    all_files <- list(
      A01 = uploaded_files$a01, HR1 = uploaded_files$hr1,
      RTISA = uploaded_files$rtisa, X09 = uploaded_files$x09,
      CLA01 = uploaded_files$cla01, X02 = uploaded_files$x02,
      `OECD UE` = uploaded_files$oecd_unemp,
      `OECD Emp` = uploaded_files$oecd_emp,
      `OECD Inact` = uploaded_files$oecd_inact
    )
    # landing pages for grey tags (users browse + download manually)
    landing <- c(
      A01   = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/summaryoflabourmarketstatistics",
      HR1   = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/hr1potentialredundancies",
      X09   = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/x09realaverageweeklyearningsusingconsumerpriceinflationseasonallyadjusted",
      RTISA = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted",
      CLA01 = "https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/outofworkbenefits/datasets/claimantcountcla01",
      X02   = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/labourforcesurveyflowsestimatesx02",
      OECD_UE    = "https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEmployment%23JOB%23%7CUnemployment%20indicators%23JOB_UNEMP%23&fs[1]=Frequency%20of%20observation%2C0%7CQuarterly%23Q%23&fs[2]=Measure%2C0%7CUnemployment%23UNE%23&fs[3]=Measure%2C0%7CUnemployment%20rate%23UNE_LF%23&pg=0&fc=Measure&snb=1&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_LFS%40DF_IALFS_INDIC&df[ag]=OECD.SDD.TPS&df[vs]=1.0&dq=EA20%2BUSA%2BGBR%2BESP%2BJPN%2BITA%2BDEU%2BFRA%2BCAN.UNE_LF.PT_LF_SUB..Y._T.Y_GE15..Q&pd=2024-Q1%2C&to[TIME_PERIOD]=false",
      OECD_EMP   = "https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEmployment%23JOB%23%7CUnemployment%20indicators%23JOB_UNEMP%23&fs[1]=Frequency%20of%20observation%2C0%7CQuarterly%23Q%23&fs[2]=Measure%2C0%7CUnemployment%23UNE%23&fs[3]=Measure%2C0%7CUnemployment%20rate%23UNE_LF%23&pg=0&fc=Measure&snb=1&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_LFS%40DF_IALFS_INDIC&df[ag]=OECD.SDD.TPS&df[vs]=1.0&dq=EA20%2BUSA%2BGBR%2BESP%2BJPN%2BITA%2BDEU%2BFRA%2BCAN.EMP_WAP.PT_WAP_SUB..Y._T.Y15T64..Q&pd=2024-Q1%2C&to[TIME_PERIOD]=false",
      OECD_INACT = "https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEmployment%23JOB%23%7CUnemployment%20indicators%23JOB_UNEMP%23&fs[1]=Frequency%20of%20observation%2C0%7CQuarterly%23Q%23&fs[2]=Measure%2C0%7CUnemployment%23UNE%23&fs[3]=Measure%2C0%7CUnemployment%20rate%23UNE_LF%23&pg=0&fc=Measure&snb=1&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_LFS%40DF_IALFS_INDIC&df[ag]=OECD.SDD.TPS&df[vs]=1.0&dq=EA20%2BUSA%2BGBR%2BESP%2BJPN%2BITA%2BDEU%2BFRA%2BCAN.OLF_WAP.PT_WAP_SUB..Y._T.Y15T64..Q&pd=2024-Q1%2C&to[TIME_PERIOD]=false"
    )
    # map display names to url keys
    url_key_map <- c(A01 = "A01", HR1 = "HR1", RTISA = "RTISA", X09 = "X09",
                     CLA01 = "CLA01", X02 = "X02",
                     `OECD UE` = "OECD_UE", `OECD Emp` = "OECD_EMP", `OECD Inact` = "OECD_INACT")
    # metric key for oecd auto-fetch lookup
    oecd_metric_map <- c(`OECD UE` = "unemp", `OECD Emp` = "emp", `OECD Inact` = "inact")
    file_tags <- lapply(names(all_files), function(nm) {
      url_key <- url_key_map[[nm]]
      metric <- oecd_metric_map[nm]
      if (!is.null(all_files[[nm]])) {
        # manual override uploaded
        span(class = "govuk-tag govuk-tag--green",
             paste0(nm, " \u2713"))
      } else if (!is.na(metric)) {
        # oecd tag: auto-fetched (blue) or failed fallback (orange), always linked to landing page
        auto_df <- switch(metric,
                          unemp = oecd_auto$unemp_df,
                          emp   = oecd_auto$emp_df,
                          inact = oecd_auto$inact_df, NULL)
        tag_cls <- if (!is.null(auto_df)) "govuk-tag govuk-tag--blue" else "govuk-tag govuk-tag--orange"
        label <- if (!is.null(auto_df)) paste0(nm, " \u2014 Auto") else paste0(nm, " \u2014 Fetch failed")
        tags$a(href = landing[[url_key]], target = "_blank",
               style = "text-decoration: none;",
               span(class = tag_cls, style = "cursor: pointer;", label))
      } else if (!is.null(url_key) && url_key %in% names(landing)) {
        tags$a(href = landing[[url_key]], target = "_blank",
               style = "text-decoration: none;",
               span(class = "govuk-tag govuk-tag--grey", style = "cursor: pointer;", nm))
      } else {
        span(class = "govuk-tag govuk-tag--grey", nm)
      }
    })
    mm <- reference_manual_month()
    month_line <- if (!is.null(mm) && nzchar(mm) && !is.null(uploaded_files$a01)) {
      div(style = "margin-top: 10px; font-weight: 600;",
          paste0("Reference month: ", manual_month_to_display(mm)))
    }
    div(div(class = "file-tag-row", tagList(file_tags)), month_line)
  })
  
  # detect vacancies periods from a01
  observeEvent(uploaded_files$a01, {
    path <- uploaded_files$a01
    if (is.null(path)) return()
    
    tryCatch({
      mm <- reference_manual_month()
      if (is.null(mm) || !nzchar(mm)) return()
      
      mm_mon3 <- substr(gsub("[^a-z]", "", mm), 1, 3)
      mm_yr   <- as.integer(substr(gsub("[^0-9]", "", mm), 1, 4))
      mm_m    <- match(mm_mon3, tolower(month.abb))
      mm_date <- as.Date(sprintf("%04d-%02d-01", mm_yr, mm_m))
      lfs_end <- add_months(mm_date, -2)
      
      tbl_19 <- readxl::read_excel(path, sheet = "19", col_names = FALSE, .name_repair = "minimal")
      periods <- trimws(as.character(tbl_19[[1]]))
      # parse "Mmm-Mmm YYYY" labels to end dates
      parsed <- vapply(periods, function(x) {
        m <- regmatches(x, gregexpr("[A-Za-z]{3}", x))[[1]]
        y <- regmatches(x, gregexpr("[0-9]{4}", x))[[1]]
        if (length(m) >= 2 && length(y) >= 1) {
          month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
          em <- month_map[tolower(m[2])]
          if (!is.na(em)) return(as.numeric(as.Date(sprintf("%s-%02d-01", y[1], em))))
        }
        NA_real_
      }, numeric(1))
      
      ok <- !is.na(parsed) & !is.na(suppressWarnings(as.numeric(tbl_19[[3]])))
      if (!any(ok)) return()
      
      ends <- as.Date(parsed[ok], origin = "1970-01-01")
      end_latest <- max(ends)
      aligned_candidates <- ends[ends <= lfs_end]
      end_aligned <- if (length(aligned_candidates) > 0) max(aligned_candidates) else end_latest
      
      make_label <- function(end_d) {
        start_d <- add_months(end_d, -2)
        paste0(format(start_d, "%b"), "-", format(end_d, "%b"), " ", format(end_d, "%Y"))
      }
      
      lab_aligned <- make_label(end_aligned)
      lab_latest  <- make_label(end_latest)
      
      manual_period_labels$vac_aligned <- lab_aligned
      manual_period_labels$vac_latest  <- lab_latest
    }, error = function(e) NULL)
  })
  
  # detect payroll periods from rtisa
  observeEvent(uploaded_files$rtisa, {
    path <- uploaded_files$rtisa
    if (is.null(path)) return()
    
    tryCatch({
      mm <- reference_manual_month()
      if (is.null(mm) || !nzchar(mm)) return()
      
      mm_mon3 <- substr(gsub("[^a-z]", "", mm), 1, 3)
      mm_yr   <- as.integer(substr(gsub("[^0-9]", "", mm), 1, 4))
      mm_m    <- match(mm_mon3, tolower(month.abb))
      mm_date <- as.Date(sprintf("%04d-%02d-01", mm_yr, mm_m))
      lfs_end <- add_months(mm_date, -2)
      
      rtisa_pay <- readxl::read_excel(path, sheet = "1. Payrolled employees (UK)", col_names = FALSE, .name_repair = "minimal")
      text_col <- trimws(as.character(rtisa_pay[[1]]))
      parsed <- suppressWarnings(lubridate::parse_date_time(text_col, orders = c("B Y", "bY", "BY")))
      months_parsed <- lubridate::floor_date(as.Date(parsed), "month")
      vals <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(rtisa_pay[[2]]))))
      
      ok <- !is.na(months_parsed) & !is.na(vals)
      if (!any(ok)) return()
      
      avail <- months_parsed[ok]
      end_latest <- max(avail)
      aligned_candidates <- avail[avail <= lfs_end]
      end_aligned <- if (length(aligned_candidates) > 0) max(aligned_candidates) else end_latest
      
      make_label <- function(end_d) {
        start_d <- add_months(end_d, -2)
        paste0(format(start_d, "%b"), "-", format(end_d, "%b"), " ", format(end_d, "%Y"))
      }
      
      lab_aligned <- make_label(end_aligned)
      lab_latest  <- make_label(end_latest)
      
      manual_period_labels$pay_aligned <- lab_aligned
      manual_period_labels$pay_latest  <- lab_latest
    }, error = function(e) NULL)
  })
  
  reference_manual_month <- reactiveVal(NULL)
  manual_period_labels <- reactiveValues(
    vac_aligned = NULL, vac_latest = NULL,
    pay_aligned = NULL, pay_latest = NULL
  )
  selected_vac_period <- reactiveVal("latest")
  selected_pay_period <- reactiveVal("latest")
  period_labels <- reactiveVal(list(
    vac = list(aligned = NULL, latest = NULL),
    payroll = list(aligned = NULL, latest = NULL)
  ))
  
  # period toggle buttons
  output$manual_vac_period_buttons <- renderUI({
    aligned <- manual_period_labels$vac_aligned
    latest  <- manual_period_labels$vac_latest
    if (is.null(aligned) || !nzchar(aligned)) return(p(class = "govuk-hint", "Upload A01 first"))
    sel <- selected_vac_period()
    div(class = "period-toggle-group",
        actionButton("vac_period_aligned", aligned,
                     class = paste("govuk-button", if (sel == "aligned") "active" else "")),
        if (!identical(aligned, latest))
          actionButton("vac_period_latest", latest,
                       class = paste("govuk-button", if (sel == "latest") "active" else ""))
    )
  })
  
  output$manual_pay_period_buttons <- renderUI({
    aligned <- manual_period_labels$pay_aligned
    latest  <- manual_period_labels$pay_latest
    if (is.null(aligned) || !nzchar(aligned)) return(p(class = "govuk-hint", "Upload RTISA first"))
    sel <- selected_pay_period()
    div(class = "period-toggle-group",
        actionButton("pay_period_aligned", aligned,
                     class = paste("govuk-button", if (sel == "aligned") "active" else "")),
        if (!identical(aligned, latest))
          actionButton("pay_period_latest", latest,
                       class = paste("govuk-button", if (sel == "latest") "active" else ""))
    )
  })
  
  observeEvent(input$vac_period_aligned, { selected_vac_period("aligned") })
  observeEvent(input$vac_period_latest,  { selected_vac_period("latest") })
  observeEvent(input$pay_period_aligned, { selected_pay_period("aligned") })
  observeEvent(input$pay_period_latest,  { selected_pay_period("latest") })
  
  # auto tab period toggles
  auto_selected_vac_period <- reactiveVal("latest")
  auto_selected_pay_period <- reactiveVal("latest")
  
  output$auto_vac_period_buttons <- renderUI({
    labs <- period_labels()
    aligned <- labs$vac$aligned
    latest  <- labs$vac$latest
    if (is.null(aligned) || !nzchar(aligned)) return(p(class = "govuk-hint", "Loading periods..."))
    sel <- auto_selected_vac_period()
    div(class = "period-toggle-group",
        actionButton("auto_vac_period_aligned", aligned,
                     class = paste("govuk-button", if (sel == "aligned") "active" else "")),
        if (!identical(aligned, latest))
          actionButton("auto_vac_period_latest", latest,
                       class = paste("govuk-button", if (sel == "latest") "active" else ""))
    )
  })
  
  output$auto_pay_period_buttons <- renderUI({
    labs <- period_labels()
    aligned <- labs$payroll$aligned
    latest  <- labs$payroll$latest
    if (is.null(aligned) || !nzchar(aligned)) return(p(class = "govuk-hint", "Loading periods..."))
    sel <- auto_selected_pay_period()
    div(class = "period-toggle-group",
        actionButton("auto_pay_period_aligned", aligned,
                     class = paste("govuk-button", if (sel == "aligned") "active" else "")),
        if (!identical(aligned, latest))
          actionButton("auto_pay_period_latest", latest,
                       class = paste("govuk-button", if (sel == "latest") "active" else ""))
    )
  })
  
  observeEvent(input$auto_vac_period_aligned, { auto_selected_vac_period("aligned") })
  observeEvent(input$auto_vac_period_latest,  { auto_selected_vac_period("latest") })
  observeEvent(input$auto_pay_period_aligned, { auto_selected_pay_period("aligned") })
  observeEvent(input$auto_pay_period_latest,  { auto_selected_pay_period("latest") })
  
  .auto_selected_vac_label <- function() {
    labs <- period_labels()
    if (auto_selected_vac_period() == "latest") labs$vac$latest else labs$vac$aligned
  }
  .auto_selected_pay_label <- function() {
    labs <- period_labels()
    if (auto_selected_pay_period() == "latest") labs$payroll$latest else labs$payroll$aligned
  }
  
  # date helpers
  add_months <- function(d, n) {
    d <- as.Date(d)
    if (is.na(d)) return(as.Date(NA))
    if (n == 0) return(d)
    if (n > 0) return(as.Date(seq(d, by = "month", length.out = n + 1)[n + 1]))
    as.Date(seq(d, by = paste0(n, " months"), length.out = 2)[2])
  }
  
  parse_lfs_end <- function(label) {
    x <- trimws(as.character(label))
    month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
    months_found <- regmatches(x, gregexpr("[A-Za-z]{3}", x))[[1]]
    year_found <- regmatches(x, gregexpr("[0-9]{4}", x))[[1]]
    if (length(months_found) >= 2 && length(year_found) >= 1) {
      end_month <- month_map[tolower(months_found[2])]
      yr <- as.integer(year_found[1])
      if (!is.na(end_month) && !is.na(yr)) return(as.Date(sprintf("%04d-%02d-01", yr, end_month)))
    }
    as.Date(NA)
  }
  
  manual_month_from_date <- function(d) {
    tolower(paste0(format(d, "%b"), format(d, "%Y")))
  }
  
  manual_month_to_display <- function(mm) {
    # e.g. "dec2025" -> "December 2025"
    mm <- tolower(gsub("[[:space:]]+", "", as.character(mm)))
    mon3 <- substr(gsub("[^a-z]", "", mm), 1, 3)
    yr <- as.integer(substr(gsub("[^0-9]", "", mm), 1, 4))
    m <- match(mon3, tolower(month.abb))
    if (is.na(m) || is.na(yr)) return(mm)
    format(as.Date(sprintf("%04d-%02d-01", yr, m)), "%B %Y")
  }
  
  mode_from_choice <- function(choice, labs) {
    if (!is.null(labs$latest) && identical(choice, labs$latest)) "latest" else "aligned"
  }
  
  # parse period label to its end-month date
  .parse_period_end <- function(label) {
    if (is.null(label) || !nzchar(label)) return(NULL)
    d <- parse_lfs_end(label)
    if (is.na(d)) return(NULL)
    d
  }
  
  # selected period label from toggle buttons
  .selected_vac_label <- function() {
    if (selected_vac_period() == "latest") manual_period_labels$vac_latest
    else manual_period_labels$vac_aligned
  }
  .selected_pay_label <- function() {
    if (selected_pay_period() == "latest") manual_period_labels$pay_latest
    else manual_period_labels$pay_aligned
  }
  
  # update reference month from auto-detected value
  .update_ref_month <- function(detected_mm) {
    old_mm <- reference_manual_month()
    reference_manual_month(detected_mm)
    if (!is.null(old_mm) && nzchar(old_mm) && !identical(tolower(old_mm), tolower(detected_mm))) {
      showNotification(
        paste0("Reference month auto-detected: ", manual_month_to_display(detected_mm),
               " (from uploaded A01)"),
        type = "message", duration = 8
      )
    }
  }
  
  # auto detect ref month on startup
  session$onFlushed(function() {
    
    showModal(modalDialog(
      div(
        div(class = "loader"),
        strong("Loading…"),
        div(style = "margin-top: 8px; color: #505a5f;", "Detecting latest reference month and periods")
      ),
      footer = NULL, easyClose = FALSE
    ))
    
    mm <- NULL
    
    # try latest lfs period from db
    if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RPostgres", quietly = TRUE)) {
      conn <- NULL
      tryCatch({
        conn <- DBI::dbConnect(RPostgres::Postgres())
        res <- DBI::dbGetQuery(conn, 'SELECT DISTINCT time_period FROM "ons"."labour_market__age_group"')
        if (nrow(res) > 0) {
          ends <- as.Date(vapply(res$time_period, parse_lfs_end, as.Date(NA)), origin = "1970-01-01")
          if (any(!is.na(ends))) {
            end_latest <- max(ends, na.rm = TRUE)
            mm_date <- add_months(end_latest, 2)
            mm <- manual_month_from_date(mm_date)
          }
        }
      }, error = function(e) NULL, finally = {
        if (!is.null(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
      })
    }
    
    # fallback to config
    if (is.null(mm) && file.exists(config_path)) {
      env <- new.env()
      tryCatch({
        source(config_path, local = env)
        if (exists("manual_month", envir = env)) mm <- tolower(env$manual_month)
      }, error = function(e) NULL)
    }
    
    if (is.null(mm) || !nzchar(mm)) {
      mm <- manual_month_from_date(Sys.Date())
    }
    
    reference_manual_month(mm)
    
    # lfs end is manual_month minus 2 months
    mm_mon3 <- substr(gsub("[^a-z]", "", mm), 1, 3)
    mm_yr <- as.integer(substr(gsub("[^0-9]", "", mm), 1, 4))
    mm_m <- match(mm_mon3, tolower(month.abb))
    mm_date <- as.Date(sprintf("%04d-%02d-01", mm_yr, mm_m))
    lfs_end <- add_months(mm_date, -2)
    
    # vacancies period labels
    vac_lab_aligned <- ""
    vac_lab_latest <- ""
    if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RPostgres", quietly = TRUE)) {
      conn <- NULL
      tryCatch({
        conn <- DBI::dbConnect(RPostgres::Postgres())
        res <- DBI::dbGetQuery(conn, 'SELECT DISTINCT time_period FROM "ons"."labour_market__vacancies_business"')
        if (nrow(res) > 0) {
          ends <- as.Date(vapply(res$time_period, parse_lfs_end, as.Date(NA)), origin = "1970-01-01")
          ok <- !is.na(ends)
          if (any(ok)) {
            end_latest <- max(ends[ok], na.rm = TRUE)
            end_aligned_candidates <- ends[ok & ends <= lfs_end]
            end_aligned <- if (length(end_aligned_candidates) >= 1) max(end_aligned_candidates) else end_latest
            
            # build labels
            make_lfs_label_local <- function(end_date) {
              start_date <- add_months(end_date, -2)
              paste0(format(start_date, "%b"), "-", format(end_date, "%b"), " ", format(end_date, "%Y"))
            }
            vac_lab_aligned <- make_lfs_label_local(end_aligned)
            vac_lab_latest  <- make_lfs_label_local(end_latest)
          }
        }
      }, error = function(e) NULL, finally = {
        if (!is.null(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
      })
    }
    
    # payroll period labels
    pay_lab_aligned <- ""
    pay_lab_latest <- ""
    if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RPostgres", quietly = TRUE)) {
      conn <- NULL
      tryCatch({
        conn <- DBI::dbConnect(RPostgres::Postgres())
        res <- DBI::dbGetQuery(conn, 'SELECT DISTINCT time_period FROM "ons"."labour_market__payrolled_employees"')
        if (nrow(res) > 0) {
          months <- suppressWarnings(as.Date(paste0("01 ", res$time_period), format = "%d %B %Y"))
          ok <- !is.na(months)
          if (any(ok)) {
            end_latest <- max(months[ok], na.rm = TRUE)
            end_aligned_candidates <- months[ok & months <= lfs_end]
            end_aligned <- if (length(end_aligned_candidates) >= 1) max(end_aligned_candidates) else end_latest
            
            make_lfs_label_local <- function(end_date) {
              start_date <- add_months(end_date, -2)
              paste0(format(start_date, "%b"), "-", format(end_date, "%b"), " ", format(end_date, "%Y"))
            }
            pay_lab_aligned <- make_lfs_label_local(end_aligned)
            pay_lab_latest  <- make_lfs_label_local(end_latest)
          }
        }
      }, error = function(e) NULL, finally = {
        if (!is.null(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
      })
    }
    
    period_labels(list(
      vac = list(aligned = vac_lab_aligned, latest = vac_lab_latest),
      payroll = list(aligned = pay_lab_aligned, latest = pay_lab_latest)
    ))
    
    removeModal()
  }, once = TRUE)
  
  # reference month display (auto tab)
  output$month_status <- renderUI({
    mm <- reference_manual_month()
    if (is.null(mm) || !nzchar(mm)) {
      return(div(style = "margin-top: 10px;", div(class = "loader")))
    }
    textInput("auto_ref_month_input", label = NULL,
              value = manual_month_to_display(mm),
              placeholder = "e.g. March 2026", width = "320px")
  })
  
  # sync manual tab month input from auto-detected value
  observe({
    mm <- reference_manual_month()
    if (!is.null(mm) && nzchar(mm)) {
      display <- manual_month_to_display(mm)
      updateTextInput(session, "ref_month_input", value = display)
    }
  })
  
  # parse typed month to "mar2026" format for internal use
  .parse_month_input <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    txt <- trimws(txt)
    # try "March 2026" format
    d <- suppressWarnings(as.Date(paste0("01 ", txt), format = "%d %B %Y"))
    if (!is.na(d)) return(tolower(paste0(format(d, "%b"), format(d, "%Y"))))
    # try "Mar 2026"
    d <- suppressWarnings(as.Date(paste0("01 ", txt), format = "%d %b %Y"))
    if (!is.na(d)) return(tolower(paste0(format(d, "%b"), format(d, "%Y"))))
    NULL
  }
  
  # update ref month when user types in manual tab
  observeEvent(input$ref_month_input, {
    parsed <- .parse_month_input(input$ref_month_input)
    if (!is.null(parsed)) reference_manual_month(parsed)
  }, ignoreInit = TRUE)
  
  # update ref month when user types in auto tab
  observeEvent(input$auto_ref_month_input, {
    parsed <- .parse_month_input(input$auto_ref_month_input)
    if (!is.null(parsed)) reference_manual_month(parsed)
  }, ignoreInit = TRUE)
  
  # set manual tab month if user hasn't typed yet
  observe({
    mm <- reference_manual_month()
    if (!is.null(mm) && nzchar(mm)) {
      display <- manual_month_to_display(mm)
      if (is.null(input$ref_month_input) || !nzchar(input$ref_month_input)) {
        updateTextInput(session, "ref_month_input", value = display)
      }
    }
  }) |> bindEvent(reference_manual_month(), once = TRUE)
  
  
  # preview: dashboard
  
  observeEvent(input$preview_dashboard, {
    
    withProgress(message = "loading dashboard...", value = 0, {
      
      incProgress(0.2, detail = "loading...")
      
      if (!file.exists(calculations_path)) {
        showNotification("Error: calculations.R not found", type = "error")
        return()
      }
      
      if (file.exists(config_path)) {
        source(config_path, local = FALSE)
      }
      
      mm <- reference_manual_month()
      if (!is.null(mm) && nzchar(mm)) {
        manual_month <<- tolower(mm)
      }
      
      # vacancies & payroll choices from toggle buttons
      vacancies_mode <<- auto_selected_vac_period()
      payroll_mode   <<- auto_selected_pay_period()
      
      incProgress(0.4, detail = "calculating...")
      
      tryCatch({
        source(calculations_path, local = FALSE)
      }, error = function(e) {
        showNotification(paste("Calculation error:", e$message), type = "error", duration = 5)
        return()
      })
      
      gv <- function(name) {
        if (exists(name, inherits = TRUE)) {
          val <- tryCatch(get(name, inherits = TRUE), error = function(e) NULL)
          if (is.numeric(val)) return(val)
        }
        NA_real_
      }
      
      metrics <- list(
        list(name = "Employment 16+ (000s)", cur = gv("emp16_cur") / 1000, dq = gv("emp16_dq") / 1000, dy = gv("emp16_dy") / 1000, dc = gv("emp16_dc") / 1000, de = gv("emp16_de") / 1000, invert = FALSE, type = "count"),
        list(name = "Employment rate 16-64 (%)", cur = gv("emp_rt_cur"), dq = gv("emp_rt_dq"), dy = gv("emp_rt_dy"), dc = gv("emp_rt_dc"), de = gv("emp_rt_de"), invert = FALSE, type = "rate"),
        list(name = "Unemployment 16+ (000s)", cur = gv("unemp16_cur") / 1000, dq = gv("unemp16_dq") / 1000, dy = gv("unemp16_dy") / 1000, dc = gv("unemp16_dc") / 1000, de = gv("unemp16_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Unemployment rate 16+ (%)", cur = gv("unemp_rt_cur"), dq = gv("unemp_rt_dq"), dy = gv("unemp_rt_dy"), dc = gv("unemp_rt_dc"), de = gv("unemp_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity 16-64 (000s)", cur = gv("inact_cur") / 1000, dq = gv("inact_dq") / 1000, dy = gv("inact_dy") / 1000, dc = gv("inact_dc") / 1000, de = gv("inact_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity 50-64 (000s)", cur = gv("inact5064_cur") / 1000, dq = gv("inact5064_dq") / 1000, dy = gv("inact5064_dy") / 1000, dc = gv("inact5064_dc") / 1000, de = gv("inact5064_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity rate 16-64 (%)", cur = gv("inact_rt_cur"), dq = gv("inact_rt_dq"), dy = gv("inact_rt_dy"), dc = gv("inact_rt_dc"), de = gv("inact_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity rate 50-64 (%)", cur = gv("inact5064_rt_cur"), dq = gv("inact5064_rt_dq"), dy = gv("inact5064_rt_dy"), dc = gv("inact5064_rt_dc"), de = gv("inact5064_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Vacancies (000s)", cur = gv("vac_cur"), dq = gv("vac_dq"), dy = gv("vac_dy"), dc = gv("vac_dc"), de = gv("vac_de"), invert = NA, type = "exempt"),
        list(name = "Payroll employees (000s)", cur = gv("payroll_cur"), dq = gv("payroll_dq"), dy = gv("payroll_dy"), dc = gv("payroll_dc"), de = gv("payroll_de"), invert = FALSE, type = "exempt"),
        list(name = "Wages total pay (%)", cur = gv("latest_wages"), dq = gv("wages_change_q"), dy = gv("wages_change_y"), dc = gv("wages_change_covid"), de = gv("wages_change_election"), invert = FALSE, type = "wages"),
        list(name = "Wages CPI-adjusted (%)", cur = gv("latest_wages_cpi"), dq = gv("wages_cpi_change_q"), dy = gv("wages_cpi_change_y"), dc = gv("wages_cpi_change_covid"), de = gv("wages_cpi_change_election"), invert = FALSE, type = "wages")
      )
      
      incProgress(0.4, detail = "done")
      
      dashboard_data(metrics)
    })
    
    showNotification("Dashboard loaded successfully!", type = "message", duration = 3)
  })
  
  # preview: top ten
  
  observeEvent(input$preview_topten, {
    
    withProgress(message = "loading top ten...", value = 0, {
      
      incProgress(0.2, detail = "loading...")
      
      if (!file.exists(calculations_path)) {
        showNotification("Error: calculations.R not found", type = "error")
        return()
      }
      
      if (!file.exists(top_ten_path)) {
        showNotification("Error: top_ten_stats.R not found", type = "error")
        return()
      }
      
      if (file.exists(config_path)) {
        source(config_path, local = FALSE)
      }
      
      mm <- reference_manual_month()
      if (!is.null(mm) && nzchar(mm)) {
        manual_month <<- tolower(mm)
      }
      
      incProgress(0.4, detail = "calculating...")
      
      tryCatch({
        source(calculations_path, local = FALSE)
      }, error = function(e) {
        showNotification(paste("Calculation error:", e$message), type = "error", duration = 5)
        return()
      })
      
      source(top_ten_path, local = FALSE)
      
      incProgress(0.4, detail = "generating...")
      
      if (exists("generate_top_ten")) {
        top10 <- tryCatch(generate_top_ten(), error = function(e) {
          showNotification(paste("Top ten generation error:", e$message), type = "error")
          NULL
        })
        if (!is.null(top10)) topten_data(top10)
      } else {
        showNotification("Error: generate_top_ten function not found", type = "error")
        return()
      }
    })
    
    showNotification("Top Ten statistics loaded successfully!", type = "message", duration = 3)
  })
  
  # auto preview: summary
  observeEvent(input$auto_preview_summary, {
    
    withProgress(message = "generating summary...", value = 0, {
      incProgress(0.2, detail = "loading...")
      
      if (file.exists(config_path)) source(config_path, local = FALSE)
      source("utils/helpers.R", local = FALSE)
      
      mm <- reference_manual_month()
      if (!is.null(mm) && nzchar(mm)) manual_month <<- tolower(mm)
      
      incProgress(0.3, detail = "calculating...")
      tryCatch({
        source(calculations_path, local = FALSE)
      }, error = function(e) {
        showNotification(paste("Calculation error:", e$message), type = "error", duration = 5)
        return()
      })
      
      incProgress(0.3, detail = "generating...")
      source(summary_path, local = FALSE)
      
      result <- tryCatch(generate_summary(), error = function(e) {
        showNotification(paste("Summary error:", e$message), type = "error")
        NULL
      })
      if (!is.null(result)) summary_data(result)
      incProgress(0.2, detail = "done")
    })
    
    showNotification("Summary generated (Database)", type = "message", duration = 3)
  })
  
  # auto preview: oecd
  observeEvent(input$auto_preview_oecd, {
    
    withProgress(message = "loading oecd data...", value = 0, {
      
      .fetch_oecd_table <- function(conn, table_name) {
        tryCatch({
          query <- sprintf(
            'SELECT ref_area, time_period, obs_value FROM "oecd"."%s" WHERE ref_area IN (\'GBR\',\'USA\',\'FRA\',\'DEU\',\'ITA\',\'ESP\',\'CAN\',\'JPN\',\'EA20\') ORDER BY ref_area, time_period DESC',
            table_name
          )
          raw <- DBI::dbGetQuery(conn, query)
          if (nrow(raw) == 0) return(NULL)
          # skip NA values then pick latest row per country
          raw <- raw[!is.na(raw$obs_value) & nzchar(raw$obs_value), , drop = FALSE]
          raw <- raw[!duplicated(raw$ref_area), , drop = FALSE]
          country_map <- c(GBR = "United Kingdom", USA = "United States", FRA = "France",
                           DEU = "Germany", ITA = "Italy", ESP = "Spain",
                           CAN = "Canada", JPN = "Japan", EA20 = "Euro area")
          raw$country <- country_map[raw$ref_area]
          raw$value <- as.numeric(raw$obs_value)
          raw$period <- raw$time_period
          raw[!is.na(raw$country), c("country", "period", "value"), drop = FALSE]
        }, error = function(e) NULL)
      }
      
      incProgress(0.2, detail = "loading...")
      
      conn <- NULL
      unemp_data <- NULL; emp_data <- NULL; inact_data <- NULL
      tryCatch({
        conn <- DBI::dbConnect(RPostgres::Postgres())
        
        incProgress(0.5, detail = "generating...")
        unemp_data <- .fetch_oecd_table(conn, "labour_statistics__unemployment_rate")
        emp_data <- .fetch_oecd_table(conn, "labour_statistics__employment_rate")
        inact_data <- .fetch_oecd_table(conn, "labour_statistics__inactivity_rate")
      }, error = function(e) {
        showNotification(paste("OECD DB error:", e$message), type = "error", duration = 5)
      }, finally = {
        if (!is.null(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
      })
      
      if (is.null(unemp_data) && is.null(emp_data) && is.null(inact_data)) {
        showNotification("No OECD data found in database", type = "warning")
        return()
      }
      
      preview <- .build_oecd_preview(unemp_data, emp_data, inact_data)
      oecd_preview_data(preview)
      incProgress(0.3, detail = "done")
    })
    
    showNotification("OECD data loaded (Database)", type = "message", duration = 3)
  })
  
  # download: word (auto tab)
  
  output$download_word <- downloadHandler(
    filename = function() {
      mm <- reference_manual_month()
      label <- if (!is.null(mm) && nzchar(mm)) manual_month_to_display(mm) else format(Sys.Date(), "%B %Y")
      paste0("Labour Market Stats Briefing - ", label, ".docx")
    },
    content = function(file) {
      
      tryCatch({
        withProgress(message = "generating word document...", value = 0, {
          
          incProgress(0.2, detail = "loading...")
          
          if (!requireNamespace("officer", quietly = TRUE)) {
            stop("officer package not installed")
          }
          
          if (!file.exists(template_path)) {
            stop("Template file (ManualDB.docx) not found")
          }
          
          incProgress(0.4, detail = "generating...")
          
          source(word_script_path, local = FALSE)
          
          mm <- reference_manual_month()
          vac_mode <- auto_selected_vac_period()
          pay_mode <- auto_selected_pay_period()
          month_override <- mm
          
          generate_word_output(
            template_path = template_path,
            output_path = file,
            calculations_path = calculations_path,
            config_path = config_path,
            summary_path = summary_path,
            top_ten_path = top_ten_path,
            manual_month_override = month_override,
            vacancies_mode_override = vac_mode,
            payroll_mode_override = pay_mode,
            contact_names = input$auto_contact_names
          )

          # IMPORTANT: append the Custom Indicators section BEFORE the Key
          # Charts page. Both insert before the last <w:sectPr>, so the second
          # call sits closer to sectPr and the first ends up earlier in the
          # body. Order on the page: OECD -> Custom -> Charts.
          tryCatch({
            sels <- lms_selections_data(); cd <- lms_catalog_data()
            if (!is.null(sels) && length(sels) > 0L && !is.null(cd)) {
              recs <- lms_summary_lines(sels, cd$catalog, cd$periods, cd$path)
              keep <- vapply(recs, function(rec) {
                v <- input[[paste0("lms_line_", rec$cdid, "_", rec$baseline)]]
                is.null(v) || isTRUE(v)
              }, logical(1))
              keys <- vapply(recs[keep], `[[`, "", "key")
              frag <- build_custom_indicators_xml(sels, cd$catalog, cd$periods, cd$path,
                                                  line_keys = keys)
              if (nzchar(frag)) append_custom_indicators(file, frag)
            }
          }, error = function(e) message("Custom indicators skipped: ", e$message))

          # append the configurable Key Charts page (native editable charts)
          tryCatch({
            source("sheets/lfs.R", local = FALSE)
            source("sheets/payroll.R", local = FALSE)
            source("sheets/vacancies.R", local = FALSE)
            source("sheets/redundancy.R", local = FALSE)
            source("sheets/wages_nominal.R", local = FALSE)
            configs <- .build_chart_configs()
            if (length(configs) > 0) {
              ids <- vapply(configs, `[[`, "", "id")
              append_key_charts_page(
                file,
                build_chart_series(configs, "auto",
                                   c(build_auto_sources(ids), list(lms = lms_catalog_data())))
              )
            }
          }, error = function(e) message("Key Charts page skipped: ", e$message))

          incProgress(0.4, detail = "done")
        })
        
        showNotification("Word document generated!", type = "message", duration = 3)
        
      }, error = function(e) {
        message("Word download error: ", e$message)
        showNotification(paste("Word error:", e$message), type = "error", duration = 5)
        
        # write a fallback docx so the download doesn't silently fail
        if (requireNamespace("officer", quietly = TRUE)) {
          doc <- officer::read_docx()
          doc <- officer::body_add_par(doc, "Error Generating Brief", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Error:", e$message))
          doc <- officer::body_add_par(doc, "Please check the R console for details.")
          print(doc, target = file)
        } else {
          writeLines(paste("Error:", e$message), con = file)
        }
      })
    }
  )
  
  
  output$download_excel <- downloadHandler(
    filename = function() {
      mm <- reference_manual_month()
      label <- if (!is.null(mm) && nzchar(mm)) manual_month_to_display(mm) else format(Sys.Date(), "%B %Y")
      paste0("LM Stats Audit - ", label, ".xlsx")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = "generating excel workbook...", value = 0, {
          
          incProgress(0.2, detail = "loading...")
          if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("openxlsx package not installed")
          }
          
          excel_env <- new.env(parent = globalenv())
          source(excel_script_path, local = excel_env)
          
          if (!exists("create_audit_workbook", envir = excel_env)) {
            stop("create_audit_workbook() not found after sourcing excel_audit_workbook.R")
          }
          
          incProgress(0.5, detail = "generating...")
          mm <- reference_manual_month()
          vac_mode <- auto_selected_vac_period()
          pay_mode <- auto_selected_pay_period()
          month_override <- mm
          tmp_xlsx <- tempfile(fileext = ".xlsx")

          # build the LMS custom-indicators audit sheets from the analyst's
          # current selection (same per-line checkboxes as the Word briefing)
          lms_aud <- NULL
          tryCatch({
            sels <- lms_selections_data(); cd <- lms_catalog_data()
            if (!is.null(sels) && length(sels) > 0L && !is.null(cd)) {
              recs <- lms_summary_lines(sels, cd$catalog, cd$periods, cd$path)
              keep <- vapply(recs, function(rec) {
                v <- input[[paste0("lms_line_", rec$cdid, "_", rec$baseline)]]
                is.null(v) || isTRUE(v)
              }, logical(1))
              keys <- vapply(recs[keep], `[[`, "", "key")
              lms_aud <- build_custom_audit(sels, cd$catalog, cd$periods, cd$path,
                                            line_keys = keys)
            }
          }, error = function(e) message("LMS audit sheets skipped: ", e$message))

          excel_env$create_audit_workbook(
            output_path = tmp_xlsx,
            file_a01 = uploaded_files$a01,
            file_hr1 = uploaded_files$hr1,
            file_x09 = uploaded_files$x09,
            file_rtisa = uploaded_files$rtisa,
            file_cla01 = uploaded_files$cla01,
            file_x02 = uploaded_files$x02,
            file_oecd_unemp = .oecd_source("unemp"),
            file_oecd_emp = .oecd_source("emp"),
            file_oecd_inact = .oecd_source("inact"),
            calculations_path = calculations_path,
            config_path = config_path,
            vacancies_mode = vac_mode,
            payroll_mode = pay_mode,
            manual_month_override = month_override,
            lms_audit = lms_aud,
            verbose = FALSE
          )
          
          incProgress(0.3, detail = "done")
          ok <- file.copy(tmp_xlsx, file, overwrite = TRUE)
          if (!isTRUE(ok) || !file.exists(file)) {
            stop("Excel workbook could not be copied to Shiny download location")
          }
        })
        
        showNotification("Excel workbook generated", type = "message", duration = 3)
        
      }, error = function(e) {
        # return a valid xlsx with error info so browser doesn't hang
        message("Excel download error: ", e$message)
        showNotification(paste("Excel error:", e$message), type = "error", duration = 5)
        
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          tmp_xlsx <- tempfile(fileext = ".xlsx")
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Error")
          openxlsx::writeData(wb, "Error", data.frame(
            Error = paste("Failed to generate workbook:", e$message),
            Suggestion = "Check database connectivity / package availability, then try again."
          ))
          openxlsx::saveWorkbook(wb, tmp_xlsx, overwrite = TRUE)
          file.copy(tmp_xlsx, file, overwrite = TRUE)
        } else {
          writeLines(paste("Failed to generate Excel workbook:", e$message), con = file)
        }
      })
    }
  )
  
  
  # manual tab
  
  .check_manual_ready <- function() {
    if (is.null(uploaded_files$a01)) {
      showNotification("Upload at least the A01 file", type = "warning", duration = 5)
      return(FALSE)
    }
    missing <- c()
    if (is.null(uploaded_files$hr1))   missing <- c(missing, "HR1 (redundancy notifications)")
    if (is.null(uploaded_files$x09))   missing <- c(missing, "X09 (real wages/CPI)")
    if (is.null(uploaded_files$rtisa)) missing <- c(missing, "RTISA (payroll/sector)")
    if (length(missing) > 0) {
      showNotification(
        paste0("Proceeding without: ", paste(missing, collapse = ", "),
               ". Those sections will show \u2014."),
        type = "message", duration = 8
      )
    }
    TRUE
  }
  
  # manual preview: dashboard (excel upload)
  observeEvent(input$manual_preview_dashboard, {
    if (!.check_manual_ready()) return()
    
    withProgress(message = "loading dashboard...", value = 0, {
      
      incProgress(0.2, detail = "calculating...")
      
      calc_env <- new.env(parent = globalenv())
      
      source(excel_calc_path, local = calc_env)
      source("utils/helpers.R", local = calc_env)
      detected_mm <- calc_env$run_calculations_from_excel(
        manual_month = NULL,
        file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
        file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
        vac_end_override = .parse_period_end(.selected_vac_label()),
        payroll_end_override = .parse_period_end(.selected_pay_label()),
        target_env = calc_env
      )
      .update_ref_month(detected_mm)
      
      incProgress(0.6, detail = "generating...")
      
      gv <- function(name) {
        if (exists(name, envir = calc_env)) {
          val <- get(name, envir = calc_env)
          if (is.numeric(val)) return(val)
        }
        NA_real_
      }
      
      metrics <- list(
        list(name = "Employment 16+ (000s)", cur = gv("emp16_cur") / 1000, dq = gv("emp16_dq") / 1000, dy = gv("emp16_dy") / 1000, dc = gv("emp16_dc") / 1000, de = gv("emp16_de") / 1000, invert = FALSE, type = "count"),
        list(name = "Employment rate 16-64 (%)", cur = gv("emp_rt_cur"), dq = gv("emp_rt_dq"), dy = gv("emp_rt_dy"), dc = gv("emp_rt_dc"), de = gv("emp_rt_de"), invert = FALSE, type = "rate"),
        list(name = "Unemployment 16+ (000s)", cur = gv("unemp16_cur") / 1000, dq = gv("unemp16_dq") / 1000, dy = gv("unemp16_dy") / 1000, dc = gv("unemp16_dc") / 1000, de = gv("unemp16_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Unemployment rate 16+ (%)", cur = gv("unemp_rt_cur"), dq = gv("unemp_rt_dq"), dy = gv("unemp_rt_dy"), dc = gv("unemp_rt_dc"), de = gv("unemp_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity 16-64 (000s)", cur = gv("inact_cur") / 1000, dq = gv("inact_dq") / 1000, dy = gv("inact_dy") / 1000, dc = gv("inact_dc") / 1000, de = gv("inact_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity 50-64 (000s)", cur = gv("inact5064_cur") / 1000, dq = gv("inact5064_dq") / 1000, dy = gv("inact5064_dy") / 1000, dc = gv("inact5064_dc") / 1000, de = gv("inact5064_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity rate 16-64 (%)", cur = gv("inact_rt_cur"), dq = gv("inact_rt_dq"), dy = gv("inact_rt_dy"), dc = gv("inact_rt_dc"), de = gv("inact_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity rate 50-64 (%)", cur = gv("inact5064_rt_cur"), dq = gv("inact5064_rt_dq"), dy = gv("inact5064_rt_dy"), dc = gv("inact5064_rt_dc"), de = gv("inact5064_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Vacancies (000s)", cur = gv("vac_cur"), dq = gv("vac_dq"), dy = gv("vac_dy"), dc = gv("vac_dc"), de = gv("vac_de"), invert = NA, type = "exempt"),
        list(name = "Payroll employees (000s)", cur = gv("payroll_cur"), dq = gv("payroll_dq"), dy = gv("payroll_dy"), dc = gv("payroll_dc"), de = gv("payroll_de"), invert = FALSE, type = "exempt"),
        list(name = "Wages total pay (%)", cur = gv("latest_wages"), dq = gv("wages_change_q"), dy = gv("wages_change_y"), dc = gv("wages_change_covid"), de = gv("wages_change_election"), invert = FALSE, type = "wages"),
        list(name = "Wages CPI-adjusted (%)", cur = gv("latest_wages_cpi"), dq = gv("wages_cpi_change_q"), dy = gv("wages_cpi_change_y"), dc = gv("wages_cpi_change_covid"), de = gv("wages_cpi_change_election"), invert = FALSE, type = "wages")
      )
      
      incProgress(0.2, detail = "done")
      dashboard_data(metrics)
    })
    
    showNotification("Dashboard loaded (Manual Upload)", type = "message", duration = 3)
  })
  
  # manual preview: top ten
  observeEvent(input$manual_preview_topten, {
    if (!.check_manual_ready()) return()
    
    withProgress(message = "loading top ten...", value = 0, {
      
      incProgress(0.2, detail = "calculating...")
      
      if (file.exists(config_path)) source(config_path, local = FALSE)
      source("utils/helpers.R", local = FALSE)
      
      source(excel_calc_path, local = FALSE)
      detected_mm <- run_calculations_from_excel(
        manual_month = NULL,
        file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
        file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
        target_env = globalenv()
      )
      manual_month <<- detected_mm
      .update_ref_month(detected_mm)
      
      incProgress(0.5, detail = "generating...")
      
      source(top_ten_path, local = FALSE)
      
      if (exists("generate_top_ten")) {
        top10 <- tryCatch(generate_top_ten(), error = function(e) {
          showNotification(paste("Top ten error:", e$message), type = "error")
          NULL
        })
        if (!is.null(top10)) topten_data(top10)
      }
      
      incProgress(0.3, detail = "done")
    })
    
    showNotification("Top Ten loaded (Manual Upload)", type = "message", duration = 3)
  })
  
  # manual preview: summary
  summary_data <- reactiveVal(NULL)
  
  observeEvent(input$manual_preview_summary, {
    if (!.check_manual_ready()) return()
    
    withProgress(message = "generating summary...", value = 0, {
      incProgress(0.2, detail = "calculating...")
      
      source("utils/helpers.R", local = FALSE)
      source(excel_calc_path, local = FALSE)
      if (file.exists(config_path)) source(config_path, local = FALSE)
      
      detected_mm <- run_calculations_from_excel(
        manual_month = NULL,
        file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
        file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
        target_env = globalenv()
      )
      manual_month <<- detected_mm
      .update_ref_month(detected_mm)
      
      incProgress(0.5, detail = "generating...")
      source(summary_path, local = FALSE)
      
      result <- tryCatch(generate_summary(), error = function(e) {
        showNotification(paste("Summary error:", e$message), type = "error")
        NULL
      })
      if (!is.null(result)) summary_data(result)
      incProgress(0.3, detail = "done")
    })
    
    showNotification("Summary generated (Manual Upload)", type = "message", duration = 3)
  })

  # charts preview (configurable Key Charts page) — config inputs are per-metric;
  # rendered once up-front so quick-preset buttons can updateSelectInput even
  # when a row is collapsed. Do not change this to dynamic mount/unmount.
  charts_preview_data <- reactiveVal(NULL)

  # Per-metric settings rows. Each row mounts a full set of scoped inputs
  # (chart_<id>_on/type/from/to/smoothing/ymin/ymax); the settings panel is
  # show/hidden via conditionalPanel, not re-rendered.
  output$chart_metric_rows <- renderUI({
    cur_yr  <- as.integer(format(Sys.Date(), "%Y"))
    yrs     <- as.character(1971:cur_yr)
    defs_on <- c("unemp16", "payroll_change", "youth_unemp", "inact")
    type_choices <- c("Line" = "line", "Bar (column)" = "bar", "Area" = "area")
    smooth_choices <- c("Raw" = "raw",
                        "3-month rolling avg" = "rolling3",
                        "12-month rolling avg" = "rolling12",
                        "Annual average" = "annual")
    rows <- lapply(CHART_METRICS, function(m) {
      id <- m$id
      div(style = "border-bottom:1px solid #e3e3e3; padding:10px 0;",
          checkboxInput(paste0("chart_", id, "_on"), label = m$label,
                        value = id %in% defs_on),
          conditionalPanel(
            condition = sprintf("input.chart_%s_on == true", id),
            div(style = "display:flex; flex-wrap:wrap; gap:10px; padding-left:24px; margin-top:4px;",
                div(style = "min-width:130px;",
                    selectInput(paste0("chart_", id, "_type"), "Type",
                                choices = type_choices, selected = m$type, width = "100%")),
                div(style = "min-width:100px;",
                    selectInput(paste0("chart_", id, "_from"), "From",
                                choices = yrs, selected = "2010", width = "100%")),
                div(style = "min-width:100px;",
                    selectInput(paste0("chart_", id, "_to"), "To",
                                choices = yrs, selected = as.character(cur_yr), width = "100%")),
                div(style = "min-width:170px;",
                    selectInput(paste0("chart_", id, "_smoothing"), "Smoothing",
                                choices = smooth_choices, selected = "raw", width = "100%")))))
    })
    do.call(tagList, rows)
  })

  # Config rows for LMS series added as charts (frequency + the usual controls).
  output$chart_lms_rows <- renderUI({
    cd <- lms_catalog_data(); picks <- input$chart_lms_pick
    if (is.null(cd) || length(picks) == 0L) return(NULL)
    cur_yr <- as.integer(format(Sys.Date(), "%Y")); yrs <- as.character(1971:cur_yr)
    type_choices   <- c("Line" = "line", "Bar (column)" = "bar", "Area" = "area")
    smooth_choices <- c("Raw" = "raw", "3-month rolling avg" = "rolling3",
                        "12-month rolling avg" = "rolling12", "Annual average" = "annual")
    rows <- lapply(picks, function(cdid) {
      crow <- cd$catalog[cd$catalog$cdid == cdid, , drop = FALSE]
      if (nrow(crow) == 0L) return(NULL)
      avail <- .lms_avail_freqs(cd, cdid); if (length(avail) == 0L) avail <- c("M","Q","A")
      freq_choices <- setNames(avail, c(M="Monthly", Q="Quarterly", A="Annual")[avail])
      default_freq <- if ("Q" %in% avail) "Q" else avail[1]
      sl <- tryCatch(lms_series(cd$catalog, cd$periods, cd$path, cdid, default_freq),
                     error = function(e) NULL)
      spark <- if (!is.null(sl) && nrow(sl) >= 2L) .lms_sparkline_svg(sl$value) else ""
      div(style = "border-bottom:1px solid #e3e3e3; padding:8px 0;",
          div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:10px;",
              div(style = "flex:1 1 auto; min-width:0;", tags$strong(cdid), " | ", crow$title),
              if (nzchar(spark)) div(style = "flex:0 0 auto;", HTML(spark))),
          div(style = "display:flex; flex-wrap:wrap; gap:8px; padding-left:8px; margin-top:4px;",
              div(style = "min-width:120px;",
                  selectInput(paste0("chart_lms_", cdid, "_freq"), "Frequency",
                              choices = freq_choices, selected = default_freq, width = "100%")),
              div(style = "min-width:120px;",
                  selectInput(paste0("chart_lms_", cdid, "_type"), "Type",
                              choices = type_choices, selected = "line", width = "100%")),
              div(style = "min-width:90px;",
                  selectInput(paste0("chart_lms_", cdid, "_from"), "From",
                              choices = yrs, selected = "2010", width = "100%")),
              div(style = "min-width:90px;",
                  selectInput(paste0("chart_lms_", cdid, "_to"), "To",
                              choices = yrs, selected = as.character(cur_yr), width = "100%")),
              div(style = "min-width:160px;",
                  selectInput(paste0("chart_lms_", cdid, "_smoothing"), "Smoothing",
                              choices = smooth_choices, selected = "raw", width = "100%"))))
    })
    do.call(tagList, Filter(Negate(is.null), rows))
  })

  # Build the list of per-chart configs from the scoped inputs of every ticked metric.
  .build_chart_configs <- function() {
    configs <- list()
    for (m in CHART_METRICS) {
      if (!isTRUE(input[[paste0("chart_", m$id, "_on")]])) next
      configs[[length(configs) + 1]] <- list(
        id        = m$id,
        type      = input[[paste0("chart_", m$id, "_type")]] %||% m$type,
        year_from = suppressWarnings(as.integer(input[[paste0("chart_", m$id, "_from")]])),
        year_to   = suppressWarnings(as.integer(input[[paste0("chart_", m$id, "_to")]])),
        smoothing = input[[paste0("chart_", m$id, "_smoothing")]] %||% "raw"
      )
    }
    # LMS-series charts (added via the search box, src = "lms")
    for (cdid in input$chart_lms_pick) {
      fr <- input[[paste0("chart_lms_", cdid, "_freq")]]
      if (is.null(fr) || !nzchar(fr)) next
      configs[[length(configs) + 1]] <- list(
        src       = "lms", id = cdid, cdid = cdid, freq = fr,
        type      = input[[paste0("chart_lms_", cdid, "_type")]] %||% "line",
        year_from = suppressWarnings(as.integer(input[[paste0("chart_lms_", cdid, "_from")]])),
        year_to   = suppressWarnings(as.integer(input[[paste0("chart_lms_", cdid, "_to")]])),
        smoothing = input[[paste0("chart_lms_", cdid, "_smoothing")]] %||% "raw"
      )
    }
    configs
  }

  .validate_chart_configs <- function(configs) {
    errs <- character(0)
    for (c in configs) {
      if (is.na(c$year_from) || is.na(c$year_to) || c$year_from > c$year_to)
        errs <- c(errs, paste0("Invalid From/To for ", c$id))
    }
    errs
  }

  observeEvent(input$regenerate_charts, {
    configs <- .build_chart_configs()
    if (length(configs) == 0) {
      showNotification("Tick at least one metric to chart", type = "warning"); return()
    }
    errs <- .validate_chart_configs(configs)
    if (length(errs) > 0) {
      showNotification(paste(errs, collapse = "; "), type = "warning"); return()
    }
    flow <- input$mode_tabs

    if (identical(flow, "manual")) {
      if (!.check_manual_ready()) return()
      withProgress(message = "building charts...", value = 0, {
        incProgress(0.3, detail = "reading files...")
        series <- tryCatch(
          build_chart_series(configs, "manual",
                             list(file_a01 = uploaded_files$a01,
                                  file_rtisa = uploaded_files$rtisa,
                                  lms = lms_catalog_data())),
          error = function(e) {
            showNotification(paste("Charts error:", e$message), type = "error"); NULL
          })
        charts_preview_data(series)
        incProgress(0.7, detail = "done")
      })
      showNotification("Charts generated (Manual Upload)", type = "message", duration = 3)
    } else {
      withProgress(message = "building charts...", value = 0, {
        incProgress(0.3, detail = "querying database...")
        source("sheets/lfs.R", local = FALSE)
        source("sheets/payroll.R", local = FALSE)
        source("sheets/vacancies.R", local = FALSE)
        source("sheets/redundancy.R", local = FALSE)
        source("sheets/wages_nominal.R", local = FALSE)
        ids <- vapply(configs, `[[`, "", "id")
        series <- tryCatch(
          build_chart_series(configs, "auto",
                             c(build_auto_sources(ids), list(lms = lms_catalog_data()))),
          error = function(e) {
            showNotification(paste("Charts error:", e$message), type = "error"); NULL
          })
        charts_preview_data(series)
        incProgress(0.7, detail = "done")
      })
      showNotification("Charts loaded (Database)", type = "message", duration = 3)
    }
  })

  # Quick presets — apply yr_from/yr_to to every currently-ticked metric.
  # Works for collapsed rows too because their inputs are always mounted.
  .apply_chart_preset <- function(yr_from, yr_to) {
    any_on <- FALSE
    for (m in CHART_METRICS) {
      if (!isTRUE(input[[paste0("chart_", m$id, "_on")]])) next
      any_on <- TRUE
      updateSelectInput(session, paste0("chart_", m$id, "_from"), selected = as.character(yr_from))
      updateSelectInput(session, paste0("chart_", m$id, "_to"),   selected = as.character(yr_to))
    }
    if (!any_on)
      showNotification("No metrics ticked — tick at least one to apply the preset.", type = "warning")
  }
  observeEvent(input$preset_last5,       { cy <- as.integer(format(Sys.Date(), "%Y")); .apply_chart_preset(cy - 5, cy) })
  observeEvent(input$preset_since_covid, { cy <- as.integer(format(Sys.Date(), "%Y")); .apply_chart_preset(2019, cy) })
  observeEvent(input$preset_since_2010,  { cy <- as.integer(format(Sys.Date(), "%Y")); .apply_chart_preset(2010, cy) })
  observeEvent(input$preset_full,        { cy <- as.integer(format(Sys.Date(), "%Y")); .apply_chart_preset(1971, cy) })

  # ---- LMS Explorer (custom indicators) -----------------------------------
  # `lms_catalog_data`  is populated when the LMS file is uploaded.
  # `lms_selections_data` holds the per-CDID selections set on "Refresh preview".
  # `lms_table_preview_data` is the data.frame rendered in-app.
  lms_catalog_data       <- reactiveVal(NULL)
  lms_selections_data    <- reactiveVal(NULL)
  lms_table_preview_data <- reactiveVal(NULL)

  output$lms_loaded <- reactive({ !is.null(lms_catalog_data()) })
  outputOptions(output, "lms_loaded", suspendWhenHidden = FALSE)

  # Per-CDID settings rows. INVARIANT: all selected CDIDs render their own
  # input widgets here; row order follows input$lms_pick.
  output$lms_settings_rows <- renderUI({
    cd <- lms_catalog_data(); picks <- input$lms_pick
    if (is.null(cd) || length(picks) == 0L) {
      return(div(class = "govuk-hint", style = "margin-top:8px;",
                 "Pick one or more series above to configure them."))
    }
    cat0 <- cd$catalog
    bl_choices <- setNames(names(LMS_BASELINES)[names(LMS_BASELINES) != "current"],
                           vapply(names(LMS_BASELINES)[names(LMS_BASELINES) != "current"],
                                  function(b) LMS_BASELINES[[b]]$label, ""))
    rows <- lapply(picks, function(cdid) {
      crow <- cat0[cat0$cdid == cdid, , drop = FALSE]
      if (nrow(crow) == 0L) return(NULL)
      avail <- .lms_avail_freqs(cd, cdid)
      if (length(avail) == 0L) avail <- c("M","Q","A")  # graceful: show all if none
      freq_choices <- setNames(avail, c(M="Monthly", Q="Quarterly", A="Annual")[avail])
      default_freq <- if ("Q" %in% avail) "Q" else avail[1]  # prefer quarterly

      # offer to add the rest of this series' breakdown family (industry, age, ...)
      fam <- NULL
      if (!is.null(cd$families)) {
        fi <- unname(cd$families$fam_of[cdid])
        if (!is.na(fi)) fam <- cd$families$families[[fi]]
      }
      fam_ui <- NULL
      if (!is.null(fam) && fam$n > 1L) {
        not_added <- setdiff(fam$cdids, picks)
        if (length(not_added) > 0L) {
          preview <- paste(utils::head(fam$variants, 4L), collapse = ", ")
          if (length(fam$variants) > 4L) preview <- paste0(preview, ", ...")
          fam_ui <- div(style = "margin:4px 0 2px 8px; font-size:90%; color:#505050;",
                        tags$span(sprintf("Part of \"%s\" - %d series (%s). ",
                                          fam$label, fam$n, preview)),
                        actionButton(paste0("lms_addfam_", cdid),
                                     sprintf("Add all %d", length(not_added)),
                                     class = "govuk-button govuk-button--blue",
                                     style = "padding:2px 10px; font-size:95%; margin-left:6px;"))
        }
      }

      # mini trend sparkline + latest value (off the cached matrix, instant)
      sl <- tryCatch(lms_series(cd$catalog, cd$periods, cd$path, cdid, default_freq),
                     error = function(e) NULL)
      spark <- if (!is.null(sl) && nrow(sl) >= 2L) .lms_sparkline_svg(sl$value) else ""
      latest_lbl <- ""
      if (!is.null(sl) && nrow(sl) >= 1L) {
        ir <- .lms_is_rate(crow); lv <- sl$value[nrow(sl)]
        latest_lbl <- paste0(.lms_format_value(lv, ir), if (ir) "%" else "",
                             " (", sl$period_label[nrow(sl)], ")")
      }

      div(style = "border-bottom:1px solid #e3e3e3; padding:10px 0;",
          div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px;",
              div(style = "flex:1 1 auto; min-width:0;", tags$strong(cdid), " | ", crow$title),
              if (nzchar(spark))
                div(style = "flex:0 0 auto; text-align:right;",
                    HTML(spark),
                    div(style = "font-size:80%; color:#505050; white-space:nowrap;", latest_lbl))),
          fam_ui,
          div(style = "display:flex; flex-wrap:wrap; gap:10px; padding-left:8px; margin-top:6px;",
              div(style = "min-width:140px;",
                  selectInput(paste0("lms_", cdid, "_freq"), "Frequency",
                              choices = freq_choices, selected = default_freq,
                              width = "100%")),
              div(style = "min-width:300px;",
                  div(style = "display:flex; align-items:center; gap:14px;",
                      tags$strong("Baselines"),
                      div(class = "lms-selectall",
                          checkboxInput(paste0("lms_", cdid, "_bl_all"),
                                        "Select all", value = TRUE))),
                  checkboxGroupInput(paste0("lms_", cdid, "_baselines"), label = NULL,
                                     choices = bl_choices, selected = unname(bl_choices),
                                     inline = TRUE))))
    })
    do.call(tagList, Filter(Negate(is.null), rows))
  })

  # Wire each per-series "Select all" baselines toggle. Observers are registered
  # once per CDID (tracked in lms_bl_all_registered) so re-picking a series does
  # not stack duplicate observers.
  lms_bl_all_registered <- reactiveVal(character(0))
  observeEvent(input$lms_pick, {
    reg <- lms_bl_all_registered()
    new <- setdiff(input$lms_pick, reg)
    bl_ids <- names(LMS_BASELINES)[names(LMS_BASELINES) != "current"]
    for (cdid in new) {
      local({
        cc <- cdid
        observeEvent(input[[paste0("lms_", cc, "_bl_all")]], {
          sel <- if (isTRUE(input[[paste0("lms_", cc, "_bl_all")]])) bl_ids else character(0)
          updateCheckboxGroupInput(session, paste0("lms_", cc, "_baselines"), selected = sel)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("lms_addfam_", cc)]], {
          cdat <- lms_catalog_data(); if (is.null(cdat) || is.null(cdat$families)) return()
          fi <- unname(cdat$families$fam_of[cc]); if (is.na(fi)) return()
          fam <- cdat$families$families[[fi]]
          added <- setdiff(fam$cdids, input$lms_pick)
          if (length(added) == 0L) return()
          ch <- setNames(cdat$catalog$cdid,
                         paste0(cdat$catalog$cdid, " | ", cdat$catalog$title))
          updateSelectizeInput(session, "lms_pick", choices = ch,
                               selected = union(input$lms_pick, fam$cdids), server = TRUE)
          showNotification(sprintf("Added %d series from this breakdown.", length(added)),
                           type = "message", duration = 3)
        }, ignoreInit = TRUE)
      })
    }
    if (length(new) > 0L) lms_bl_all_registered(c(reg, new))
  }, ignoreNULL = FALSE)

  # "Add a whole breakdown" search: selecting a family adds all its members.
  observeEvent(input$lms_family_pick, {
    cd <- lms_catalog_data(); if (is.null(cd) || is.null(cd$families)) return()
    v <- input$lms_family_pick
    if (length(v) == 0L || !nzchar(v)) return()
    fi <- suppressWarnings(as.integer(v))
    if (is.na(fi) || fi < 1L || fi > length(cd$families$families)) return()
    fam <- cd$families$families[[fi]]
    added <- setdiff(fam$cdids, input$lms_pick)
    ch <- setNames(cd$catalog$cdid, paste0(cd$catalog$cdid, " | ", cd$catalog$title))
    updateSelectizeInput(session, "lms_pick", choices = ch,
                         selected = union(input$lms_pick, fam$cdids), server = TRUE)
    updateSelectizeInput(session, "lms_family_pick", selected = character(0))
    showNotification(sprintf("Added %d series from '%s'.", length(added), fam$label),
                     type = "message", duration = 3)
  }, ignoreNULL = TRUE)

  # clear the whole selection in one go (avoids deleting chips one by one)
  observeEvent(input$lms_clear, {
    updateSelectizeInput(session, "lms_pick", selected = character(0))
    updateSelectizeInput(session, "lms_family_pick", selected = character(0))
  })

  observeEvent(input$regenerate_custom, {
    cd <- lms_catalog_data()
    if (is.null(cd)) {
      showNotification("Upload the LMS file first", type = "warning"); return()
    }
    picks <- input$lms_pick
    if (length(picks) == 0L) {
      lms_selections_data(NULL); lms_table_preview_data(NULL)
      showNotification("Pick at least one series", type = "warning"); return()
    }
    sels <- list()
    for (cdid in picks) {
      row <- cd$catalog[cd$catalog$cdid == cdid, , drop = FALSE]
      if (nrow(row) == 0L) next
      freq <- input[[paste0("lms_", cdid, "_freq")]]
      bls  <- input[[paste0("lms_", cdid, "_baselines")]]
      if (is.null(freq) || length(bls) == 0L) next
      # drop baselines not applicable to chosen frequency
      bls_keep <- vapply(bls, function(b) freq %in% LMS_BASELINES[[b]]$applies, logical(1))
      bls <- bls[bls_keep]
      sels[[length(sels) + 1L]] <- list(
        cdid = cdid, title = row$title, unit = row$unit, pre_unit = row$pre_unit,
        freq = freq, baselines = bls, catalog_row = row)
    }
    if (length(sels) == 0L) {
      lms_selections_data(NULL); lms_table_preview_data(NULL)
      showNotification("Selections have no applicable baselines", type = "warning"); return()
    }
    lms_selections_data(sels)
    df <- tryCatch(build_custom_preview_df(sels, cd$catalog, cd$periods, cd$path),
                   error = function(e) {
                     showNotification(paste("LMS preview error:", e$message), type = "error")
                     NULL
                   })
    lms_table_preview_data(df)
    showNotification("Custom indicators refreshed", type = "message", duration = 3)
  })

  # manual preview: oecd
  oecd_preview_data <- reactiveVal(NULL)
  
  # shared builder for oecd preview — used by both manual and auto tabs
  .build_oecd_preview <- function(unemp_data, emp_data, inact_data) {
    country_order <- c("United Kingdom", "United States", "France", "Germany",
                       "Italy", "Spain", "Canada", "Japan", "Euro area")
    g7_members <- c("United Kingdom", "United States", "France", "Germany",
                    "Italy", "Canada", "Japan")
    
    # override uk row with ons-derived rates when available
    .override_uk <- function(data, ons_val, tp) {
      if (is.na(ons_val)) return(data)
      uk_row <- data.frame(country = "United Kingdom", period = tp, value = ons_val,
                           stringsAsFactors = FALSE)
      if (is.null(data)) return(uk_row)
      data <- data[data$country != "United Kingdom", , drop = FALSE]
      rbind(data, uk_row)
    }
    .gv <- function(name) if (exists(name, envir = globalenv())) get(name, envir = globalenv()) else NA_real_
    uk_ur <- .gv("unemp_rt_cur"); uk_er <- .gv("emp_rt_cur"); uk_ir <- .gv("inact_rt_cur")
    mm <- if (exists("manual_month", envir = globalenv())) get("manual_month", envir = globalenv()) else ""
    uk_tp <- {
      mm2 <- tolower(gsub("[[:space:]]+", "", as.character(mm)))
      mon3 <- substr(gsub("[^a-z]", "", mm2), 1, 3)
      yr <- as.integer(substr(gsub("[^0-9]", "", mm2), 1, 4))
      m <- match(mon3, tolower(month.abb))
      if (!is.na(m) && !is.na(yr)) paste0(yr, "-Q", ceiling(m / 3)) else NA_character_
    }
    if (!is.na(uk_ur) || !is.na(uk_er) || !is.na(uk_ir)) {
      unemp_data <- .override_uk(unemp_data, uk_ur, uk_tp)
      emp_data   <- .override_uk(emp_data,   uk_er, uk_tp)
      inact_data <- .override_uk(inact_data, uk_ir, uk_tp)
    }
    
    .get_val <- function(data, country) {
      if (is.null(data)) return(list(period = NA_character_, value = NA_real_))
      idx <- match(country, data$country)
      if (is.na(idx)) return(list(period = NA_character_, value = NA_real_))
      list(period = data$period[idx], value = data$value[idx])
    }
    
    .fmt <- function(v) {
      if (is.na(v)) "\u2014" else paste0(formatC(round(v, 1), format = "f", digits = 1), "%")
    }
    
    display_names <- c(
      "United Kingdom" = "UK", "United States" = "United States",
      "France" = "France", "Germany" = "Germany", "Italy" = "Italy",
      "Spain" = "Spain", "Canada" = "Canada", "Japan" = "Japan",
      "Euro area" = "Euro Area"
    )
    
    # find the most common latest period across all data to detect outliers
    all_periods <- c(
      if (!is.null(unemp_data)) unemp_data$period else character(0),
      if (!is.null(emp_data))   emp_data$period   else character(0),
      if (!is.null(inact_data)) inact_data$period  else character(0)
    )
    all_periods <- all_periods[!is.na(all_periods)]
    
    footnotes <- character(0)
    
    rows <- lapply(country_order, function(country) {
      u  <- .get_val(unemp_data, country)
      e  <- .get_val(emp_data,   country)
      ia <- .get_val(inact_data, country)
      
      # row period = earliest of available metric periods (most conservative)
      avail <- c(u$period, e$period, ia$period)
      avail <- avail[!is.na(avail)]
      tp <- if (length(avail) > 0) sort(avail, decreasing = TRUE)[1] else NA_character_
      
      # detect cells from older periods and mark with **
      u_str  <- .fmt(u$value)
      e_str  <- .fmt(e$value)
      ia_str <- .fmt(ia$value)
      
      dname <- display_names[[country]]
      
      if (!is.na(tp)) {
        stale <- character(0)
        if (!is.na(u$period) && u$period != tp) {
          u_str <- paste0(u_str, "**")
          stale <- c(stale, paste0(dname, " unemployment rate from ", u$period))
        }
        if (!is.na(e$period) && e$period != tp) {
          e_str <- paste0(e_str, "**")
          stale <- c(stale, paste0(dname, " employment rate from ", e$period))
        }
        if (!is.na(ia$period) && ia$period != tp) {
          ia_str <- paste0(ia_str, "**")
          stale <- c(stale, paste0(dname, " inactivity rate from ", ia$period))
        }
        if (length(stale) > 0) footnotes <<- c(footnotes, stale)
      }
      
      if (country == "United Kingdom" && !is.na(tp)) tp <- paste0(tp, "*")
      list(
        country = dname,
        period  = if (is.na(tp)) "\u2014" else tp,
        unemp   = u_str,
        emp     = e_str,
        inact   = ia_str
      )
    })
    
    # g7 average row
    .g7_avg <- function(data) {
      if (is.null(data)) return(NA_real_)
      vals <- data$value[data$country %in% g7_members]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NA_real_)
      mean(vals)
    }
    g7_ur <- .g7_avg(unemp_data); g7_er <- .g7_avg(emp_data); g7_ir <- .g7_avg(inact_data)
    g7_periods <- character(0)
    for (d in list(unemp_data, emp_data, inact_data)) {
      if (!is.null(d)) {
        p <- d$period[d$country %in% g7_members]
        g7_periods <- c(g7_periods, p[!is.na(p)])
      }
    }
    g7_tp <- if (length(g7_periods) > 0) sort(unique(g7_periods), decreasing = TRUE)[1] else "\u2014"
    
    g7_row <- list(
      country = "G7 Average", period = g7_tp,
      unemp = .fmt(g7_ur), emp = .fmt(g7_er), inact = .fmt(g7_ir)
    )
    rows <- append(rows, list(g7_row), after = 8)
    
    # generate bullet points
    source("utils/word_helpers.R", local = TRUE)
    bullets <- .generate_oecd_bullets(unemp_data, emp_data, inact_data)
    
    # build ** footnote text
    stale_note <- if (length(footnotes) > 0) {
      paste0("**Latest ", paste(footnotes, collapse = ". "), ".")
    } else NULL
    
    list(rows = rows, bullets = bullets, stale_note = stale_note)
  }
  
  observeEvent(input$manual_preview_oecd, {
    unemp_src <- .oecd_source("unemp")
    emp_src   <- .oecd_source("emp")
    inact_src <- .oecd_source("inact")
    has_any <- !is.null(unemp_src) || !is.null(emp_src) || !is.null(inact_src)
    if (!has_any) {
      showNotification("OECD data not available. Auto-fetch failed \u2014 upload the 3 OECD CSVs manually to override.", type = "warning")
      return()
    }
    
    # src may be a file path (uploaded) or a pre-fetched data.frame (auto db)
    .coerce_oecd <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      tryCatch(.read_oecd_latest(x), error = function(e) NULL)
    }
    
    withProgress(message = "loading oecd data...", value = 0, {
      source("utils/manual_word_output.R", local = TRUE)
      incProgress(0.3, detail = "loading...")
      unemp_data <- .coerce_oecd(unemp_src)
      emp_data   <- .coerce_oecd(emp_src)
      incProgress(0.5, detail = "generating...")
      inact_data <- .coerce_oecd(inact_src)
      
      preview <- .build_oecd_preview(unemp_data, emp_data, inact_data)
      oecd_preview_data(preview)
      incProgress(0.2, detail = "done")
    })
    
    showNotification("OECD data loaded", type = "message", duration = 3)
  })
  
  # manual download: word
  output$manual_download_word <- downloadHandler(
    filename = function() {
      mm <- reference_manual_month()
      label <- if (!is.null(mm) && nzchar(mm)) manual_month_to_display(mm) else format(Sys.Date(), "%B %Y")
      paste0("Labour Market Stats Briefing - ", label, ".docx")
    },
    content = function(file) {
      if (is.null(uploaded_files$a01)) {
        showNotification("Upload at least the A01 file first", type = "warning")
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Upload the A01 file to generate.", style = "heading 1")
        print(doc, target = file)
        return()
      }
      
      tryCatch({
        withProgress(message = "generating word document...", value = 0, {
          
          incProgress(0.2, detail = "loading...")
          
          source("utils/helpers.R", local = FALSE)
          source(excel_calc_path, local = FALSE)
          if (file.exists(config_path)) source(config_path, local = FALSE)
          
          incProgress(0.3, detail = "calculating...")
          
          detected_mm <- run_calculations_from_excel(
            manual_month = NULL,
            file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
            file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
            payroll_end_override = .parse_period_end(.selected_pay_label()),
            target_env = globalenv()
          )
          manual_month <<- detected_mm
          .update_ref_month(detected_mm)
          
          incProgress(0.2, detail = "generating...")
          
          source(summary_path, local = FALSE)
          source(top_ten_path, local = FALSE)
          
          fallback_lines <- function() {
            stats <- list()
            for (i in 1:10) stats[[paste0("line", i)]] <- "(Data unavailable)"
            stats
          }
          summary_lines <- tryCatch(generate_summary(), error = function(e) fallback_lines())
          top10_lines <- tryCatch(generate_top_ten(), error = function(e) fallback_lines())
          
          # use ManualDB.docx template
          manual_template <- "utils/ManualDB.docx"
          if (file.exists(manual_template)) {
            source("utils/manual_word_output.R", local = FALSE)
            generate_manual_word_output(
              manual_month = manual_month,
              file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
              file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
              file_oecd_unemp = .oecd_source("unemp"),
              file_oecd_emp   = .oecd_source("emp"),
              file_oecd_inact = .oecd_source("inact"),
              vac_end_override = .parse_period_end(.selected_vac_label()),
              payroll_end_override = .parse_period_end(.selected_pay_label()),
              summary_override = summary_lines,
              top_ten_override = top10_lines,
              template_path = manual_template,
              output_path = file,
              contact_names = input$contact_names,
              verbose = FALSE
            )
            # IMPORTANT: Custom Indicators FIRST, then Key Charts — see the
            # auto handler above for the ordering rationale.
            tryCatch({
              sels <- lms_selections_data(); cd <- lms_catalog_data()
              if (!is.null(sels) && length(sels) > 0L && !is.null(cd)) {
                recs <- lms_summary_lines(sels, cd$catalog, cd$periods, cd$path)
                keep <- vapply(recs, function(rec) {
                  v <- input[[paste0("lms_line_", rec$cdid, "_", rec$baseline)]]
                  is.null(v) || isTRUE(v)
                }, logical(1))
                keys <- vapply(recs[keep], `[[`, "", "key")
                frag <- build_custom_indicators_xml(sels, cd$catalog, cd$periods, cd$path,
                                                    line_keys = keys)
                if (nzchar(frag)) append_custom_indicators(file, frag)
              }
            }, error = function(e) message("Custom indicators skipped: ", e$message))
            # append the configurable Key Charts page (native editable charts)
            tryCatch({
              configs <- .build_chart_configs()
              if (length(configs) > 0) {
                append_key_charts_page(
                  file,
                  build_chart_series(configs, "manual",
                                     list(file_a01 = uploaded_files$a01,
                                          file_rtisa = uploaded_files$rtisa,
                                          lms = lms_catalog_data()))
                )
              }
            }, error = function(e) message("Key Charts page skipped: ", e$message))
          } else {
            stop("No Word template found (ManualDB.docx)")
          }
          
          incProgress(0.1, detail = "done")
        })
        
        showNotification("Word document generated (Manual Upload)", type = "message", duration = 3)
        
      }, error = function(e) {
        message("Manual Word error: ", e$message)
        showNotification(
          paste("Word generation failed:", e$message),
          type = "error", duration = 10
        )
        # write something so the download doesn't silently fail
        writeLines(paste("Generation failed:", e$message), con = file)
      })
    }
  )
  
  # manual download: excel
  output$manual_download_excel <- downloadHandler(
    filename = function() {
      mm <- reference_manual_month()
      label <- if (!is.null(mm) && nzchar(mm)) manual_month_to_display(mm) else format(Sys.Date(), "%B %Y")
      paste0("LM Stats Audit - ", label, ".xlsx")
    },
    content = function(file) {
      if (is.null(uploaded_files$a01)) {
        showNotification("Upload at least the A01 file first", type = "warning")
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Error")
          openxlsx::writeData(wb, "Error", data.frame(Error = "Upload the A01 file to generate."))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
        return()
      }
      
      tryCatch({
        withProgress(message = "generating excel workbook...", value = 0, {
          incProgress(0.2, detail = "loading...")
          source("utils/helpers.R", local = FALSE)
          source(excel_calc_path, local = FALSE)
          if (file.exists(config_path)) source(config_path, local = FALSE)
          
          incProgress(0.4, detail = "calculating...")
          detected_mm <- run_calculations_from_excel(
            manual_month = NULL,
            file_a01 = uploaded_files$a01, file_hr1 = uploaded_files$hr1,
            file_x09 = uploaded_files$x09, file_rtisa = uploaded_files$rtisa,
            vac_end_override = .parse_period_end(.selected_vac_label()),
            payroll_end_override = .parse_period_end(.selected_pay_label()),
            target_env = globalenv()
          )
          manual_month <<- detected_mm
          .update_ref_month(detected_mm)
          
          incProgress(0.3, detail = "generating...")
          excel_env <- new.env(parent = globalenv())
          source(excel_script_path, local = excel_env)
          tmp_xlsx <- tempfile(fileext = ".xlsx")

          # build the LMS custom-indicators audit sheets from the analyst's
          # current selection (same per-line checkboxes as the Word briefing)
          lms_aud <- NULL
          tryCatch({
            sels <- lms_selections_data(); cd <- lms_catalog_data()
            if (!is.null(sels) && length(sels) > 0L && !is.null(cd)) {
              recs <- lms_summary_lines(sels, cd$catalog, cd$periods, cd$path)
              keep <- vapply(recs, function(rec) {
                v <- input[[paste0("lms_line_", rec$cdid, "_", rec$baseline)]]
                is.null(v) || isTRUE(v)
              }, logical(1))
              keys <- vapply(recs[keep], `[[`, "", "key")
              lms_aud <- build_custom_audit(sels, cd$catalog, cd$periods, cd$path,
                                            line_keys = keys)
            }
          }, error = function(e) message("LMS audit sheets skipped: ", e$message))

          excel_env$create_audit_workbook(
            output_path = tmp_xlsx,
            file_a01 = uploaded_files$a01,
            file_hr1 = uploaded_files$hr1,
            file_x09 = uploaded_files$x09,
            file_rtisa = uploaded_files$rtisa,
            file_cla01 = uploaded_files$cla01,
            file_x02 = uploaded_files$x02,
            file_oecd_unemp = .oecd_source("unemp"),
            file_oecd_emp = .oecd_source("emp"),
            file_oecd_inact = .oecd_source("inact"),
            calculations_path = calculations_path,
            config_path = config_path,
            vacancies_mode = selected_vac_period(),
            payroll_mode = selected_pay_period(),
            lms_audit = lms_aud,
            verbose = FALSE
          )
          
          incProgress(0.1, detail = "done")
          file.copy(tmp_xlsx, file, overwrite = TRUE)
        })
        showNotification("Excel workbook generated (manual mode)", type = "message", duration = 3)
      }, error = function(e) {
        message("Manual Excel error: ", e$message)
        showNotification(paste("Excel error:", e$message), type = "error", duration = 5)
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Error")
          openxlsx::writeData(wb, "Error", data.frame(Error = paste("Failed:", e$message)))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
      })
    }
  )
  
  # render: dashboard preview
  output$dashboard_preview <- renderUI({
    metrics <- dashboard_data()
    
    if (is.null(metrics)) {
      return(div(
        p(class = "govuk-body", "Click 'Preview Dashboard' to load statistics."),
        tags$ul(class = "govuk-list",
                tags$li("Employment and unemployment figures"),
                tags$li("Inactivity rates"),
                tags$li("Vacancies and payroll data"),
                tags$li("Wage statistics")
        )
      ))
    }
    
    format_change <- function(val, invert, type) {
      val <- suppressWarnings(as.numeric(gsub("^\\+", "", as.character(val))))
      if (is.na(val)) return(tags$span(class = "stat-neutral", "-"))
      
      css_class <- if (is.na(invert)) "stat-neutral"
      else if (val > 0) { if (invert) "stat-negative" else "stat-positive" }
      else if (val < 0) { if (invert) "stat-positive" else "stat-negative" }
      else "stat-neutral"
      
      sign_str <- if (val > 0) "+" else if (val < 0) "-" else ""
      abs_val <- abs(val)
      
      formatted <- if (type == "rate") paste0(sign_str, round(abs_val, 1), "pp")
      else if (type == "wages") paste0(sign_str, "£", format(round(abs_val), big.mark = ","))
      else paste0(sign_str, format(round(abs_val), big.mark = ","))
      
      tags$span(class = css_class, formatted)
    }
    
    format_current <- function(val, type) {
      val <- suppressWarnings(as.numeric(gsub("^\\+", "", as.character(val))))
      if (is.na(val)) return("-")
      if (type == "rate" || type == "wages") paste0(round(val, 1), "%")
      else format(round(val), big.mark = ",")
    }
    
    rows <- lapply(metrics, function(m) {
      tags$tr(
        tags$td(m$name),
        tags$td(format_current(m$cur, m$type)),
        tags$td(format_change(m$dq, m$invert, m$type)),
        tags$td(format_change(m$dy, m$invert, m$type)),
        tags$td(format_change(m$dc, m$invert, m$type)),
        tags$td(format_change(m$de, m$invert, m$type))
      )
    })
    
    tags$table(class = "stats-table",
               tags$thead(tags$tr(
                 tags$th("Metric"), tags$th("Current"), tags$th("vs Qtr"),
                 tags$th("vs Year"), tags$th("vs Covid"), tags$th("vs Election")
               )),
               tags$tbody(rows)
    )
  })
  
  # render: top ten preview
  output$topten_preview <- renderUI({
    top10 <- topten_data()
    
    if (is.null(top10)) {
      return(div(
        p(class = "govuk-body", "Click 'Preview Top Ten Stats' to load statistics."),
        tags$ul(class = "govuk-list",
                tags$li("Wage growth (nominal and CPI-adjusted)"),
                tags$li("Employment and unemployment rates"),
                tags$li("Payroll employment"),
                tags$li("Inactivity trends"),
                tags$li("Vacancies and redundancies")
        )
      ))
    }
    
    items <- lapply(1:10, function(i) {
      line_key <- paste0("line", i)
      line_text <- top10[[line_key]]
      if (is.null(line_text) || line_text == "") line_text <- "(Data not available)"
      tags$li(line_text)
    })
    
    tags$ol(class = "top-ten-list", items)
  })
  
  # render: summary preview
  output$summary_preview <- renderUI({
    summ <- summary_data()
    if (is.null(summ)) {
      return(p(class = "govuk-body", "Click 'Summary' to generate the narrative."))
    }
    show_lines <- setdiff(1:10, c(3, 6, 7))
    items <- lapply(show_lines, function(i) {
      txt <- summ[[paste0("line", i)]]
      if (is.null(txt) || txt == "") txt <- "(Data not available)"
      tags$li(txt)
    })
    tags$ol(class = "top-ten-list", items)
  })
  
  # render: key charts preview
  output$charts_preview <- renderPlot({
    series <- charts_preview_data()
    if (is.null(series)) {
      plot.new()
      text(0.5, 0.5, "Select metrics and a time period, then click ‘Charts’.",
           col = "grey40")
      return(invisible())
    }
    render_key_charts_preview(series)
  }, height = function() {
    series <- charts_preview_data()
    n <- if (is.null(series)) 0 else length(series)
    if (n == 0) 360 else 320 * ceiling(n / 2)
  })

  # render: custom indicators preview
  output$lms_table_preview <- renderUI({
    df <- lms_table_preview_data()
    if (is.null(df) || nrow(df) == 0L) {
      return(div(class = "govuk-hint", "No selections."))
    }
    tags$table(
      style = "border-collapse:collapse; width:100%; font-size:13px; font-family:Arial,sans-serif;",
      tags$thead(tags$tr(lapply(names(df), function(h)
        tags$th(style = "padding:6px 8px; background:#1f3864; color:#fff; border:1px solid #fff; text-align:left;", h)))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i)
        tags$tr(lapply(as.character(unlist(df[i, ])), function(v)
          tags$td(style = "padding:6px 8px; border:1px solid #d0d0d0;", v))))))
  })

  output$lms_summary_preview <- renderUI({
    sels <- lms_selections_data()
    cd   <- lms_catalog_data()
    if (is.null(sels) || length(sels) == 0L || is.null(cd)) {
      return(div(class = "govuk-hint", "No summary lines."))
    }
    recs <- lms_summary_lines(sels, cd$catalog, cd$periods, cd$path)
    if (length(recs) == 0L) return(div(class = "govuk-hint", "No summary lines."))
    # One checkbox per generated line; ticked lines go into the Word briefing.
    tagList(
      div(class = "govuk-hint", style = "margin-bottom:6px;",
          "Tick the lines to include in the Word briefing."),
      lapply(recs, function(rec)
        checkboxInput(paste0("lms_line_", rec$cdid, "_", rec$baseline),
                      label = rec$text, value = TRUE)))
  })

  # render: oecd preview
  output$oecd_preview <- renderUI({
    pd <- oecd_preview_data()
    if (is.null(pd)) {
      return(p(class = "govuk-body", "Click 'OECD' to preview uploaded international data."))
    }
    rows       <- pd$rows
    bullets    <- pd$bullets
    stale_note <- pd$stale_note
    
    # match word briefing table styling
    header_style <- paste0(
      "background-color:#1f3864; color:#ffffff; font-weight:bold;",
      "padding:8px 10px; text-align:center; border:1px solid #ffffff;"
    )
    country_cell_style <- paste0(
      "background-color:#1f3864; color:#ffffff; font-weight:bold;",
      "padding:8px 10px; text-align:center; border:1px solid #ffffff;"
    )
    data_cell_style <- paste0(
      "padding:8px 10px; text-align:center;",
      "border:1px solid #b0b0b0; background-color:#ffffff;"
    )
    alt_cell_style <- paste0(
      "padding:8px 10px; text-align:center;",
      "border:1px solid #b0b0b0; background-color:#dce6f1;"
    )
    
    header_row <- tags$tr(
      tags$th(style = header_style, ""),
      tags$th(style = header_style, "Time period¹"),
      tags$th(style = header_style, "Unemployment rate (15+², %)"),
      tags$th(style = header_style, "Employment rate (15-64², %)"),
      tags$th(style = header_style, "Inactivity Rate (15-64², %)")
    )
    
    data_rows <- lapply(seq_along(rows), function(i) {
      r   <- rows[[i]]
      sty <- if (i %% 2 == 0) alt_cell_style else data_cell_style
      tags$tr(
        tags$td(style = country_cell_style, r$country),
        tags$td(style = sty, r$period),
        tags$td(style = sty, r$unemp),
        tags$td(style = sty, r$emp),
        tags$td(style = sty, r$inact)
      )
    })
    
    footnote_parts <- list(
      tags$em(paste0(
        "Source: OECD Infra-annual labour statistics. *Latest UK data from ONS Labour Force Survey. ",
        "\u00b9Note: Included is the latest OECD data. Countries release labour market statistics on different schedules ",
        "and so reference periods vary, with some outdated. Comparisons should be treated with caution. ",
        "\u00b2Age groups differ from OECD standard where UK data is used."
      ))
    )
    if (!is.null(stale_note)) {
      footnote_parts <- c(footnote_parts, list(tags$br(), tags$em(stale_note)))
    }
    footnote <- tags$p(
      style = "font-size:12px; color:#505050; margin-top:10px;",
      footnote_parts
    )
    
    # bullet points below footnote
    bullet_html <- NULL
    if (!is.null(bullets)) {
      blist <- Filter(nzchar, c(bullets$bullet1, bullets$bullet2, bullets$bullet3))
      if (length(blist) > 0) {
        bullet_html <- tags$ul(
          style = "margin-top:12px; font-size:13px; line-height:1.6;",
          lapply(blist, function(b) tags$li(style = "margin-bottom:6px;", tags$strong(b)))
        )
      }
    }
    
    tagList(
      div(style = "overflow-x:auto; margin-bottom:8px;",
          tags$table(
            style = "border-collapse:collapse; width:100%; font-size:14px; font-family:Arial,sans-serif;",
            tags$thead(header_row),
            tags$tbody(data_rows)
          )
      ),
      footnote,
      bullet_html
    )
  })
}

shinyApp(ui = ui, server = server)