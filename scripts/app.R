# ============================================================
# Cloud-Based Predictive Network Traffic Congestion
# Forecasting & Intelligent Routing System
# ============================================================

# ── Libraries ──
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(tidyr)
library(tseries)
library(ggcorrplot)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Routing recommendation engine
recommend_routing <- function(score, latency_val, packet_loss_val, bw_util) {
  if (score >= 75 || packet_loss_val >= 0.7) {
    list(
      protocol  = "QoS-Aware Load Balancing + Traffic Shaping",
      severity  = "CRITICAL",
      color     = "#FF4444",
      icon      = "exclamation-triangle",
      rationale = paste0(
        "Severe congestion detected (Score: ", round(score, 1), "/100). ",
        "High packet loss and latency demand immediate multi-path ",
        "redistribution with QoS prioritisation."
      ),
      steps = c(
        "Activate weighted traffic shaping policies",
        "Redistribute flows across all available paths (WRR)",
        "Prioritise latency-sensitive traffic via DSCP marking",
        "Monitor and alert on SLA breach thresholds"
      )
    )
  } else if (score >= 50 || bw_util >= 0.75) {
    list(
      protocol  = "ECMP — Equal-Cost Multi-Path Routing",
      severity  = "HIGH",
      color     = "#FF8C00",
      icon      = "warning",
      rationale = paste0(
        "Elevated congestion (Score: ", round(score, 1), "/100). ",
        "ECMP will distribute traffic across multiple equal-cost paths, ",
        "reducing per-link load and preventing bottleneck formation."
      ),
      steps = c(
        "Enable ECMP across available equal-cost paths",
        "Configure per-flow load balancing hashing",
        "Monitor link utilisation for rebalancing triggers",
        "Pre-stage failover paths for rapid switching"
      )
    )
  } else if (score >= 25) {
    list(
      protocol  = "Adaptive OSPF with Dynamic Path Selection",
      severity  = "MEDIUM",
      color     = "#FFC300",
      icon      = "info-circle",
      rationale = paste0(
        "Moderate congestion (Score: ", round(score, 1), "/100). ",
        "OSPF with adaptive metrics will dynamically prefer ",
        "less-loaded paths. Preemptive action prevents escalation."
      ),
      steps = c(
        "Adjust OSPF link cost metrics based on utilisation",
        "Enable fast re-convergence timers (BFD)",
        "Monitor trend — escalate to ECMP if score increases",
        "Log event for predictive model feedback"
      )
    )
  } else {
    list(
      protocol  = "Standard OSPF — No Intervention Required",
      severity  = "LOW",
      color     = "#00C853",
      icon      = "check-circle",
      rationale = paste0(
        "Network operating normally (Score: ", round(score, 1), "/100). ",
        "Standard OSPF shortest-path routing is optimal. ",
        "Continue monitoring for early congestion signals."
      ),
      steps = c(
        "Maintain standard OSPF routing",
        "Continue passive metric monitoring",
        "No rerouting action required",
        "System will auto-alert if score exceeds 25"
      )
    )
  }
}

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",

  # ── Header ──
  dashboardHeader(
    title = span(icon("network-wired"),
                 "NetCast — Traffic Intelligence"),
    titleWidth = 320
  ),

  # ── Sidebar ──
  dashboardSidebar(
    width = 270,
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo {
        background-color: #0A1628;
        color: #00B4D8;
        font-weight: 700;
      }
      .skin-blue .main-header .navbar { background-color: #112240; }
      .skin-blue .main-sidebar        { background-color: #0D1F3C; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background-color: #00B4D8;
        border-left: 3px solid #90E0EF;
      }
      .sidebar-menu > li > a  { color: #8892B0; }
      .content-wrapper        { background-color: #0A1628; }
      .box                    { background-color: #112240;
                                border-top-color: #00B4D8; }
      .box-header             { color: #CCD6F6; }
      .routing-card {
        background: #112240;
        border-radius: 10px;
        padding: 20px;
        border-left: 5px solid;
        margin-bottom: 15px;
      }
      .step-item {
        background: #0D1F3C;
        border-radius: 6px;
        padding: 10px 14px;
        margin: 6px 0;
        color: #CCD6F6;
        font-size: 13px;
      }
      .step-item::before { content: '▶  '; color: #00B4D8; }
      
      .dataTables_wrapper {
        color: #CCD6F6 !important;
      }
      table.dataTable tbody td {
        color: #FFFFFF !important;
        border-color: #1E3A5F !important;
      }
      table.dataTable thead th {
        color: #00B4D8 !important;
        border-color: #1E3A5F !important;
        background-color: #0D1F3C !important;
      }
      table.dataTable tbody tr {
        background-color: #112240 !important;
      }
      table.dataTable tbody tr:hover {
        background-color: #1E3A5F !important;
      }
      .dataTables_filter input,
      .dataTables_length select {
        color: #CCD6F6 !important;
        background-color: #0D1F3C !important;
        border-color: #1E3A5F !important;
      }
      .dataTables_info,
      .dataTables_paginate {
        color: #CCD6F6 !important;
      }
      .paginate_button {
        color: #CCD6F6 !important;
      }
    "))),

    fileInput("data_file", "📂 Load Dataset (.xlsx)",
              accept = c(".xlsx"),
              placeholder = "Upload Excel file"),

    hr(),

    sidebarMenu(
      menuItem("📊 Live Dashboard",
               tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("📈 Time Series Analysis",
               tabName = "timeseries", icon = icon("chart-line")),
      menuItem("🔮 Congestion Forecast",
               tabName = "forecast",   icon = icon("brain")),
      menuItem("🚨 Alert System",
               tabName = "alerts",     icon = icon("bell")),
      menuItem("🔀 Routing Advisory",
               tabName = "routing",    icon = icon("route")),
      menuItem("📋 Data & Export",
               tabName = "rawdata",    icon = icon("table"))
    ),

    hr(),
    tags$div(style = "padding:15px;",
      tags$p(style = "color:#8892B0; font-size:11px; line-height:1.6;",
        "Cloud-Based Predictive Network Traffic Congestion",
        "Forecasting & Intelligent Routing System"),
      tags$p(style = "color:#00B4D8; font-size:10px;",
        "Electronics & Telematics Engineering")
    )
  ),

  # ── Body ──
  dashboardBody(
    tabItems(

      # ── Tab 1: Live Dashboard ──
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("vb_records",    width = 3),
          valueBoxOutput("vb_congestion", width = 3),
          valueBoxOutput("vb_latency",    width = 3),
          valueBoxOutput("vb_packetloss", width = 3)
        ),
        fluidRow(
          box(width = 6, title = "Overload Status Distribution",
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_overload", height = 280),
                          color = "#00B4D8")),
          box(width = 6, title = "Congestion Score Over Time",
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_score_time", height = 280),
                          color = "#00B4D8"))
        ),
        fluidRow(
          box(width = 6, title = "Traffic Load by Time of Day",
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_tod", height = 260),
                          color = "#00B4D8")),
          box(width = 6, title = "Metric Correlation Heatmap",
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_corr", height = 260),
                          color = "#00B4D8"))
        )
      ),

      # ── Tab 2: Time Series ──
      tabItem(tabName = "timeseries",
        fluidRow(
          box(width = 12, title = "Select Metric",
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6,
                selectInput("ts_metric", "Metric to Analyse:",
                  choices = list(
                    "Congestion Score"          = "congestion_score",
                    "Traffic Load"              = "traffic_load",
                    "Network Utilization"       = "network_utilization",
                    "Latency"                   = "latency",
                    "Packet Loss"               = "packet_loss",
                    "Bandwidth Utilization"     = "bandwidth_utilization"
                  ), selected = "congestion_score")),
              column(6,
                sliderInput("ts_days", "Display Window (days):",
                  min = 7, max = 90, value = 30))
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Time Series with Rolling Average",
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_ts", height = 350),
                          color = "#00B4D8"))
        ),
        fluidRow(
          box(width = 6, title = "Autocorrelation — ACF",
              status = "info", solidHeader = TRUE,
              withSpinner(plotOutput("plot_acf", height = 260),
                          color = "#00B4D8")),
          box(width = 6, title = "Partial Autocorrelation — PACF",
              status = "info", solidHeader = TRUE,
              withSpinner(plotOutput("plot_pacf", height = 260),
                          color = "#00B4D8"))
        ),
        fluidRow(
          box(width = 12, title = "Decomposition & Statistics",
              status = "warning", solidHeader = TRUE,
              verbatimTextOutput("ts_stats"))
        )
      ),

      # ── Tab 3: Forecast ──
      tabItem(tabName = "forecast",
        fluidRow(
          box(width = 12, title = "Forecast Configuration",
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4,
                selectInput("fc_metric", "Metric to Forecast:",
                  choices = list(
                    "Congestion Score"        = "congestion_score",
                    "Traffic Load"            = "traffic_load",
                    "Latency"                 = "latency",
                    "Bandwidth Utilization"   = "bandwidth_utilization",
                    "Packet Loss"             = "packet_loss"
                  ), selected = "congestion_score")),
              column(4,
                sliderInput("fc_horizon", "Forecast Horizon (hours):",
                  min = 6, max = 48, value = 24, step = 6)),
              column(4, br(),
                actionButton("run_forecast", "▶  Run ARIMA Forecast",
                  class = "btn btn-primary btn-block",
                  style = "background:#00B4D8; border-color:#00B4D8;
                           font-weight:700;"))
            )
          )
        ),
        fluidRow(
          box(width = 12,
              title = "ARIMA Forecast with Confidence Intervals",
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_forecast", height = 400),
                          color = "#00B4D8"))
        ),
        fluidRow(
          box(width = 6, title = "Model Summary",
              status = "info", solidHeader = TRUE,
              verbatimTextOutput("fc_summary")),
          box(width = 6, title = "Accuracy Metrics",
              status = "success", solidHeader = TRUE,
              tableOutput("fc_accuracy"))
        ),
        fluidRow(
          box(width = 12, title = "Predicted Congestion Events",
              status = "warning", solidHeader = TRUE,
              DT::dataTableOutput("fc_events"))
        )
      ),

      # ── Tab 4: Alerts ──
      tabItem(tabName = "alerts",
        fluidRow(
          valueBoxOutput("vb_total_alerts",    width = 3),
          valueBoxOutput("vb_critical_alerts", width = 3),
          valueBoxOutput("vb_high_alerts",     width = 3),
          valueBoxOutput("vb_alert_rate",      width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Alert Threshold Configuration",
              status = "warning", solidHeader = TRUE,
            fluidRow(
              column(3,
                sliderInput("thresh_critical", "CRITICAL Threshold:",
                  min = 50, max = 100, value = 75)),
              column(3,
                sliderInput("thresh_high", "HIGH Threshold:",
                  min = 30, max = 74,  value = 50)),
              column(3,
                sliderInput("thresh_medium", "MEDIUM Threshold:",
                  min = 10, max = 29,  value = 25)),
              column(3, br(),
                actionButton("apply_thresh", "Apply Thresholds",
                  class = "btn btn-warning btn-block",
                  style = "font-weight:700;"))
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Alert Log",
              status = "danger", solidHeader = TRUE,
              DT::dataTableOutput("alert_log"))
        ),
        fluidRow(
          box(width = 12, title = "Congestion Events Timeline",
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_alerts", height = 300),
                          color = "#00B4D8"))
        )
      ),

      # ── Tab 5: Routing ──
      tabItem(tabName = "routing",
        fluidRow(
          box(width = 12, title = "Live Network Metric Input",
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3,
                sliderInput("rt_latency", "Latency (normalised):",
                  min = 0, max = 1, value = 0.5, step = 0.01)),
              column(3,
                sliderInput("rt_pkt_loss", "Packet Loss (normalised):",
                  min = 0, max = 1, value = 0.3, step = 0.01)),
              column(3,
                sliderInput("rt_bw_util", "Bandwidth Utilisation:",
                  min = 0, max = 1, value = 0.6, step = 0.01)),
              column(3,
                sliderInput("rt_net_util", "Network Utilisation:",
                  min = 0, max = 1, value = 0.5, step = 0.01))
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vb_rt_score",    width = 4),
          valueBoxOutput("vb_rt_severity", width = 4),
          valueBoxOutput("vb_rt_protocol", width = 4)
        ),
        fluidRow(
          box(width = 12,
              title = "Recommended Routing Protocol & Action Plan",
              status = "primary", solidHeader = TRUE,
              uiOutput("routing_card"))
        ),
        fluidRow(
          box(width = 12, title = "Routing Decision Matrix",
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("plot_rt_matrix", height = 300),
                          color = "#00B4D8"))
        )
      ),

      # ── Tab 6: Raw Data ──
      tabItem(tabName = "rawdata",
        fluidRow(
          box(width = 12, title = "Filter Options",
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3,
                selectInput("filter_overload", "Overload Status:",
                  choices = c("All" = "all", "Normal" = "0",
                              "Congested" = "1"),
                  selected = "all")),
              column(3,
                selectInput("filter_region", "Region:",
                  choices = c("All" = "all", "North" = "0",
                              "South" = "1", "East" = "2",
                              "West" = "3"),
                  selected = "all")),
              column(3,
                selectInput("filter_tod", "Time of Day:",
                  choices = c("All" = "all", "Night" = "0",
                              "Day" = "1", "Evening" = "2"),
                  selected = "all")),
              column(3, br(),
                downloadButton("download_data", "⬇️  Export to CSV",
                  class = "btn btn-success btn-block",
                  style = "font-weight:700;"))
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Dataset — 6G Network Slicing QoS",
              status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("raw_table"))
        )
      )
    )
  )
)

# ============================================================
# SERVER (placeholder — we add this next step)
# ============================================================
server <- function(input, output, session) {

  # ── Reactive: Load Data ──
  df <- reactive({
    req(input$data_file)
    d <- read_excel(input$data_file$datapath)
    colnames(d) <- c(
      "slice_id", "timestamp", "device_id", "traffic_load",
      "traffic_type", "network_utilization", "latency",
      "packet_loss", "signal_strength", "bandwidth_utilization",
      "slice_failure", "qos_throughput", "overload_status",
      "device_type", "region", "failure_count", "time_of_day",
      "weather"
    )
    d$timestamp  <- as.POSIXct(d$timestamp)
    d$hour       <- hour(d$timestamp)
    d$date       <- as.Date(d$timestamp)
    d$congestion_score <- with(d, (
      rescale(latency,               to = c(0, 30)) +
      rescale(packet_loss,           to = c(0, 25)) +
      rescale(bandwidth_utilization, to = c(0, 25)) +
      rescale(network_utilization,   to = c(0, 20))
    ))
    d
  })

  # ── Value Boxes: Dashboard ──
  output$vb_records <- renderValueBox({
    req(df())
    valueBox(format(nrow(df()), big.mark = ","),
             "Total Records", icon = icon("database"), color = "blue")
  })

  output$vb_congestion <- renderValueBox({
    req(df())
    pct <- round(mean(df()$overload_status) * 100, 1)
    valueBox(paste0(pct, "%"), "Congestion Rate",
             icon = icon("exclamation-triangle"),
             color = if (pct > 30) "red" else if (pct > 15) "orange" else "green")
  })

  output$vb_latency <- renderValueBox({
    req(df())
    valueBox(round(mean(df()$latency), 3),
             "Avg Latency (norm.)", icon = icon("clock"), color = "blue")
  })

  output$vb_packetloss <- renderValueBox({
    req(df())
    valueBox(paste0(round(mean(df()$packet_loss) * 100, 2), "%"),
             "Avg Packet Loss", icon = icon("times-circle"), color = "red")
  })

  # ── Dashboard: Overload Plot ──
  output$plot_overload <- renderPlotly({
    req(df())
    counts <- df() %>%
      mutate(status = ifelse(overload_status == 1,
                             "Congested", "Normal")) %>%
      count(status)
    plot_ly(counts, x = ~status, y = ~n, type = "bar",
            color = ~status,
            colors = c("Normal" = "#00C853", "Congested" = "#FF4444")) %>%
      layout(title = list(text = "Overload Distribution",
                          font = list(color = "#00B4D8")),
             paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             showlegend = FALSE)
  })

  # ── Dashboard: Score Over Time ──
  output$plot_score_time <- renderPlotly({
    req(df())
    daily <- df() %>%
      group_by(date) %>%
      summarise(avg = mean(congestion_score),
                mx  = max(congestion_score))
    plot_ly(daily) %>%
      add_lines(x = ~date, y = ~avg, name = "Daily Average",
                line = list(color = "#00B4D8", width = 2)) %>%
      add_lines(x = ~date, y = ~mx,  name = "Daily Max",
                line = list(color = "#FF4444", width = 1,
                            dash = "dot")) %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             xaxis = list(gridcolor = "#1E3A5F"),
             yaxis = list(gridcolor = "#1E3A5F",
                          title = "Congestion Score"))
  })

  # ── Dashboard: Time of Day ──
  output$plot_tod <- renderPlotly({
    req(df())
    d <- df() %>%
      mutate(tod = case_when(
        time_of_day == 0 ~ "Night",
        time_of_day == 1 ~ "Day",
        TRUE             ~ "Evening"))
    plot_ly(d, x = ~tod, y = ~congestion_score,
            type = "box", color = ~tod,
            colors = c("Night"   = "#0077B6",
                       "Day"     = "#00B4D8",
                       "Evening" = "#90E0EF")) %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"), showlegend = FALSE,
             xaxis = list(title = ""),
             yaxis = list(title = "Congestion Score",
                          gridcolor = "#1E3A5F"))
  })

  # ── Dashboard: Correlation Heatmap ──
  output$plot_corr <- renderPlotly({
    req(df())
    vars <- c("traffic_load", "network_utilization", "latency",
              "packet_loss", "bandwidth_utilization",
              "qos_throughput", "overload_status")
    labs <- c("Traffic","NetUtil","Latency",
              "PktLoss","BWUtil","QoS","Overload")
    cm <- cor(df()[, vars], use = "complete.obs")
    rownames(cm) <- labs; colnames(cm) <- labs
    plot_ly(z = cm, x = labs, y = labs, type = "heatmap",
            colorscale = list(c(0,"#0A1628"),
                              c(0.5,"#0077B6"),
                              c(1,"#00B4D8")),
            text = round(cm, 2), texttemplate = "%{text}") %>%
      layout(paper_bgcolor = "#0A1628",
             font = list(color = "#CCD6F6"))
  })

  # ── Time Series: Main Plot ──
  output$plot_ts <- renderPlotly({
    req(df())
    d <- df() %>%
      arrange(timestamp) %>%
      tail(input$ts_days * 24) %>%
      select(timestamp, value = all_of(input$ts_metric))
    d$roll <- stats::filter(d$value, rep(1/24, 24), sides = 2)
    plot_ly(d) %>%
      add_lines(x = ~timestamp, y = ~value, name = "Raw",
                line = list(color = "#8892B0", width = 0.8)) %>%
      add_lines(x = ~timestamp, y = ~roll,  name = "24h Average",
                line = list(color = "#00B4D8", width = 2)) %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             xaxis = list(gridcolor = "#1E3A5F"),
             yaxis = list(gridcolor = "#1E3A5F"))
  })

  # ── Time Series: ACF ──
  output$plot_acf <- renderPlot({
    req(df())
    par(bg = "#0A1628", col.axis = "#8892B0",
        col.lab = "#CCD6F6", col.main = "#00B4D8", fg = "#8892B0")
    acf(df()[[input$ts_metric]], lag.max = 48,
        main = "ACF", col = "#00B4D8", lwd = 2)
  }, bg = "#0A1628")

  # ── Time Series: PACF ──
  output$plot_pacf <- renderPlot({
    req(df())
    par(bg = "#0A1628", col.axis = "#8892B0",
        col.lab = "#CCD6F6", col.main = "#00B4D8", fg = "#8892B0")
    pacf(df()[[input$ts_metric]], lag.max = 48,
         main = "PACF", col = "#FF4444", lwd = 2)
  }, bg = "#0A1628")

  # ── Time Series: Stats ──
  output$ts_stats <- renderPrint({
    req(df())
    x <- df()[[input$ts_metric]]
    cat("=== DESCRIPTIVE STATISTICS ===\n")
    print(summary(x))
    cat("\nStd Dev: ",  round(sd(x),  6))
    cat("\nVariance:", round(var(x), 6))
    cat("\n\n=== ADF STATIONARITY TEST ===\n")
    tryCatch({
      r <- adf.test(ts(x, frequency = 24))
      cat("ADF Statistic:", round(r$statistic, 4), "\n")
      cat("p-value:      ", round(r$p.value,   4), "\n")
      cat("Result:       ",
          if (r$p.value < 0.05) "STATIONARY ✓"
          else "NON-STATIONARY — differencing needed", "\n")
    }, error = function(e) cat("Error:", e$message))
  })

  # ── Forecast: ARIMA ──
  fc_result <- eventReactive(input$run_forecast, {
    req(df())
    x   <- ts(df()[[input$fc_metric]], frequency = 24)
    fit <- auto.arima(x, seasonal = TRUE,
                      stepwise = TRUE, approximation = TRUE)
    fc  <- forecast(fit, h = input$fc_horizon, level = c(80, 95))
    last_t  <- tail(df()$timestamp, 1)
    fc_times <- seq(last_t + 3600, by = 3600,
                    length.out = input$fc_horizon)
    list(fit = fit, fc = fc, times = fc_times)
  })

  output$plot_forecast <- renderPlotly({
    req(fc_result())
    r  <- fc_result()
    ht <- tail(df()$timestamp, 168)
    hy <- tail(df()[[input$fc_metric]], 168)
    fm <- as.numeric(r$fc$mean)
    fl <- as.numeric(r$fc$lower[, 2])
    fu <- as.numeric(r$fc$upper[, 2])
    plot_ly() %>%
      add_ribbons(x = r$times, ymin = fl, ymax = fu,
                  fillcolor = "rgba(0,180,216,0.15)",
                  line = list(color = "transparent"),
                  name = "95% CI") %>%
      add_lines(x = ht, y = hy, name = "Historical",
                line = list(color = "#8892B0", width = 1)) %>%
      add_lines(x = r$times, y = fm, name = "Forecast",
                line = list(color = "#00B4D8", width = 2.5)) %>%
      add_lines(x = c(min(ht), max(r$times)),
                y = rep(quantile(hy, 0.75), 2),
                name = "Congestion Threshold",
                line = list(color = "#FF4444", dash = "dot")) %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             xaxis = list(gridcolor = "#1E3A5F"),
             yaxis = list(gridcolor = "#1E3A5F"))
  })

  output$fc_summary <- renderPrint({
    req(fc_result())
    print(fc_result()$fit)
  })

  output$fc_accuracy <- renderTable({
    req(fc_result())
    a <- accuracy(fc_result()$fc)
    data.frame(
      Metric = c("MAE", "RMSE", "MAPE", "MASE"),
      Value  = round(c(a[1,"MAE"], a[1,"RMSE"],
                       a[1,"MAPE"], a[1,"MASE"]), 4)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$fc_events <- DT::renderDataTable({
    req(fc_result())
    r   <- fc_result()
    thr <- quantile(tail(df()[[input$fc_metric]], 168), 0.75)
    fm  <- as.numeric(r$fc$mean)
    data.frame(
      Timestamp = format(r$times, "%Y-%m-%d %H:%M"),
      Forecast  = round(fm, 4),
      Threshold = round(thr, 4),
      Alert     = ifelse(fm > thr,
                         "⚠️ CONGESTION PREDICTED", "✅ Normal")
    ) %>%
      DT::datatable(options = list(pageLength = 10),
                    rownames = FALSE)
  })

  # ── Alerts ──
  alert_data <- reactive({
    req(df())
    df() %>%
      mutate(
        severity = case_when(
          congestion_score >= input$thresh_critical ~ "CRITICAL",
          congestion_score >= input$thresh_high     ~ "HIGH",
          congestion_score >= input$thresh_medium   ~ "MEDIUM",
          TRUE                                      ~ "NORMAL"
        )
      ) %>%
      filter(severity != "NORMAL")
  })

  output$vb_total_alerts <- renderValueBox({
    req(alert_data())
    valueBox(nrow(alert_data()), "Total Alerts",
             icon = icon("bell"), color = "yellow")
  })

  output$vb_critical_alerts <- renderValueBox({
    req(alert_data())
    n <- sum(alert_data()$severity == "CRITICAL")
    valueBox(n, "Critical Alerts",
             icon = icon("exclamation-triangle"), color = "red")
  })

  output$vb_high_alerts <- renderValueBox({
    req(alert_data())
    n <- sum(alert_data()$severity == "HIGH")
    valueBox(n, "High Alerts",
             icon = icon("warning"), color = "orange")
  })

  output$vb_alert_rate <- renderValueBox({
    req(df(), alert_data())
    rate <- round(nrow(alert_data()) / nrow(df()) * 100, 1)
    valueBox(paste0(rate, "%"), "Alert Rate",
             icon = icon("percent"), color = "purple")
  })

  output$alert_log <- DT::renderDataTable({
    req(alert_data())
    alert_data() %>%
      select(timestamp, congestion_score, latency,
             packet_loss, bandwidth_utilization, severity) %>%
      mutate(congestion_score = round(congestion_score, 2),
             timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>%
      DT::datatable(options = list(pageLength = 10),
                    rownames = FALSE)
  })

  output$plot_alerts <- renderPlotly({
    req(df())
    plot_ly(df(), x = ~timestamp, y = ~congestion_score,
            type = "scatter", mode = "lines",
            line = list(color = "#00B4D8", width = 1)) %>%
      add_lines(x = range(df()$timestamp),
                y = rep(input$thresh_critical, 2),
                line = list(color = "#FF4444", dash = "dot"),
                name = "Critical") %>%
      add_lines(x = range(df()$timestamp),
                y = rep(input$thresh_high, 2),
                line = list(color = "#FF8C00", dash = "dot"),
                name = "High") %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             xaxis = list(gridcolor = "#1E3A5F"),
             yaxis = list(gridcolor = "#1E3A5F",
                          title = "Congestion Score"))
  })

  # ── Routing ──
  rt_score <- reactive({
    (input$rt_latency  * 30) + (input$rt_pkt_loss * 25) +
    (input$rt_bw_util  * 25) + (input$rt_net_util * 20)
  })

  rt_rec <- reactive({
    recommend_routing(rt_score(), input$rt_latency,
                      input$rt_pkt_loss, input$rt_bw_util)
  })

  output$vb_rt_score <- renderValueBox({
    valueBox(round(rt_score(), 1), "Congestion Score (0-100)",
             icon = icon("tachometer-alt"),
             color = if (rt_score() >= 75) "red"
                     else if (rt_score() >= 50) "orange"
                     else if (rt_score() >= 25) "yellow"
                     else "green")
  })

  output$vb_rt_severity <- renderValueBox({
    r <- rt_rec()
    valueBox(r$severity, "Alert Severity",
             icon = icon(r$icon),
             color = switch(r$severity,
               "CRITICAL" = "red",   "HIGH"   = "orange",
               "MEDIUM"   = "yellow", "LOW"   = "green"))
  })

  output$vb_rt_protocol <- renderValueBox({
    valueBox("See Advisory Below", "Recommended Protocol",
             icon = icon("route"), color = "blue")
  })

  output$routing_card <- renderUI({
    r <- rt_rec()
    tagList(
      tags$div(class = "routing-card",
        style = paste0("border-left-color:", r$color),
        tags$h4(r$protocol,
                style = paste0("color:", r$color,
                               "; font-size:20px; font-weight:700;")),
        tags$p(r$rationale,
               style = "color:#8892B0; font-size:13px;
                        line-height:1.6;"),
        tags$hr(style = paste0("border-color:", r$color, "44;")),
        tags$h5("Recommended Actions:",
                style = "color:#00B4D8;"),
        lapply(r$steps, function(s) {
          tags$div(s, class = "step-item")
        }),
        tags$hr(style = paste0("border-color:", r$color, "44;")),
        tags$p(style = "font-size:11px; color:#8892B0;",
          "Future scope: This recommendation can be automatically ",
          "pushed to SDN-capable routers via NETCONF/YANG or ",
          "OpenFlow APIs for fully autonomous congestion mitigation.")
      )
    )
  })

  output$plot_rt_matrix <- renderPlotly({
    scores <- seq(0, 100, by = 5)
    protos <- sapply(scores, function(s) {
      if (s >= 75)      "QoS + Traffic Shaping"
      else if (s >= 50) "ECMP Multi-Path"
      else if (s >= 25) "Adaptive OSPF"
      else              "Standard OSPF"
    })
    cols <- c(
      "Standard OSPF"         = "#00C853",
      "Adaptive OSPF"         = "#FFC300",
      "ECMP Multi-Path"       = "#FF8C00",
      "QoS + Traffic Shaping" = "#FF4444"
    )
    plot_ly(x = scores, y = protos, type = "scatter",
            mode = "markers",
            marker = list(color = cols[protos],
                          size = 18, symbol = "square"),
            text  = paste0("Score: ", scores,
                           "<br>Protocol: ", protos),
            hoverinfo = "text") %>%
      add_segments(x = rt_score(), xend = rt_score(),
                   y = 0.5, yend = 4.5,
                   line = list(color = "white",
                               dash = "dot", width = 2),
                   showlegend = FALSE, inherit = FALSE) %>%
      layout(paper_bgcolor = "#0A1628", plot_bgcolor = "#112240",
             font = list(color = "#CCD6F6"),
             xaxis = list(title = "Congestion Score",
                          gridcolor = "#1E3A5F"),
             yaxis = list(title = "",
                          gridcolor = "#1E3A5F"))
  })

  # ── Raw Data ──
  filtered_df <- reactive({
    req(df())
    d <- df()
    if (input$filter_overload != "all")
      d <- d %>% filter(overload_status ==
                        as.numeric(input$filter_overload))
    if (input$filter_region != "all")
      d <- d %>% filter(region == as.numeric(input$filter_region))
    if (input$filter_tod != "all")
      d <- d %>% filter(time_of_day == as.numeric(input$filter_tod))
    d
  })

  output$raw_table <- DT::renderDataTable({
    req(filtered_df())
    filtered_df() %>%
      select(timestamp, traffic_load, network_utilization,
             latency, packet_loss, bandwidth_utilization,
             qos_throughput, overload_status, congestion_score) %>%
      mutate(across(where(is.numeric), ~ round(., 4)),
             timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>%
      DT::datatable(options = list(pageLength = 15,
                                   scrollX = TRUE),
                    rownames = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("network_traffic_filtered_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui = ui, server = server)