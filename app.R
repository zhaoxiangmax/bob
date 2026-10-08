library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)

# ── DB connection ─────────────────────────────────────────────────────────────
con <- dbConnect(
  RPostgres::Postgres(),
  host     = "ec2-13-236-76-115.ap-southeast-2.compute.amazonaws.com",
  port     = 5432,
  dbname   = "ecommerce",
  user     = "participant_09",
  password = "hack2026_09"
)

onStop(function() dbDisconnect(con))

# ── Helper: format large numbers ──────────────────────────────────────────────
fmt_million <- function(x) paste0("$", round(x / 1e6, 1), "M")
fmt_k       <- function(x) paste0(round(x / 1e3, 1), "K")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "Ecommerce Orders Dashboard",
  theme = bs_theme(bootswatch = "flatly", primary = "#3b82d4"),
  bg    = "#1f2328",
  inverse = TRUE,

  # Sidebar filters (shared across all tabs)
  sidebar = sidebar(
    title = "Filters",
    selectInput("country_filter", "Country",
      choices  = c("All"),
      selected = "All"
    ),
    selectInput("category_filter", "Category",
      choices  = c("All"),
      selected = "All"
    ),
    selectInput("year_filter", "Year",
      choices  = c("All", "2024", "2025", "2026"),
      selected = "All"
    ),
    selectInput("segment_filter", "Customer Segment",
      choices  = c("All", "Regular", "Premium", "VIP"),
      selected = "All"
    ),
    hr(),
    helpText("Data: ecommerce.orders (960K rows)")
  ),

  # ── Tab 1: KPIs ─────────────────────────────────────────────────────────────
  nav_panel("KPI Overview",
    layout_columns(
      fill = FALSE,
      value_box("Total Revenue",    textOutput("kpi_revenue"),    showcase = bsicons::bs_icon("currency-dollar"), theme = "primary"),
      value_box("Total Profit",     textOutput("kpi_profit"),     showcase = bsicons::bs_icon("graph-up-arrow"),  theme = "success"),
      value_box("Total Orders",     textOutput("kpi_orders"),     showcase = bsicons::bs_icon("bag-check"),       theme = "info"),
      value_box("Avg Order Value",  textOutput("kpi_aov"),        showcase = bsicons::bs_icon("receipt"),         theme = "warning"),
      value_box("Avg Profit Margin",textOutput("kpi_margin"),     showcase = bsicons::bs_icon("percent"),         theme = "secondary"),
      value_box("Avg Delivery Days",textOutput("kpi_delivery"),   showcase = bsicons::bs_icon("truck"),           theme = "danger")
    ),
    card(
      card_header("Monthly Revenue & Profit"),
      plotOutput("revenue_trend", height = "320px")
    )
  ),

  # ── Tab 2: Order Status ──────────────────────────────────────────────────────
  nav_panel("Order Status",
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Order Status Mix"),
        plotOutput("status_pie", height = "340px")
      ),
      card(
        card_header("Delivery Status Breakdown"),
        plotOutput("delivery_bar", height = "340px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Payment Status"),
        plotOutput("payment_bar", height = "280px")
      ),
      card(
        card_header("Orders by Day Type"),
        plotOutput("weekend_bar", height = "280px")
      )
    )
  ),

  # ── Tab 3: Categories & Brands ───────────────────────────────────────────────
  nav_panel("Categories & Brands",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Revenue by Category"),
        plotOutput("category_bar", height = "300px")
      ),
      card(
        card_header("Profit Margin by Category"),
        plotOutput("margin_bar", height = "300px")
      )
    ),
    card(
      card_header("Top 15 Brands by Revenue"),
      plotOutput("brand_bar", height = "380px")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Populate filter dropdowns dynamically
  observe({
    countries <- dbGetQuery(con, "SELECT DISTINCT country FROM orders ORDER BY country")$country
    updateSelectInput(session, "country_filter", choices = c("All", countries))

    cats <- dbGetQuery(con, "SELECT DISTINCT category FROM orders ORDER BY category")$category
    updateSelectInput(session, "category_filter", choices = c("All", cats))
  })

  # Build WHERE clause from filters
  where_clause <- reactive({
    clauses <- character(0)
    if (input$country_filter  != "All") clauses <- c(clauses, sprintf("country = '%s'",          input$country_filter))
    if (input$category_filter != "All") clauses <- c(clauses, sprintf("category = '%s'",         input$category_filter))
    if (input$year_filter     != "All") clauses <- c(clauses, sprintf("order_year = %s",         input$year_filter))
    if (input$segment_filter  != "All") clauses <- c(clauses, sprintf("customer_segment = '%s'", input$segment_filter))
    if (length(clauses) == 0) "1=1" else paste(clauses, collapse = " AND ")
  })

  # ── KPI data ────────────────────────────────────────────────────────────────
  kpi_data <- reactive({
    dbGetQuery(con, sprintf("
      SELECT
        SUM(total_price_usd)      AS revenue,
        SUM(profit_usd)           AS profit,
        COUNT(*)                  AS orders,
        AVG(total_price_usd)      AS aov,
        AVG(profit_margin_percent)AS margin,
        AVG(delivery_days)        AS delivery
      FROM orders WHERE %s", where_clause()))
  })

  output$kpi_revenue  <- renderText(fmt_million(kpi_data()$revenue))
  output$kpi_profit   <- renderText(fmt_million(kpi_data()$profit))
  output$kpi_orders   <- renderText(fmt_k(kpi_data()$orders))
  output$kpi_aov      <- renderText(paste0("$", round(kpi_data()$aov, 2)))
  output$kpi_margin   <- renderText(paste0(round(kpi_data()$margin, 1), "%"))
  output$kpi_delivery <- renderText(paste0(round(kpi_data()$delivery, 1), " days"))

  # ── Revenue trend ────────────────────────────────────────────────────────────
  output$revenue_trend <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT order_year, order_month,
        SUM(total_price_usd) AS revenue,
        SUM(profit_usd)      AS profit
      FROM orders WHERE %s
      GROUP BY order_year, order_month
      ORDER BY order_year, order_month", where_clause())) %>%
      mutate(period = as.Date(sprintf("%d-%02d-01", order_year, order_month)))

    ggplot(df, aes(x = period)) +
      geom_area(aes(y = revenue, fill = "Revenue"), alpha = 0.4) +
      geom_area(aes(y = profit,  fill = "Profit"),  alpha = 0.6) +
      geom_line(aes(y = revenue, colour = "Revenue"), linewidth = 0.8) +
      geom_line(aes(y = profit,  colour = "Profit"),  linewidth = 0.8) +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
      scale_fill_manual(values   = c("Revenue" = "#3b82d4", "Profit" = "#22c55e")) +
      scale_colour_manual(values = c("Revenue" = "#3b82d4", "Profit" = "#22c55e")) +
      labs(x = NULL, y = NULL, fill = NULL, colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")
  })

  # ── Order status pie ─────────────────────────────────────────────────────────
  output$status_pie <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT order_status, COUNT(*) AS cnt
      FROM orders WHERE %s GROUP BY order_status", where_clause()))

    ggplot(df, aes(x = "", y = cnt, fill = order_status)) +
      geom_col(width = 1, colour = "white") +
      coord_polar("y") +
      scale_fill_brewer(palette = "Set2") +
      geom_text(aes(label = paste0(order_status, "\n", percent(cnt / sum(cnt), accuracy = 0.1))),
                position = position_stack(vjust = 0.5), size = 3.5) +
      labs(fill = NULL) +
      theme_void(base_size = 13) +
      theme(legend.position = "none")
  })

  # ── Delivery status bar ───────────────────────────────────────────────────────
  output$delivery_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT delivery_status, COUNT(*) AS cnt
      FROM orders WHERE %s GROUP BY delivery_status ORDER BY cnt DESC", where_clause()))

    ggplot(df, aes(x = reorder(delivery_status, cnt), y = cnt, fill = delivery_status)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = comma(cnt)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
      scale_fill_brewer(palette = "Blues", direction = -1) +
      labs(x = NULL, y = "Orders") +
      theme_minimal(base_size = 13)
  })

  # ── Payment status bar ────────────────────────────────────────────────────────
  output$payment_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT payment_status, COUNT(*) AS cnt
      FROM orders WHERE %s GROUP BY payment_status ORDER BY cnt DESC", where_clause()))

    ggplot(df, aes(x = reorder(payment_status, cnt), y = cnt, fill = payment_status)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = comma(cnt)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
      scale_fill_manual(values = c("Paid" = "#22c55e", "Pending" = "#f59e0b", "Failed" = "#ef4444")) +
      labs(x = NULL, y = "Orders") +
      theme_minimal(base_size = 13)
  })

  # ── Weekend vs weekday ────────────────────────────────────────────────────────
  output$weekend_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT is_weekend, COUNT(*) AS cnt,
             ROUND(SUM(total_price_usd)::numeric, 2) AS revenue
      FROM orders WHERE %s GROUP BY is_weekend", where_clause())) %>%
      mutate(label = ifelse(is_weekend == "Yes", "Weekend", "Weekday"))

    ggplot(df, aes(x = label, y = cnt, fill = label)) +
      geom_col(show.legend = FALSE, width = 0.5) +
      geom_text(aes(label = comma(cnt)), vjust = -0.5, size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
      scale_fill_manual(values = c("Weekend" = "#7c5cd8", "Weekday" = "#3b82d4")) +
      labs(x = NULL, y = "Orders") +
      theme_minimal(base_size = 13)
  })

  # ── Revenue by category ───────────────────────────────────────────────────────
  output$category_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT category,
        ROUND(SUM(total_price_usd)::numeric, 2) AS revenue,
        COUNT(*) AS orders
      FROM orders WHERE %s GROUP BY category ORDER BY revenue DESC", where_clause()))

    ggplot(df, aes(x = reorder(category, revenue), y = revenue, fill = category)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0("$", round(revenue / 1e6, 1), "M")), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
  })

  # ── Profit margin by category ─────────────────────────────────────────────────
  output$margin_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT category,
        ROUND(AVG(profit_margin_percent)::numeric, 1) AS avg_margin
      FROM orders WHERE %s GROUP BY category ORDER BY avg_margin DESC", where_clause()))

    ggplot(df, aes(x = reorder(category, avg_margin), y = avg_margin, fill = category)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(avg_margin, "%")), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = NULL, y = "Avg Profit Margin (%)") +
      theme_minimal(base_size = 13)
  })

  # ── Top brands ────────────────────────────────────────────────────────────────
  output$brand_bar <- renderPlot({
    df <- dbGetQuery(con, sprintf("
      SELECT brand,
        ROUND(SUM(total_price_usd)::numeric, 2) AS revenue,
        ROUND(SUM(profit_usd)::numeric, 2)      AS profit
      FROM orders WHERE %s GROUP BY brand ORDER BY revenue DESC LIMIT 15", where_clause()))

    df_long <- df %>%
      tidyr::pivot_longer(c(revenue, profit), names_to = "metric", values_to = "value")

    ggplot(df_long, aes(x = reorder(brand, ifelse(metric == "revenue", value, 0)),
                        y = value, fill = metric)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_fill_manual(values = c("revenue" = "#3b82d4", "profit" = "#22c55e"),
                        labels  = c("revenue" = "Revenue", "profit" = "Profit")) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
}

shinyApp(ui, server)
