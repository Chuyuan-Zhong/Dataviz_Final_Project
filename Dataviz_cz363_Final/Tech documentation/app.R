library(lubridate)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(shinydashboard)
library(scales)
library(plotly)
library(ggplot2)
library(readr)
library(DT)
library(spData)
library(sf)
library(leaflet)

# Dataset and cleaning
data <- readxl::read_excel("filtered_data.xlsx")
data$DEAL_DATE <- as.Date(data$DEAL_DATE, format = "%Y-%m-%d")

mapData <- world[c(2,11)]
countries <- data %>% 
  group_by(`PORTFOLIO COMPANY COUNTRY`) %>%
  summarise(TotalDealSize = sum(DEAL_SIZE, na.rm = TRUE),
            DealCount = n(),
            .groups = 'drop')
countries <- left_join(countries, mapData, by = c("PORTFOLIO COMPANY COUNTRY" = "name_long"))

# Set up UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Venture Capital Investment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Investment", tabName = "investment", icon = icon("dashboard"),
               menuSubItem("Investment Distribution", tabName = "investment_distribution"),
               menuSubItem("Investment Stage", tabName = "investment_stage"),
               menuSubItem("Investment Industry", tabName = "investment_industry")
      ),
      menuItem("Investors", tabName = "investors", icon = icon("users"),
               menuSubItem("Investor Types", tabName = "investor_types")
      ),
      menuItem("Deals", tabName = "deals", icon = icon("handshake")),
      menuItem("Top 10 Companies Info", tabName = "top_10_companies_info", icon = icon("building")
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Welcome to the Global Venture Capital Investment Dashboard!",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        p("This is an interactive data visualization tool for exploring cross-border startup and venture capital investment trends. This dashboard was developed to provide a window into innovation-driven economic growth and development worldwide based on granular tracking of industry-specific investment flows between countries.", style = "font-size: 16px;"),
        p("The dashboard visualizes data from Preqin's global venture capital deals database spanning 2016-2023. It includes over 252,986 deals, capturing funded startup names, investment sizes, dates, locations, backer details, and recipient company industries. This extensive dataset powers customized summaries and maps tracking international capital flows as they relate to development indicators and emerging high-potential sectors.", style = "font-size: 16px;"),
        h4("Key insights the dashboard enables users to explore include:"),
        tags$ul(
          tags$li("Surging venture investment levels in developing Asian economies like China and India."),
          tags$li("Information technology's position as the dominant and fastest-growing target industry."),
          tags$li("The rising prominence of cross-border South-South investment corridors."),
          tags$li("Investors and targets are concentrated in Asia.")
        )
      )
    ),
    tabItems(
      tabItem(tabName = "investment_distribution",
              tabPanel("Investment Trends",
                       fluidPage(
                         titlePanel("Investment trends by year and region"),
                         p("This section provides insights into investment trends over time, segmented by year and region. Use the selectors to customize the view based on different measures and industries.", style = "font-size: 16px;"),
                         p("Investments are concentrated in Asia, especially China.", style = "font-size: 16px;"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("measure", "Measure:", choices = c("Count", "Value")),
                             selectInput("industry", "Industry:",
                                         choices = unique(data$INDUSTRY_CLASSIFICATION),
                                         multiple = TRUE, selected = unique(data$INDUSTRY_CLASSIFICATION)[1])
                           ),
                           mainPanel(
                             box(width = 16, plotOutput("stacked", height = "400px"), 
                                 title = "Annual investment count by region in industries", status = "primary", solidHeader = TRUE)
                           )
                         )
                       )
              ),
              tabPanel("Global Investment Landscape",
                       fluidPage(
                         titlePanel("Global investment landscape: tracking deals and sizes by country"),
                         p("Explore the global investment landscape with this interactive map. You can view the data by either the count of deals or the size of the deals. Use the radio buttons to switch between different views.", style = "font-size: 16px;"),
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons("viewType", "View By:",
                                          choices = list("Count of Deals" = "count", "Deal Size" = "deal_size"))
                           ),
                           mainPanel(
                             leafletOutput("investmentMap")
                           )
                         )
                       )
              )
      ),
      tabItem(tabName = "investment_stage", 
              ui <- fluidPage(
                titlePanel("Treemap of investment stages by count or deal size from 2016-2023"),
                p("Leverage the date range selector to check specific time periods and see how investment priorities and the prominence of different stages have evolved over the years!", style = "font-size: 16px;"),
                p("Venture capital funds concentrate most prominently in the seed stages as well as Series C, with the largest sums funneled to slightly more mature startups raising Series C capital.", style = "font-size: 16px;"),
                fluidRow(
                  
                  sidebarPanel(
                    radioButtons("viewType2", "View By:",
                                 choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "value")),
                    dateRangeInput("dateRange2", "Date Range:",
                                   start = min(data$DEAL_DATE, na.rm = TRUE),
                                   end = max(data$DEAL_DATE, na.rm = TRUE),
                                   min = min(data$DEAL_DATE, na.rm = TRUE),
                                   max = max(data$DEAL_DATE, na.rm = TRUE))
                    
                  ),
                  column(12,
                         box(
                           plotlyOutput("treemapstage"),
                           title = "Investment stage treemap",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           div(class = "custom-box-height")
                         )
                  )
                ))
      ),
      tabItem(tabName = "investment_industry", 
              ui <- fluidPage(
                titlePanel("Treemap of investment industry by count or deal size from 2016-2023"),
                p("Explore the dynamic landscape of investment across various industries with this treemap", style = "font-size: 16px;"),
                p("Information technology captures the largest share of venture capital at 40% of deals, indicative of tech's dominating and growing role driving growth, while the next largest sectors of consumer discretionary at 17% and healthcare with a rising recent profile also draw significant capital.", style = "font-size: 16px;"),
                fluidRow(
                  column(12,
                         sidebarPanel(
                           radioButtons("viewType3", "View By:",
                                        choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "value")),
                           selectInput("region3", "Region:",
                                       choices = c("All", na.omit(unique(data$PORTFOLIO_COMPANY_REGION)))),
                           dateRangeInput("dateRange3", "Date Range:",
                                          start = min(data$DEAL_DATE, na.rm = TRUE),
                                          end = max(data$DEAL_DATE, na.rm = TRUE))
                         )
                  ),
                  column(12,
                         box(
                           plotlyOutput("treemapind"),
                           title = "Investment industry treemap",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           div(class = "custom-box-height")
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "investor_types", 
              fluidPage(
                titlePanel("Treemap of investor types"),
                p("This treemap offers a visualization of the distribution of investor types, highlighting trends in the venture capital landscape based on the count of deals or the total deal size in millions of dollars.", style = "font-size: 16px;"),
                p("Private equity firms, investment companies, and fund managers are the major VC investors.", style = "font-size: 16px;"),
                fluidRow(
                  column(12,
                         sidebarPanel(
                           radioButtons("viewType4", "View By:",
                                        choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "value")),
                           selectInput("investorRegion4", "Investor Region:",
                                       choices = c("All", unique(data$INVESTOR_REGION[!is.na(data$INVESTOR_REGION)]))),
                           dateRangeInput("dateRange4", "Date Range:",
                                          start = min(data$DEAL_DATE, na.rm = TRUE),
                                          end = max(data$DEAL_DATE, na.rm = TRUE))
                         )
                  ),
                  column(12,
                         box(
                           plotlyOutput("treemap", height = "400px"),
                           title = "Investor type treemap",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "deals", 
              fluidPage(
                titlePanel(title = "Money Flow in Different Industries and Regions Across Years"),
                p("Beginning in 2019, there was a rapid increase in both the number and size of deals, culminating in a peak in 2021.", style = "font-size: 16px;"), 
                fluidRow(
                  column(12,
                         sidebarPanel(
                           
                           selectInput("industry2", "Industry:",
                                       choices = unique(data$INDUSTRY_CLASSIFICATION),
                                       multiple = TRUE,
                                       selected = unique(data$INDUSTRY_CLASSIFICATION)[1]),
                           selectInput("region2", "Region:", 
                                       choices = unique(data$PORTFOLIO_COMPANY_REGION),
                                       multiple = TRUE,
                                       selected = unique(data$PORTFOLIO_COMPANY_REGION)[2])
                         )
                  ),
                  column(12,
                         box(
                           plotlyOutput("barPlot", height = "400px"),
                           title = "Deal plot",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "top_10_companies_info",
              fluidPage(
                titlePanel("Top 10 Companies Info"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("industry3", "Industry:", 
                                choices = c("All", unique(data$INDUSTRY_CLASSIFICATION))),
                    selectInput("investorRegion3", "Investor Region:", 
                                choices = c("All", na.omit(unique(data$INVESTOR_REGION))))
                  ),
                  mainPanel(
                    DTOutput("investorsTable")
                  )
                )
              )
      )
      
    )
  )
)

## Server
server <- function(input, output) {
  
  ## Map
  output$investmentMap <- renderLeaflet({
    
    valueField <- if(input$viewType == "count") {
      "DealCount"
    } else {
      "TotalDealSize"
    }
    
    
    pal <- colorNumeric(palette = "YlOrRd", domain = countries[[valueField]])
    
    map_labels <- paste(countries$`PORTFOLIO COMPANY COUNTRY`, 
                        "has", if(input$viewType == "count") "a total of" else "an average deal size of $",
                        countries[[valueField]])
    
    map <- leaflet(countries) %>%
      addTiles() %>% 
      setView(0, 0, 1)
    
    map %>% addPolygons(data = countries$geom,
                        fillColor = pal(countries[[valueField]]),
                        fillOpacity = .7,
                        color = "grey",
                        weight = 1,
                        label = map_labels,
                        labelOptions = labelOptions(textsize = "12px")) %>% 
      
      addLegend(pal = pal, 
                values = countries[[valueField]],
                position = "bottomleft")
  })
  
  ## Stacked bar plot
  output$stacked <- renderPlot({
    filtered_data <- data %>%
      filter(INDUSTRY_CLASSIFICATION %in% input$industry) %>%
      group_by(Year, PORTFOLIO_COMPANY_REGION)
    
    if (input$measure == "Count") {
      plot_data <- filtered_data %>%
        summarise(Count = n()) %>%
        ungroup()
      gg <- ggplot(plot_data, aes(x = Year, y = Count, fill = PORTFOLIO_COMPANY_REGION)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(y = "Count", fill = "Region")
    } else {
      plot_data <- filtered_data %>%
        summarise(Value = sum(DEAL_SIZE, na.rm = TRUE)) %>%
        ungroup()
      gg <- ggplot(plot_data, aes(x = Year, y = Value, fill = PORTFOLIO_COMPANY_REGION)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(y = "Deal Size", fill = "Region") +
        scale_y_continuous(labels = scales::comma)
    }
    gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    gg
  })
  
  ## Stage tree map
  output$treemapstage <- renderPlotly({
    sd <- data %>%
      filter(DEAL_DATE >= input$dateRange2[1] & DEAL_DATE <= input$dateRange2[2])
    sd1 <- if(input$viewType2 == "count") {
      sd %>%
        count(STAGE) %>%
        rename(value = n)
    } else {
      sd %>%
        group_by(STAGE) %>%
        summarise(value = sum(DEAL_SIZE, na.rm = TRUE)) %>%
        ungroup()
    }
    
    sd1$parent <- "All Stages"
    title_text <- paste("Investment stage distribution from", 
                        format(input$dateRange2[1], "%Y"), 
                        "to", format(input$dateRange2[2], "%Y"),
                        "by", ifelse(input$viewType2 == "count", "count", "value"))
    plot_ly(data = sd1,
            type = "treemap",
            values = ~value,
            labels = ~STAGE,
            parents = ~parent,
            textinfo = "label+value+percent entry") %>%
      layout(title = title_text)
  })
  
  
  ## TOP10 Table
  
  # Reactive expression 
  tc <- reactive({
    if (input$industry3 != "All" && input$investorRegion3 != "All") {
      data %>%
        filter(INDUSTRY_CLASSIFICATION == input$industry3,
               INVESTOR_REGION == input$investorRegion3)
    } else if (input$industry3 != "All") {
      data %>%
        filter(INDUSTRY_CLASSIFICATION == input$industry3)
    } else if (input$investorRegion3 != "All") {
      data %>%
        filter(INVESTOR_REGION == input$investorRegion3)
    } else {
      data
    }
  })
  
  output$investorsTable <- renderDT({
    tc() %>%
      arrange(desc(DEAL_SIZE)) %>%
      filter(!is.na(INVESTOR_REGION)) %>%
      slice(1:10) %>%
      select(
        `Investor/Firm` = `INVESTORS / BUYERS (FIRMS)`,
        `Investor Type` = `INVESTOR TYPE`,
        `Investor Country` = `INVESTOR COUNTRY`,
        `Investor Region` = `INVESTOR_REGION`,
        `Deal Date` = `DEAL_DATE`,
        `Stage` = `STAGE`,
        `Portfolio Company` = `PORTFOLIO COMPANY`,
        `Deal Size` = `DEAL_SIZE`,
        `Portfolio Company Region` = `PORTFOLIO_COMPANY_REGION`,
        `Portfolio Company Country` = `PORTFOLIO COMPANY COUNTRY`,
        `Industry` = `INDUSTRY_CLASSIFICATION`
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  ## Treemap for industry
  output$treemapind <- renderPlotly({
    
    ind <- data %>%
      filter(if(input$region3 != "All") PORTFOLIO_COMPANY_REGION == input$region3 else TRUE,
             DEAL_DATE >= input$dateRange3[1] & DEAL_DATE <= input$dateRange3[2])
    
    indt <- if(input$viewType3 == "count") {
      ind %>%
        count(INDUSTRY_CLASSIFICATION) %>%
        rename(value = n)
    } else {
      ind %>%
        group_by(INDUSTRY_CLASSIFICATION) %>%
        summarise(value = sum(DEAL_SIZE, na.rm = TRUE)) %>%
        ungroup()
    }
    indt$parent <- "All Industries"
    title_text <- paste("Investment industry distribution from", 
                        format(input$dateRange3[1], "%Y"), 
                        "to", format(input$dateRange3[2], "%Y"),
                        "by", ifelse(input$viewType3 == "count", "count", "value"))
    plot_ly(data = indt,
            type = "treemap",
            values = ~value,
            labels = ~INDUSTRY_CLASSIFICATION,
            parents = ~parent,
            textinfo = "label+value+percent entry") %>%
      layout(title = title_text)
  })

  
  dealp <- reactive({
    req(input$region2)  
    data %>%
      filter(if (length(input$industry2) > 0) INDUSTRY_CLASSIFICATION %in% input$industry2 else TRUE,
             PORTFOLIO_COMPANY_REGION %in% input$region2) %>%
      group_by(Year, PORTFOLIO_COMPANY_REGION) %>%
      summarise(TotalDealSize = sum(DEAL_SIZE, na.rm = TRUE),
                DealCount = n()) %>%
      arrange(Year, PORTFOLIO_COMPANY_REGION)
  })
  
  ## Deal plot
  output$barPlot <- renderPlotly({

    p <- ggplot(dealp(), aes(x = Year, y = TotalDealSize, group = PORTFOLIO_COMPANY_REGION, color = PORTFOLIO_COMPANY_REGION)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(
        text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()
      ) +
      labs(title = "Deal size and count by region and year",
           x = "Year", y = "Total Deal Size", color = 'Region')
    
    ggplotly(p)

  })
  
  ## Treemap for Investor types
  output$treemap <- renderPlotly({
    
    InvestorsData <- data %>%
      filter(if(input$investorRegion4 != "All") INVESTOR_REGION == input$investorRegion4 else TRUE,
             DEAL_DATE >= input$dateRange4[1] & DEAL_DATE <= input$dateRange4[2])
    
    id <- if(input$viewType4 == "count") {
      InvestorsData %>%
        count(`INVESTOR TYPE`) %>%
        rename(value = n)
    } else {
      InvestorsData %>%
        group_by(`INVESTOR TYPE`) %>%
        summarise(value = sum(DEAL_SIZE, na.rm = TRUE)) %>%
        ungroup()
    }
    
    id$parent <- "All Investor Types"
    title_text <- paste("Investor type distribution from", 
                        format(input$dateRange4[1], "%Y"), 
                        "to", format(input$dateRange4[2], "%Y"),
                        "by", ifelse(input$viewType4 == "count", "count", "value"))
    plot_ly(data = id,
            type = "treemap",
            values = ~value,
            labels = ~`INVESTOR TYPE`,
            parents = ~parent,
            textinfo = "label+value+percent entry") %>%
      layout(title = title_text)
  })
}

shinyApp(ui, server)
