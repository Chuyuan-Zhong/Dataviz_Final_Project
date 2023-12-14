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
data <- readxl::read_excel("investor_byvc.xlsx")
data$DEAL_DATE <- as.Date(data$DEAL_DATE, format = "%Y-%m-%d")
dealdata <- data %>% filter(!is.na(PORTFOLIO_COMPANY_REGION), !is.na(DEAL_DATE))
pdata <- data %>% filter(!is.na(PORTFOLIO_COMPANY_REGION))
idata <- data %>% filter(!is.na(INVESTOR_REGION))
mapdata <- data %>% filter(!is.na(`PORTFOLIO COMPANY COUNTRY`))

# Data clean for map
mapData <- world[c(2,11)]
countries <- mapdata %>% 
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
      menuItem("Companies Info", tabName = "companies_info", icon = icon("building"),
               menuSubItem("Top 10 Companies Information", tabName = "top10_companies")
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
        p("The dashboard visualizes data from Preqin's global venture capital deals database spanning 2000-2022. It includes over 252,986 deals, capturing funded startup names, investment sizes, dates, locations, backer details, and recipient company industries. This extensive dataset powers customized summaries and maps tracking international capital flows as they relate to development indicators and emerging high-potential sectors.", style = "font-size: 16px;"),
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
                                         choices = unique(dealdata$INDUSTRY_CLASSIFICATION),
                                         multiple = TRUE, selected = unique(dealdata$INDUSTRY_CLASSIFICATION)[1])
                           ),
                           mainPanel(
                            box(plotOutput("stacked", width = "100%", height = "600px"), 
                           title = "Your Box Title", status = "primary", solidHeader = TRUE)
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
                titlePanel("Treemap of investment stages by count or deal size from 2000-2023"),
                p("Leverage the date range selector to check specific time periods and see how investment priorities and the prominence of different stages have evolved over the years!", style = "font-size: 16px;"),
                p("Venture capital funds concentrate most prominently in the seed stages as well as Series C, with the largest sums funneled to slightly more mature startups raising Series C capital.", style = "font-size: 16px;"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("viewType", "View By:",
                                 choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "deal_size")),
                    dateRangeInput("dateRange", "Select Date Range:",
                                   start = min(data$DEAL_DATE, na.rm = TRUE),
                                   end = max(data$DEAL_DATE, na.rm = TRUE),
                                   min = min(data$DEAL_DATE, na.rm = TRUE),
                                   max = max(data$DEAL_DATE, na.rm = TRUE))
                  ),
                  mainPanel(
                    plotlyOutput("treemapstage")
                  )
                ))
      ),
      tabItem(tabName = "investment_industry", 
              ui <- fluidPage(
                titlePanel("Treemap of investment industry by count or deal size from 2000-2023"),
                p("Explore the dynamic landscape of investment across various industries with this treemap", style = "font-size: 16px;"),
                p("Information technology captures the largest share of venture capital at 40% of deals, indicative of tech's dominating and growing role driving growth, while the next largest sectors of consumer discretionary at 17% and healthcare with a rising recent profile also draw significant capital.", style = "font-size: 16px;"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("viewType", "View By:",
                                 choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "deal_size")),
                    selectInput("region", "Region:",
                                choices = c("All", na.omit(unique(data$PORTFOLIO_COMPANY_REGION)))),
                    dateRangeInput("dateRange", "Date Range:",
                                   start = min(data$DEAL_DATE, na.rm = TRUE),
                                   end = max(data$DEAL_DATE, na.rm = TRUE))
                  ),
                  mainPanel(
                    plotlyOutput("treemapind")
                  )
                )
              )
      ),
      tabItem(tabName = "investor_types", 
              ui <- fluidPage(
                titlePanel("Treemap of investor types"),
                p("This treemap offers a visualization of the distribution of investor types, highlighting trends in the venture capital landscape based on the count of deals or the total deal size in millions of dollars.", style = "font-size: 16px;"),
                p("Private equity firms, investment companies, and fund managers are the major VC investors.", style = "font-size: 16px;"),
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("viewType", "View By:",
                                 choices = list("Count of Deals" = "count", "Deal Size ($MM)" = "deal_size")),
                    selectInput("investorRegion", "Investor Region:",
                                choices = c("All", unique(data$INVESTOR_REGION[!is.na(data$INVESTOR_REGION)]))),
                    dateRangeInput("dateRange", "Date Range:",
                                   start = min(data$DEAL_DATE, na.rm = TRUE),
                                   end = max(data$DEAL_DATE, na.rm = TRUE))
                  ),
                  mainPanel(
                    plotlyOutput("treemap")
                  )
                )
              )
      ),
      tabItem(tabName = "deals", 
              ui <- fluidPage(
                titlePanel(title = "Money Flow in Different Industries and Regions Across Years"),
                p("Beginning in 2015, there was a rapid increase in both the number and size of deals, culminating in a peak in 2021.", style = "font-size: 16px;"), 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("measure", "Measure:", 
                                choices = c("Count", "Value"),
                                selected = "Count"),
                    selectInput("industry", "Industry:",
                                choices = unique(data$INDUSTRY_CLASSIFICATION),
                                multiple = TRUE,
                                selected = unique(data$INDUSTRY_CLASSIFICATION)[1])
                  ),
                  mainPanel(
                    plotlyOutput("barPlot")
                  )
                )
              )
      ),
      tabItem(tabName = "top100_companies", 
              ui <- dashboardPage(
                dashboardHeader(title = "Top Investors Info"),
                dashboardSidebar(
                  selectInput("industry", "Industry:", 
                              choices = c("All", unique(data$INDUSTRY_CLASSIFICATION))),
                  selectInput("investorRegion", "Investor Region:", 
                              choices = c("All", na.omit(unique(data$INVESTOR_REGION))))
                ),
                dashboardBody(
                  DTOutput("investorsTable")
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
    filtered_data <- dealdata %>%
      filter(INDUSTRY_CLASSIFICATION %in% input$industry) %>%
      mutate(Year = format(DEAL_DATE, "%Y")) %>%
      group_by(Year, PORTFOLIO_COMPANY_REGION)
    
    if (input$measure == "Count") {
      plot_data <- filtered_data %>%
        summarise(Count = n()) %>%
        ungroup()
      gg <- ggplot(plot_data, aes(x = Year, y = Count, fill = PORTFOLIO_COMPANY_REGION)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(y = "Count", fill = "Region", 
             title = paste("Annual investment count by region in industries"))
    } else {
      plot_data <- filtered_data %>%
        summarise(Value = sum(DEAL_SIZE, na.rm = TRUE)) %>%
        ungroup()
      gg <- ggplot(plot_data, aes(x = Year, y = Value, fill = PORTFOLIO_COMPANY_REGION)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(y = "Deal Size", fill = "Region", 
             title = paste("Annual investment value by region in industries")) +
        scale_y_continuous(labels = scales::comma)
    }
    gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    gg
  })
  
  ## Stage tree map
  output$treemapstage <- renderPlotly({
    sd <- pdata %>%
      filter(DEAL_DATE >= input$dateRange[1] & DEAL_DATE <= input$dateRange[2])
    sd1 <- if(input$viewType == "count") {
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
                        format(input$dateRange[1], "%Y"), 
                        "to", format(input$dateRange[2], "%Y"),
                        "by", ifelse(input$viewType == "count", "count", "value"))
    
    plot_ly(data = sd1,
            type = "treemap",
            values = ~value,
            labels = ~STAGE,
            parents = ~parent,
            textinfo = "label+value+percent entry") %>%
      layout(title = title_text)
  })

  
  ## TOP100 Table

  # Reactive expression 
  tc <- reactive({
    if (input$industry != "All" && input$investorRegion != "All") {
      pdata %>%
        filter(INDUSTRY_CLASSIFICATION == input$industry,
               INVESTOR_REGION == input$investorRegion)
    } else if (input$industry != "All") {
      pdata %>%
        filter(INDUSTRY_CLASSIFICATION == input$industry)
    } else if (input$investorRegion != "All") {
      pdata %>%
        filter(INVESTOR_REGION == input$investorRegion)
    } else {
      pdata
    }
  })
  
  output$investorsTable <- renderDT({
    tc() %>%
      arrange(desc(DEAL_SIZE)) %>%
      filter(!is.na(INVESTOR_REGION)) %>%
      slice(1:100) %>%
      select(
        `Investor/Firm` = `INVESTORS / BUYERS (FIRMS)`,
        `Investor Type` = `INVESTOR TYPE`,
        `Investor Country` = `INVESTOR COUNTRY`,
        `Investor Region` = `INVESTOR_REGION`,
        `Deal Date` = `DEAL_DATE`,
        `Deal Status` = `DEAL STATUS`,
        `Stage` = `STAGE`,
        `Portfolio Company` = `PORTFOLIO COMPANY`,
        `Deal Size` = `DEAL_SIZE`,
        `Portfolio Company Region` = `PORTFOLIO_COMPANY_REGION`,
        `Portfolio Company Country` = `PORTFOLIO COMPANY COUNTRY`,
        `Industry` = `INDUSTRY_CLASSIFICATION`
      ) %>%
      datatable(options = list(pageLength = 10))
  })
 
  ## Treemap for industry
  output$treemapind <- renderPlotly({
    
    ind <- pdata %>%
      filter(if(input$region != "All") PORTFOLIO_COMPANY_REGION == input$region else TRUE,
             DEAL_DATE >= input$dateRange[1] & DEAL_DATE <= input$dateRange[2])
    
    indt <- if(input$viewType == "count") {
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
                        format(input$dateRange[1], "%Y"), 
                        "to", format(input$dateRange[2], "%Y"),
                        "by", ifelse(input$viewType == "count", "count", "value"))
    plot_ly(data = indt,
            type = "treemap",
            values = ~value,
            labels = ~INDUSTRY_CLASSIFICATION,
            parents = ~parent,
            textinfo = "label+value+percent entry") %>%
      layout(title = title_text)
  })


## Deal plot
  output$barPlot <- renderPlot({
    
    plotData <- reactive({
      req(input$industry)  # Ensure that the industry input is not NULL
      
      dd <- dealdata %>%
        filter(INDUSTRY_CLASSIFICATION %in% input$industry) %>%
        mutate(YEAR = year(DEAL_DATE)) %>%
        group_by(YEAR, PORTFOLIO_COMPANY_REGION) %>%
        summarise(
          Count = n(),
          Value = sum(DEAL_SIZE, na.rm = TRUE),
          .groups = 'drop'
        )
      return(dd)
    })

    p <- ggplot(plotData(), aes(x = YEAR, y = ifelse(input$measure == "Count", Count, Value), fill = PORTFOLIO_COMPANY_REGION)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(title = paste("Deal size and count by region and year"),
           x = "Year", y = input$measure) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  })
  
## Treemap for Investor types
  output$treemap <- renderPlotly({
    
    InvestorsData <- idata %>%
      filter(if(input$investorRegion != "All") INVESTOR_REGION == input$investorRegion else TRUE,
             DEAL_DATE >= input$dateRange[1] & DEAL_DATE <= input$dateRange[2])
    
    id <- if(input$viewType == "count") {
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
                        format(input$dateRange[1], "%Y"), 
                        "to", format(input$dateRange[2], "%Y"),
                        "by", ifelse(input$viewType == "count", "count", "value"))
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
