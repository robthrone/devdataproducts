library(shiny)
library(gapminder)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("Countries and their GDP over time"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("GDP Plot", 
                 selectInput("country", "Select Country:", choices = unique(gapminder$country), multiple = TRUE),
                 sliderInput("year_range", "Select Year Range:", min = 1952, max = 2007, value = c(1952, 2007), step = 5)
        ),
        tabPanel("GDP Growth Analysis",
                 selectInput("country_growth", "Select Country:", choices = unique(gapminder$country)),
                 sliderInput("year_range_growth", "Select Year Range:", min = 1952, max = 2007, value = c(1952, 2007), step = 5),
                 actionButton("calculate_growth", "Calculate Growth Rate"),
                 textOutput("avg_growth"),
                 tableOutput("top_countries")
        )
      )
    ),
    mainPanel(
      uiOutput("tab_content")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$tab_content <- renderUI({
    if (input$tabs == "GDP Plot") {
      plotOutput("gdp_plot")
    } else if (input$tabs == "GDP Growth Analysis") {
      fluidRow(
        column(6, textInput("start_year", "Start Year:", value = "1952")),
        column(6, textInput("end_year", "End Year:", value = "2007"))
      )
    }
  })
  
  output$gdp_plot <- renderPlot({
    filtered_data <- gapminder %>%
      filter(country %in% input$country & year >= input$year_range[1] & year <= input$year_range[2])
    
    ggplot(filtered_data, aes(x = year, y = gdpPercap, color = country)) +
      geom_line() +
      labs(x = "Year", y = "GDP per Capita", color = "Country") +
      theme_minimal()
  })
  
  observeEvent(input$calculate_growth, {
    start_year <- as.numeric(input$start_year)
    end_year <- as.numeric(input$end_year)
    
    country_data <- gapminder %>%
      filter(country == input$country_growth & year >= start_year & year <= end_year) %>%
      arrange(year)
    
    if (nrow(country_data) > 1) {
      growth_rate <- (last(country_data$gdpPercap) - first(country_data$gdpPercap)) / (end_year - start_year)
      output$avg_growth <- renderText({
        paste("Average GDP Growth Rate:", round(growth_rate, 2))
      })
    } else {
      output$avg_growth <- renderText({
        "Insufficient data for calculation"
      })
    }
  })
  
  output$top_countries <- renderTable({
    growth_rates <- gapminder %>%
      filter(year >= input$year_range_growth[1] & year <= input$year_range_growth[2]) %>%
      group_by(country) %>%
      summarize(growth_rate = (last(gdpPercap) - first(gdpPercap)) / (input$year_range_growth[2] - input$year_range_growth[1])) %>%
      arrange(desc(growth_rate)) %>%
      top_n(5)
    
    growth_rates
  })
}

# Run the app
shinyApp(ui = ui, server = server)

