
library(shiny)
library(dplyr)
library(tibble)
library(bslib)
library(WDI)
library(highcharter)

source("helpers.R")

# data --------------------------------------------------------------------

d <- readRDS("out.rds")

labels <- WDI_data$series |> 
  filter(indicator %in% colnames(d))

indicator_description <- labels |> 
  select(indicator, description) |> 
  tibble::deframe()

# choice values -----------------------------------------------------------

indicator_choices <- labels |> 
  select(name, indicator) |> 
  distinct() |> 
  tibble::deframe()

country_choices <- d |> 
  select(country, iso3c) |> 
  distinct() |> 
  tibble::deframe()


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = bslib::bs_theme(
    version = "3",
    base_font = bslib::font_google("Arsenal"),
    heading_font = bslib::font_google("Lobster Two"),
    fg = "#386890", primary = "#E05044", bg = "#FBF7F4"
  ),
  
  titlePanel("MDG-relevant indicators", windowTitle = "MDG"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Indicator", choices = indicator_choices, selected = sample(indicator_choices, 1)),
      uiOutput("description"),
      selectInput("country_iso3", "Countries", choices = country_choices, selected = c("NGA"), multiple = TRUE)
    ),
    mainPanel(
      highchartOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$description <- renderUI(
    tagList(helpText(indicator_description[[input$indicator]]), br())
  )
  
  output$plot <- renderHighchart({
    custom_plot(y_var = input$indicator, country_list = input$country_iso3, data = d)
  })
  
}

shinyApp(ui, server)
