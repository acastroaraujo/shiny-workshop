

# Set up ------------------------------------------------------------------

library(shiny)
library(purrr)
library(ggplot2)
library(dplyr)
library(bslib)
library(thematic)

# Helpers -----------------------------------------------------------------

getHTMLhelp <- function(...){
  capture.output(
    tools:::Rd2HTML(utils:::.getHelpFile(help(...)))
  )
}

# Datasets ----------------------------------------------------------------

data_list <- ls("package:datasets")
ok <- map(data_list, get, pos = "package:datasets") |> ## load all datasets
  map_lgl(is.data.frame)                               ## check that they are data frames

ui <- fluidPage(
  
 theme = bslib::bs_theme(
   version = "3",
   base_font = bslib::font_google("Arsenal"),
   heading_font = bslib::font_google("Lobster Two"),
   fg = "#386890", primary = "#E05044", bg = "#FBF7F4"
 ),
  
 h1("Scatterplots"),
 p("This app creates scatterplots of built-in R datasets in ggplot2."),
 br(),
  
 fluidRow(
   column(
     width = 2, offset = 1,
     selectInput("dataset", "Choose:", choices = data_list[ok]),
     uiOutput("vars")
    ),
   
   column(
     width = 8,
     plotOutput("gplot")
    )
 ),
 
 br(), br(), br(),
 fluidRow(
   wellPanel(htmlOutput("help_file"))
 )
  
)

server <- function(input, output, session) {
  
  ggplot2::theme_set(ggplot2::theme_light()) ## ensures all plot aesthetics 
  thematic::thematic_shiny()            

# data stuff --------------------------------------------------------------

  df <- reactive({
    req(input$dataset)
    get(input$dataset, "package:datasets")
  })
  
# dynamic ui stuff --------------------------------------------------------

  output$vars <- renderUI({
    
    req(input$dataset)
    req(df())
    
    col_names <- colnames(df())
    
    tagList(
      selectInput("var1", "X", choices = col_names, selected = col_names[[1]]),
      selectInput("var2", "Y", choices = col_names, selected = col_names[[2]])
    )
    
  })

# plot stuff --------------------------------------------------------------
  
  output$gplot <- renderPlot({
    
    req(input$var1)
    req(input$var2)

    x <- sym(input$var1)
    y <- sym(input$var2)
    
    if (!input$var1 %in% colnames(df())) {
     validate("loading data...") 
    }
    
    message(x)
    message(str(df()))
    
    df() |> 
      ggplot(aes({{x}}, {{y}})) + 
      geom_point() +
      geom_smooth(se = FALSE, linetype = "dashed") +
      ggtitle(input$dataset)
    
  })
  
  output$help_file <- renderUI(
    HTML(getHTMLhelp(input$dataset))
  )
}

shinyApp(ui, server)


