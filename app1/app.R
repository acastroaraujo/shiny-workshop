
library(shiny)
library(purrr)
library(ggplot2)
library(dplyr)
library(bslib)
library(thematic)

data_list <- ls("package:datasets")
ok <- map(data_list, get, pos = "package:datasets") |> ## load all datasets
  map_lgl(is.data.frame)                         ## check that they are data frames

ui <- fluidPage(
 theme = bslib::bs_theme(
   version = "3",
   base_font = bslib::font_google("Crimson Pro"),
   heading_font = bslib::font_google("Lobster"),
   fg = "#386890", primary = "#E05044", bg = "#FBF7F4"
 ),
  
  h1("Hello world!"),
  p("This app plots pre-loaded R datasets in ggplot2."),
  p("another line, edit this"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose:", choice = data_list[ok]),
      uiOutput("vars")
    ),
    mainPanel(
      plotOutput("gplot")
    )
  )
  
)

server <- function(input, output, session) {
  
  ggplot2::theme_set(ggplot2::theme_light()) ## ensures all plot aesthetics 
  thematic::thematic_shiny()            

# data stuff --------------------------------------------------------------

  df <- reactive({
    get(input$dataset, "package:datasets")
  })
  
# dynamic ui stuff --------------------------------------------------------

  output$vars <- renderUI({
    
    col_names <- colnames(df())
    
    tagList(
      selectInput("var1", "X", choices = col_names),
      selectInput("var2", "Y", choices = col_names)
    )
    
  })

# plot stuff --------------------------------------------------------------
  
  output$gplot <- renderPlot({
    
    x <- sym(input$var1)
    y <- sym(input$var2)
    
    df() |> 
      ggplot(aes({{x}}, {{y}})) + 
      geom_point() +
      ggtitle(input$dataset)
  }
    
  )
  
  
  ## debugger -----
  
   observe({
     message(str(input$dataset))
     message(str(df()))
     message(str(input$var1))
     message(str(input$var2))
   })
  
}

shinyApp(ui, server)


