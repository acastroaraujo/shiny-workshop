
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(googlesheets4) #*
library(bslib) 


# Data --------------------------------------------------------------------

# data <- read.csv("css_syllabus.csv")

gs4_deauth()
options( ## see: https://gargle.r-lib.org/articles/non-interactive-auth.html#project-level-oauth-cache
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
## replace this with org spreadsheet
data <- googlesheets4::range_read("https://docs.google.com/spreadsheets/d/19Yohd8eabvvg59jmwtn4vbcOmmAiv3LfLMg1nl4kf_A/edit?usp=sharing")

data$date_formatted <- as.Date(ISOdate(data$Publication.Year, 1, 1))

data <- data |> 
  drop_na(date_formatted) |> 
  arrange(date_formatted)

#Unique author last names
authors <- strsplit(data$Author, split = ";")
authors <- data.frame(Key = rep(data$Key, sapply(authors, length)), author = unlist(authors))
authors <- as_tibble(authors)
authors$author <- str_trim(authors$author)
authors$author_last <- gsub("(.*),.*", "\\1", authors$author)

#Unique pubs
pubs <- sort(unique(data$Publication.Title))

#Columns
vchoices <- 1:ncol(data)
names(vchoices) <- str_replace(names(data), "\\.", " ") 
vchoices <- vchoices[c(3, 4, 5, 6, 11)]

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = bslib::bs_theme( 
    version = "3",
    base_font = bslib::font_google("Arsenal"),
    heading_font = bslib::font_google("Rubik Iso"),
    fg = "#386890", primary = "#386890", bg = "#FBF7F4"
  ),
  
  titlePanel("VaryCSS Explorer", windowTitle = "VCSS Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("author", "Author", authors$author_last, selected = NULL, multiple = TRUE),
      selectizeInput("pubs", "Journal", pubs, selected = NULL, multiple = TRUE),
      # numericInput("thread_ids", "Thread Id:", 0, min = min(data$thread_id), max = max(data$thread_id)),
      textInput('title', "Title Keywords", value = ""),
      dateRangeInput("date", "Date", start = min(data$date_formatted), end = max(data$date_formatted)),
      checkboxGroupInput("columns", "Select Columns", choices = vchoices, selected = c(3, 4, 5, 6), inline = TRUE)
    ),
    mainPanel(
      DTOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  
  data_table <- reactive({
    
    cols <- names(data)[as.numeric(input$columns)]
    
    table <- data |> 
      filter(date_formatted >= input$date[1] & date_formatted <= input$date[2])
    
    table <- if (is.null(input$author)) table else table |> filter(grepl(paste(input$author, collapse = "|"), Author)) ## added paste regex collapse
    table <- if (is.null(input$pubs)) table else table |> filter(Publication.Title == input$pubs)
    table <- if (input$title == "") table else table |> filter(grepl(paste(input$title, collapse = "|", Title)))
    table <- if (is.null(input$columns)) table else table |> select(any_of(cols))
    
    return(table)
    
  })
  
  output$table <- DT::renderDT({
    
    DT::datatable(
      data = data_table(),
      extensions = 'Buttons',
      options = list(
        buttons = c("copy", "excel", "csv"),
        dom = "Brtip"
      ),
      filter = "top"
    )
    
  })
}

shinyApp(ui = ui, server = server)
