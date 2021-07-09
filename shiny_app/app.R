library(shiny)
library(stringr)
library(data.table)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "textInput", label = "Text"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput(outputId = "textIndex"),
      textOutput(outputId = "textOutput"),

      
    )
  )
)

source("./use_n_grams_probs.R")

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$textIndex = renderText( {
    text_split = str_split(str_trim(input$textInput), pattern=" ", simplify = TRUE)
    toString(text_split[(length(text_split)-2):length(text_split)])})
  
  output$textOutput = renderText({
    text_split = str_split(str_trim(input$textInput), pattern=" ", simplify = TRUE)
  
    result = n_grams[[4]][as.list(text_split[(length(text_split)-2):length(text_split)]),c("gram_4","p_kn"), on = c("gram_1","gram_2","gram_3")]
    if (is.na(result$p_kn)) {
      result = n_grams[[3]][as.list(text_split[(length(text_split)-1):length(text_split)]),c("gram_3","p_kn"), on = c("gram_1","gram_2")]
      if (is.na(result$p_kn)) {
        result = n_grams[[2]][as.list(text_split[length(text_split)]),c("gram_2","p_kn"), on = c("gram_1")]
        # for no word entered, need BOS 2-gram
        # if no word found use unigram?
      }
    }
    print(as.character(result[1,1][[1]]))
    as.character(result[1,1][[1]])
    })
  
}

shinyApp(ui = ui, server = server)