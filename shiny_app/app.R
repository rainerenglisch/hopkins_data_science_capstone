library(shiny)
library(stringr)
library(data.table)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Next word prediction with Kneser–Ney smoothing"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput(inputId = "textInput", label = "Text"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      actionButton("nextWord1", "NextWord1"),
      actionButton("nextWord2", "NextWord2"),
      actionButton("nextWord3", "NextWord3"),
    )
  )
)

source("./use_n_grams_probs.R")


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  #cur_next_words = c("","","")
  cur_next_words = reactive({
      my_cur_next_words = getNextWords( tolower(input$textInput))
      print("Next word predictions:")
      print(my_cur_next_words[1:3])
      
      updateActionButton(session, inputId = "nextWord1", label = toString(my_cur_next_words[1,1]))
      updateActionButton(session, inputId = "nextWord2", label = toString(my_cur_next_words[2,1]))
      updateActionButton(session, inputId = "nextWord3", label = toString(my_cur_next_words[3,1]))
      my_cur_next_words
    })
  
  observe({
    updateActionButton(session, inputId = "nextWord1", label = toString(cur_next_words()[1,1]))
    updateActionButton(session, inputId = "nextWord2", label = toString(cur_next_words()[2,1]))
    updateActionButton(session, inputId = "nextWord3", label = toString(cur_next_words()[3,1]))
    
  })

  observeEvent(input$nextWord1, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[1,1])))
  })
  observeEvent(input$nextWord2, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[2,1])))
  })
  observeEvent(input$nextWord3, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[3,1])))
  })
  
}

shinyApp(ui = ui, server = server)