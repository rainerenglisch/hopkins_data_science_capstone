library(shiny)
library(stringr)
library(data.table)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Next word prediction with kneyser nei probabilities"),
  
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

getNextWords =function(textInput) {
  print("getNextWords")
  print(textInput)
  text_split = str_split(str_trim(textInput), pattern="\\s+", simplify = TRUE)
  print(paste0("length of text_split ", toString(length(text_split))))
  print(text_split )
  result=list()
  
  if (length(text_split)>=3) {
    print("Using split >=3")
    preceding_gram = as.list(text_split[(length(text_split)-2):length(text_split)])
    print(preceding_gram)
    result = n_grams[[4]][preceding_gram,c("gram_4","p_kn"), on = c("gram_1","gram_2","gram_3")]
    
  }      
  if (length(text_split)==2  | ( length(text_split)>=3 & length(result)<3 ) ) {#| is.na(result$p_kn)) {
    print("Using split ==2")
    result = n_grams[[3]][as.list(text_split[(length(text_split)-1):length(text_split)]),c("gram_3","p_kn"), on = c("gram_1","gram_2")]
  }
  if (length(text_split)==1 | (length(text_split)>=2 & length(result)<3 )) {#| is.na(result$p_kn)) {
    print("Using split ==1")
    if (text_split[1] != "") {
      result = n_grams[[2]][as.list(text_split[length(text_split)]),c("gram_2","p_kn"), on = c("gram_1")]
    } else {
      print("Empty string using 1-gram")
      result = n_grams[[1]][,c("gram_1","p_kn")]
    }
    # for no word entered, need BOS 2-gram
    # if no word found use unigram?
  }
  result=result[order(result$p_kn, decreasing = TRUE),]  
  result=result[1:3, ]
  #setnames(result, 1, gram)
  colnames(result)[1] = "gram"
  #result$gram <- as.character(result$gram)
  result[,gram := as.character(gram)]
  return (result)
  #print(lapply(result[1:3,1:2], as.character))
}

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