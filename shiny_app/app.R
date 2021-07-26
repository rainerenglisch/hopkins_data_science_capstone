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
      actionButton("nextWord1", "NextWord1"),
      actionButton("nextWord2", "NextWord2"),
      actionButton("nextWord3", "NextWord3"),
      textOutput(outputId = "textIndex"),
      textOutput(outputId = "textOutput"),
      textOutput(outputId = "textNextWord1"),
      textOutput(outputId = "textNextWord2"),
      textOutput(outputId = "textNextWord3"),

    )
  )
)

source("./use_n_grams_probs.R")

getNextWords =function(textInput) {
  print("getNextWords")
  print(textInput)
  text_split = str_split(str_trim(textInput), pattern=" ", simplify = TRUE)
  print(paste0("length of text_split ", toString(length(text_split))))
  print(text_split )
  result=NA
  
  if (length(text_split)>=3) {
    print("Using split >=3")
    preceding_gram = as.list(text_split[(length(text_split)-2):length(text_split)])
    print(preceding_gram)
    result = n_grams[[4]][preceding_gram,c("gram_4","p_kn"), on = c("gram_1","gram_2","gram_3")]
    
  }      
  if (length(text_split)==2  | is.na(result)) {#| is.na(result$p_kn)) {
    print("Using split ==2")
    result = n_grams[[3]][as.list(text_split[(length(text_split)-1):length(text_split)]),c("gram_3","p_kn"), on = c("gram_1","gram_2")]
  }
  if (length(text_split)==1 | is.na(result)) {#| is.na(result$p_kn)) {
    print("Using split ==1")
    result = n_grams[[2]][as.list(text_split[length(text_split)]),c("gram_2","p_kn"), on = c("gram_1")]
    # for no word entered, need BOS 2-gram
    # if no word found use unigram?
  }
  result=result[order(result$p_kn, decreasing = TRUE),]  
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
      my_cur_next_words = getNextWords( input$textInput)
      print("Next word predictions:")
      print(my_cur_next_words[1:3])
      
      updateActionButton(session, inputId = "nextWord1", label = toString(my_cur_next_words[1,1]))
      updateActionButton(session, inputId = "nextWord2", label = toString(my_cur_next_words[2,1]))
      updateActionButton(session, inputId = "nextWord3", label = toString(my_cur_next_words[3,1]))
      my_cur_next_words
    })
  output$textNextWord1 = renderText({as.character(cur_next_words()[1,1])})
  output$textNextWord2 = renderText({as.character(cur_next_words()[2,1])})
  output$textNextWord3 = renderText({as.character(cur_next_words()[3,1])})
  
  
  observeEvent(input$nextWord1, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[1,1])))
  })
  observeEvent(input$nextWord2, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[2,1])))
  })
  observeEvent(input$nextWord3, {
    updateTextInput(session,inputId = "textInput", value = paste0(input$textInput," ",toString(cur_next_words()[3,1])))
  })
  
  output$textIndex = renderText( {
    text_split = str_split(str_trim(input$textInput), pattern=" ", simplify = TRUE)
    toString(text_split[(length(text_split)-2):length(text_split)])})
  # 
  # output$textOutput = renderText({
  #   text_split = str_split(str_trim(input$textInput), pattern=" ", simplify = TRUE)
  #   print(text_split )
  #   result=NA
  # 
  #   if (length(text_split)>=3) {
  #     print("Using split >=3")
  #     preceding_gram = as.list(text_split[(length(text_split)-2):length(text_split)])
  #     print(preceding_gram)
  #     result = n_grams[[4]][preceding_gram,c("gram_4","p_kn"), on = c("gram_1","gram_2","gram_3")]
  #   }      
  #   if (length(text_split)==2  | is.na(result$p_kn)) {
  #     result = n_grams[[3]][as.list(text_split[(length(text_split)-1):length(text_split)]),c("gram_3","p_kn"), on = c("gram_1","gram_2")]
  #   }
  #   if (length(text_split)==1 | is.na(result$p_kn)) {
  #       result = n_grams[[2]][as.list(text_split[length(text_split)]),c("gram_2","p_kn"), on = c("gram_1")]
  #       # for no word entered, need BOS 2-gram
  #       # if no word found use unigram?
  #   }
  #   result=result[order(result$p_kn, decreasing = TRUE),]  
  #   print(lapply(result[1:3,1:2], as.character))
  #   #as.character(result[1,1][[1]])
  #   paste0(lapply(result[1:3,1], as.character),", ")
  #   })
  # 
}

shinyApp(ui = ui, server = server)