library(shiny)
# install.packages("wordcloud2")
library(wordcloud2)
# install.packages("tm")
library(tm)
# install.packages("colourpicker")
library(colourpicker)

ui <- fluidPage(
  h1("Word Cloud"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
  # Create a container for tab panels
  tabsetPanel(
    # Create a "Word cloud" tab
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Word source",
            choices = c(
              "I Have a Dream speech by Martin Luther King Jr's" = "book",
              "Use your own words" = "own",
              "Upload a file" = "file"
            )
          ),
          hr(),
          # Add the selector for the language of the text
          selectInput(
            inputId = "language",
            label = "Remove stopwords in",
            choices = c("Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
            multiple = FALSE,
            selected = "English"
          ),
          conditionalPanel(
            condition = "input.source == 'own'",
            textAreaInput("text", "Enter text", rows = 7)
          ),
          # Wrap the file input in a conditional panel
          conditionalPanel(
            # The condition should be that the user selects
            # "file" from the radio buttons
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
            textAreaInput("words_to_remove2", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
            textAreaInput("words_to_remove3", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
            textAreaInput("words_to_remove4", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
            textAreaInput("words_to_remove5", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
            textAreaInput("words_to_remove6", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
            textAreaInput("words_to_remove7", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
            textAreaInput("words_to_remove8", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
            textAreaInput("words_to_remove9", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
            textAreaInput("words_to_remove10", "", rows = 1)
          ),
          hr(),
          numericInput("num", "Maximum number of words",
            value = 100, min = 5
          ),
          hr(),
          colourInput("col", "Background color", value = "white"),
          hr(),
          HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          # br(),
          # br(),
          # tags$a(href="https://www.antoinesoetewey.com/", "Back to www.antoinesoetewey.com"),
          br(),
          br()
        )
      )
    ),
    # Create an "About this app" tab
    tabPanel(
      title = "About this app",
      br(),
      "Instructions on how to use this Shiny app:",
      br(),
      br(),
      HTML("<ul><li>When uploading a file, make sure to upload a .csv or .txt file</li>
       <li>If it is a .csv file, there should be only one column containing all words or sentences (see below for example files)</li>
       <li>Numbers and punctuations will be automatically removed, as well as stop words in the language of your choice (via the dropdown selector)</li></ul>"),
      "Example files:",
      tags$a(href = "https://www.antoinesoetewey.com/files/ihaveadream.csv", "example.csv"),
      "and",
      tags$a(href = "https://www.antoinesoetewey.com/files/ihaveadream.txt", "example.txt"),
      br(),
      br(),
      em("Source: DataCamp"),
      br(),
      br(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>'),
      br(),
      br()
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- read.csv("ihaveadream.csv",
        sep = "&",
        stringsAsFactors = FALSE
      )
      data <- data[, 1]
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })

  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })

  create_wordcloud <- function(data, num_words = 100, background = "white") {

    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }

    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }

    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }

    wordcloud2(data, backgroundColor = background)
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
      num_words = input$num,
      background = input$col
    )
  })
}

shinyApp(ui = ui, server = server)
