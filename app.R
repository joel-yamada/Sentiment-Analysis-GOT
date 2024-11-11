# Text Analysis of Game of Thrones
# Description: A visualization of results from text analysis performed on the Game of Thrones tv-script for all seasons
# Details: Used bing lexicon for sentiment analysis and captured word trends 

# ===============================================
# R packages
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets

# =======================================================
# Sentiment Lexicons
# =======================================================
bing = read_csv("bing.csv", col_types = "cc")
afinn = read_csv("afinn.csv", col_types = "cc")
nrc = read_csv("nrc.csv", col_types = "cc")
loughran = read_csv("loughran.csv", col_types = "cc")

# ===============================================
# Import data
# ===============================================
 dat_word = read_csv(
   file = "Game_of_Thrones_Script.csv", 
   col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
   skip = 1,
   col_types = cols(
     Date = col_character(),
     Season = col_character(),
     Episode = col_character(),
     Title = col_character(),
     Name = col_character(),
     Sentence = col_character()
   )) %>% 
   select(-Date, -Season, -Episode, -Title, -Name)

# ===============================================
# Ui
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Various Text Analysis of Game of Thrones (Sentiment/Word Trend)"),
  hr(),
  
  # -------------------------------------------------------
  # Section tab
  # -------------------------------------------------------
  fluidRow(
    column(3,
           p(em("Select which season to do analysis on (Sentiment Analysis and Word Trend Analysis)")),
           selectInput(inputId = "season", 
                       label = h3("Season:"),
                       choices = list("Season 1" = 1, 
                                      "Season 2" = 2, 
                                      "Season 3" = 3,
                                      "Season 4" = 4,
                                      "Season 5" = 5, 
                                      "Season 6" = 6, 
                                      "Season 7" = 7,
                                      "Season 8" = 8), 
                       selected = 1)
    ),
    column(3,
           p(em("Choose the size of the section (Sentiment Analysis)")),
           sliderInput(inputId = "sections", 
                       label = h3("Section size:"), 
                       min = 20,
                       max = 80,
                       value = 50)
    ), 
    column(3,
           p(em("Word to see trend of (Word Trend Analysis)")),
           textInput(inputId = "word", 
                     label = h3("Word (e.g. queen, death, love, north): "),
                     value = "king")
    ),
    column(3,
           p(em("Choose the size of the set (Word Trend Analysis)")),
           numericInput(inputId = "setsize", 
                        label = h3("Set size:"), 
                        value = 200)
    ), 
  ),
  hr(),
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              # Panel for Analysis 1
              tabPanel("Sentiment Analysis",
                       h3("Sentiment Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              # Panel for Analysis 2
              tabPanel("Word Trend Analysis", 
                       h3("Word Trend Analysis"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  )
)

# ===============================================
# Server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive objects
  # ------------------------------------------------------------
  season_senti <- reactive({
    
    for(y in 1:input$season) {
      if (y == 1) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 1") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 2) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 2") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 3) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 3") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 4) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 4") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 5) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 5") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 6) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 6") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 7) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 7") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 8) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 8") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      }
    }
    
    dat_word %>% 
    mutate(linenumber = row_number()) %>% 
    unnest_tokens(word, Sentence) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(section_number = linenumber %/% input$sections, sentiment) %>% 
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
    mutate(sentiment = positive - negative)
  })
  season_senti

  word_trend <- reactive({
    
    for(y in 1:input$season) {
      if (y == 1) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 1") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 2) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 2") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 3) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 3") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 4) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 4") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 5) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 5") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 6) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 6") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 7) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 7") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      } else if (y == 8) {
        dat_word = read_csv(
          file = "Game_of_Thrones_Script.csv", 
          col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
          skip = 1,
          col_types = cols(
            Date = col_character(),
            Season = col_character(),
            Episode = col_character(),
            Title = col_character(),
            Name = col_character(),
            Sentence = col_character()
          )) %>% 
          filter(Season == "Season 8") %>% 
          select(-Date, -Season, -Episode, -Title, -Name)
      }
    }
    
    dat_word %>% 
      unnest_tokens(output = word, input = Sentence) %>% 
      anti_join(stop_words, by = "word") %>% 
      mutate(
        index = row_number(),
        set = (index %/% input$setsize) + 1
      ) %>% 
      group_by(set) %>% 
      summarise(count = sum(word == input$word))
  })
  word_trend
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  output$plot1 <- renderPlot({
    ggplot(data = season_senti(),
           aes(x = section_number, y = sentiment, fill = factor(sign(sentiment)))) +
      geom_col(show.legend = FALSE) +
      labs(title = "Sentiment Analysis across Seasons:") + 
      theme_bw()
  })
  
  output$table1 <- renderDataTable({
    season_senti()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  output$plot2 <- renderPlot({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
    ggplot(word_trend(),
           aes(x = set, y = count)) +
      geom_col(fill = "lightblue") +
      stat_smooth(method = "lm",
                  formula = y ~ poly(x, degree = 15),
                  se = FALSE) + 
      labs(title = paste("Appearance of trend word across Seasons:"),
           subtitle = "Default set size: 200")
  })
  
  output$table2 <- renderDataTable({
    word_trend()
  })
  
}

# ===============================================
# Application run
# ===============================================

shinyApp(ui = ui, server = server)

