require(quanteda)
library(NLP)
require(tm)
require(dplyr)
library(stringi)
library(ngram)
library(gridExtra)
library(stringr)
require(data.table)
library(markdown)
library(shiny)
require(shinythemes)


one_gram_data_table <- readRDS("one_gram_data_table.RDS")
two_gram_data_table <- readRDS("two_gram_data_table.RDS")
three_gram_data_table <- readRDS("three_gram_data_table.RDS")
four_gram_english_final <- readRDS("four_gram_english_final.RDS")
Unigram_dic <- readRDS("Unigram_dic.RDS")

dict_look_up <- 1:nrow(Unigram_dic)
names(dict_look_up) <- Unigram_dic$Unigram
dict_look_up_length <- length(dict_look_up)


l_up <- function(x) {
    for (i in 1:dict_look_up_length) {
        if (names(dict_look_up[i]) == x) {
            return(i)
        }
    }
    return(NULL)
}

finalizing <- function(input) {
    input <- tolower(input)
    input <- removePunctuation(input)
    input <- removeNumbers(input)
    input <- stripWhitespace(input)
    input_split <- unlist(strsplit(input, " "))
    input_length <- length(input_split)
    
    if (input_length > 3) {
        input <- paste0(input_split[input_length - 2], " ",
                        input_split[input_length - 1], " ",
                        input_split[input_length], sep = " ")
        
    }
    return(input)
}

predict <- function(input, max = 5) {
    input <- finalizing(input)
    input_split <- unlist(strsplit(input, ' '))
    input_length <- length(input_split)
    
    if (input_length == 1) {
        indx_word1 <- l_up(input)
        if (is.null(indx_word1)) {
            res <- head(one_gram_data_table, max)$word1
        }
        else {
            res <- head(two_gram_data_table[word1 == indx_word1], max)$word2
        }
    }
    if (input_length == 2) {
        indx_word1 <- l_up(input_split[1])
        indx_word2 <- l_up(input_split[2])
        
        trigrams_updated <- three_gram_data_table[word1 == indx_word1 & word2 == indx_word2]
        if (nrow(trigrams_updated) == 0) {
            bigrams_updated <- two_gram_data_table[word1 == indx_word2]
            
            if (nrow(bigrams_updated) == 0) {
                res <- head(one_gram_data_table, max)$word1
            }
            else {
                res <- head(bigrams_updated, max)$word2
            }
        }
        else{
            res <- head(trigrams_updated, max)$word3
        }
    }
    
    if (input_length == 3) {
        indx_word1 <- l_up(input_split[1])
        indx_word2 <- l_up(input_split[2])
        indx_word3 <- l_up(input_split[3])
        
        quadgram_updated <- four_gram_english_final[word1 == indx_word1 & word2 == indx_word2 & word3 == indx_word3]
        if (nrow(quadgram_updated) == 0) {
            trigrams_updated <- three_gram_data_table[word1 == indx_word2 & word2 == indx_word3]
            if (nrow(trigrams_updated) == 0) {
                bigrams_updated <- two_gram_data_table[word1 == indx_word3]
                if (nrow(bigrams_updated) == 0) {
                    res <- head(one_gram_data_table, max)$word1
                }
                else {
                    res <- head(bigrams_updated, max)$word2
                }
            }
            else {
                res <- head(trigrams_updated, max)$word3
            }
        }
        else {
            res <- head(quadgram_updated, max)$word4
        }
    }
    next_word <- names(dict_look_up[res])
    next_word
}


server <- shinyServer(function(input, output) {
    
    
    output$w1 <- renderText({
        predict(input$Phrase)[1]
    })
    output$w2 <- renderText({
        predict(input$Phrase)[2]
    })
    output$w3 <- renderText({
        predict(input$Phrase)[3]
    })
    output$w4 <- renderText({
        predict(input$Phrase)[4]
    })
    output$w5 <- renderText({
        predict(input$Phrase)[5]
    })
})

ui <- shinyUI(navbarPage(strong("Predictive Text Algorithm App"),
                         theme = shinytheme("cyborg"),
                         tabPanel(strong("Predict Word"),
                                  tags$style(type = "text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"),
                                  fluidPage(
                                      fluidRow(
                                          column(5, offset = 1,
                                                 tags$textarea(id = "Phrase", rows = 5, cols = 30),
                                                 submitButton("Predict next word")
                                          ),
                                          column(5,offset = 1,
                                                 h2("Next word in the order of descending probability"),
                                                 h2(strong(textOutput("w1"))),
                                                 h2(strong(textOutput("w2"))),
                                                 h2(strong(textOutput("w3"))),
                                                 h2(strong(textOutput("w4"))),
                                                 h2(strong(textOutput("w5")))
                                          )
                                      )
                                  )
                         ), 
                         tabPanel(strong("Instructions")),
                         mainPanel(
                             h3("1) Enter 1-5 words in the text box"),
                             h3("2) Press predict next word button and predicted words will appear"),
                         )
))
options(shiny.sanitize.errors = TRUE)
shinyApp(ui = ui, server = server)