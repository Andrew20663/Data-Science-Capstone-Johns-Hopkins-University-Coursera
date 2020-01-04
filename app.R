#Load libraries
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
library(here)
library(shiny)
require(shinythemes)


#Clean Blogs and sample .05% of the lines due to RAM and memory limitations
blogs_file <- "final/en_US/en_US.blogs.txt"
blogs_connection <- file(blogs_file, "r")
blogs_lines <- readLines(blogs_connection, encoding = "UTF-8", skipNul = T)
close(blogs_connection)
blogs_lines_lower <- tolower(blogs_lines)
blogs_lines_lower_updated <- unlist(strsplit(blogs_lines_lower, "[.,:;!?(){}<>]+"))
cleaned_blogs <- blogs_lines_lower_updated %>% 
  {gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", blogs_lines_lower_updated)} %>% 
  {gsub("[^a-z0-9]+\\s", " ", blogs_lines_lower_updated)} %>% 
  {gsub("\\s[^a-z0-9]+", " ", blogs_lines_lower_updated)} %>% 
  {gsub("\\s+", " ", blogs_lines_lower_updated)} %>% 
  {str_trim(blogs_lines_lower_updated)}
head(cleaned_blogs, 20)
saveRDS(cleaned_blogs, "cleaned_blogs.rds")
set.seed(1234)
blogs_sample <- cleaned_blogs[sample(1:length(cleaned_blogs), length(cleaned_blogs)*.0005,
                                     replace = F)]
#Create Corpus and create 1-gram, 2-gram, 3-gram, 4-gram dataframes
blogs_corpus <- corpus(blogs_sample)
remove(blogs_sample)
saveRDS(blogs_corpus, "blogs_corpus.rds")
blog_tokenized <- tokens(blogs_corpus, remove_symbols = T, remove_numbers = T,
                         remove_punct = T, remove_twitter = T)
blogs_one_gram <- dfm(blog_tokenized, ngrams = 1L, tolower = T, stem = F)
blogs_one_gram <- dfm_weight(x = blogs_one_gram, scheme = "prop")
blogs_one_gram <- t(rbind(names(blogs_one_gram), colSums(blogs_one_gram)))
blogs_one_gram <- as.data.frame(blogs_one_gram)
blogs_one_gram[ , 1] <- as.numeric(blogs_one_gram[ , 1])
blogs_one_gram <- cbind(rownames(blogs_one_gram), blogs_one_gram)
blogs_one_gram <- blogs_one_gram[order(blogs_one_gram[ , 2], decreasing = T) , ]
saveRDS(blogs_one_gram, "blogs_one_gram.rds")
save(blogs_one_gram, file = "blogs_one_gram.RData")
remove(blogs_one_gram)
blogs_two_gram <- dfm(blog_tokenized, ngrams = 2L, tolower = T, stem = F)
blogs_two_gram <- dfm_weight(x = blogs_two_gram, scheme = "prop")
blogs_two_gram <- t(rbind(names(blogs_two_gram), colSums(blogs_two_gram)))
blogs_two_gram <- as.data.frame(blogs_two_gram)
blogs_two_gram[ , 1] <- as.numeric(blogs_two_gram[ , 1])
blogs_two_gram <- cbind(rownames(blogs_two_gram), blogs_two_gram)
blogs_two_gram <- blogs_two_gram[order(blogs_two_gram[ , 2], decreasing = T) , ]
saveRDS(blogs_two_gram, "blogs_two_gram.rds")
save(blogs_two_gram, file = "blogs_two_gram.RData")
remove(blogs_two_gram)
blogs_three_gram <- dfm(blog_tokenized, ngrams = 3L, tolower = T, stem = F)
blogs_three_gram <- dfm_weight(x = blogs_three_gram, scheme = "prop")
blogs_three_gram <- t(rbind(names(blogs_three_gram), colSums(blogs_three_gram)))
blogs_three_gram <- as.data.frame(blogs_three_gram)
blogs_three_gram[ , 1] <- as.numeric(blogs_three_gram[ , 1])
blogs_three_gram <- cbind(rownames(blogs_three_gram), blogs_three_gram)
blogs_three_gram <- blogs_three_gram[order(blogs_three_gram[ , 2], decreasing = T) , ]
saveRDS(blogs_three_gram, "blogs_three_gram.rds")
save(blogs_three_gram, file = "blogs_three_gram.RData")
remove(blogs_three_gram)
blogs_four_gram <- dfm(blog_tokenized, ngrams = 4L, tolower = T, stem = F)
blogs_four_gram <- dfm_weight(x = blogs_four_gram, scheme = "prop")
blogs_four_gram <- t(rbind(names(blogs_four_gram), colSums(blogs_four_gram)))
blogs_four_gram <- as.data.frame(blogs_four_gram)
blogs_four_gram[ , 1] <- as.numeric(blogs_four_gram[ , 1])
blogs_four_gram <- cbind(rownames(blogs_four_gram), blogs_four_gram)
blogs_four_gram <- blogs_four_gram[order(blogs_four_gram[ , 2], decreasing = T) , ]
saveRDS(blogs_four_gram, "blogs_four_gram.rds")
save(blogs_four_gram, file = "blogs_four_gram.RData")
remove(blogs_four_gram)

#Twitter cleaning and sample .05% of the lines due to RAM and memory limitations
twitter_file <- "final/en_US/en_US.twitter.txt"
twitter_connection <- file(twitter_file, "r")
twitter_lines <- readLines(twitter_connection, encoding = "UTF-8", skipNul = T)
close(twitter_connection)
twitter_lines_lower <- tolower(twitter_lines)
twitter_lines_lower_updated <- unlist(strsplit(twitter_lines_lower, "[.,:;!?(){}<>]+"))

cleaned_twitter <- twitter_lines_lower_updated %>% 
  {gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", twitter_lines_lower_updated)} %>% 
  {gsub("[^a-z0-9]+\\s", " ", twitter_lines_lower_updated)} %>% 
  {gsub("\\s[^a-z0-9]+", " ", twitter_lines_lower_updated)} %>% 
  {gsub("\\s+", " ", twitter_lines_lower_updated)} %>% 
  {str_trim(twitter_lines_lower_updated)}
head(cleaned_twitter, 20)
saveRDS(cleaned_twitter, "cleaned_twitter.rds")
set.seed(1234)
twitter_sample <- cleaned_twitter[sample(1:length(cleaned_twitter), length(cleaned_twitter)*.0005, 
                                         replace = F)]
saveRDS(twitter_sample, "sampled_twitter_text.rds")
#Create Corpus and create 1-gram, 2-gram, 3-gram, 4-gram dataframes
twitter_corpus <- corpus(twitter_sample)
remove(twitter_sample)
saveRDS(twitter_corpus, "sampled_twitter_text.rds")
twitter_tokenized <- tokens(twitter_corpus, remove_symbols = T, remove_numbers = T,
                            remove_punct = T, remove_twitter = T)
twitter_one_gram <- dfm(twitter_tokenized, ngrams = 1L, tolower = T, stem = F)
twitter_one_gram <- dfm_weight(x = twitter_one_gram, scheme = "prop")
twitter_one_gram <- t(rbind(names(twitter_one_gram), colSums(twitter_one_gram)))
twitter_one_gram <- as.data.frame(twitter_one_gram)
twitter_one_gram[ , 1] <- as.numeric(twitter_one_gram[ , 1])
twitter_one_gram <- cbind(rownames(twitter_one_gram), twitter_one_gram)
twitter_one_gram <- twitter_one_gram[order(twitter_one_gram[ , 2], decreasing = T) , ]
saveRDS(twitter_one_gram, "twitter_one_gram.rds")
save(twitter_one_gram, file = "twitter_one_gram.RData")
remove(twitter_one_gram)
twitter_two_gram <- dfm(twitter_tokenized, ngrams = 2L, tolower = T, stem = F)
twitter_two_gram <- dfm_weight(x = twitter_two_gram, scheme = "prop")
twitter_two_gram <- t(rbind(names(twitter_two_gram), colSums(twitter_two_gram)))
twitter_two_gram <- as.data.frame(twitter_two_gram)
twitter_two_gram[ , 1] <- as.numeric(twitter_two_gram[ , 1])
twitter_two_gram <- cbind(rownames(twitter_two_gram), twitter_two_gram)
twitter_two_gram <- twitter_two_gram[order(twitter_two_gram[ , 2], decreasing = T) , ]
saveRDS(twitter_two_gram, "twitter_two_gram.rds")
save(twitter_two_gram, file = "twitter_two_gram.RData")
remove(twitter_two_gram)
twitter_three_gram <- dfm(twitter_tokenized, ngrams = 3L, tolower = T, stem = F)
twitter_three_gram <- dfm_weight(x = twitter_three_gram, scheme = "prop")
twitter_three_gram <- t(rbind(names(twitter_three_gram), colSums(twitter_three_gram)))
twitter_three_gram <- as.data.frame(twitter_three_gram)
twitter_three_gram[ , 1] <- as.numeric(twitter_three_gram[ , 1])
twitter_three_gram <- cbind(rownames(twitter_three_gram), twitter_three_gram)
twitter_three_gram <- twitter_three_gram[order(twitter_three_gram[ , 2], decreasing = T) , ]
saveRDS(twitter_three_gram, "twitter_three_gram.rds")
save(twitter_three_gram, file = "twitter_three_gram.RData")
remove(twitter_three_gram)
twitter_four_gram <- dfm(twitter_tokenized, ngrams = 4L, tolower = T, stem = F)
twitter_four_gram <- dfm_weight(x = twitter_four_gram, scheme = "prop")
twitter_four_gram <- t(rbind(names(twitter_four_gram), colSums(twitter_four_gram)))
twitter_four_gram <- as.data.frame(twitter_four_gram)
twitter_four_gram[ , 1] <- as.numeric(twitter_four_gram[ , 1])
twitter_four_gram <- cbind(rownames(twitter_four_gram), twitter_four_gram)
twitter_four_gram <- twitter_four_gram[order(twitter_four_gram[ , 2], decreasing = T) , ]
saveRDS(twitter_four_gram, "twitter_four_gram.rds")
save(twitter_four_gram, file = "twitter_four_gram.RData")
remove(twitter_four_gram)

#News cleaning and sample .05% of the lines due to RAM and memory limitations
us_news <- file("final/en_US/en_US.news.txt", "rb")
us_news_lines <- readLines(us_news, encoding = "UTF-8", skipNul = T)
close(us_news)
lower_us_news <- tolower(us_news_lines)
lower_us_news_updated <- unlist(strsplit(lower_us_news, "[.,:;!?(){}<>]+"))
cleaned_us_news <- lower_us_news_updated %>% 
  {gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", lower_us_news_updated)} %>% 
  {gsub("[^a-z0-9]+\\s", " ", lower_us_news_updated)} %>% 
  {gsub("\\s[^a-z0-9]+", " ", lower_us_news_updated)} %>% 
  {gsub("\\s+", " ", lower_us_news_updated)} %>% 
  {str_trim(lower_us_news_updated)}
head(cleaned_us_news, 20)
saveRDS(cleaned_us_news, "cleaned_news.rds")
set.seed(1234)
news_sample <- cleaned_us_news[sample(1:length(cleaned_us_news), length(cleaned_us_news)*.0005, 
                                      replace = F)]
#Create Corpus and create 1-gram, 2-gram, 3-gram, 4-gram dataframes
news_corpus <- corpus(news_sample)
remove(news_sample)
saveRDS(news_corpus, "blogs_corpus.rds")
news_tokenized <- tokens(news_corpus, remove_symbols = T, remove_numbers = T,
                         remove_punct = T, remove_twitter = T)
news_one_gram <- dfm(news_tokenized, ngrams = 1L, tolower = T, stem = F)
news_one_gram <- dfm_weight(x = news_one_gram, scheme = "prop")
news_one_gram <- t(rbind(names(news_one_gram), colSums(news_one_gram)))
news_one_gram <- as.data.frame(news_one_gram)
news_one_gram[ , 1] <- as.numeric(news_one_gram[ , 1])
news_one_gram <- cbind(rownames(news_one_gram), news_one_gram)
news_one_gram <- news_one_gram[order(news_one_gram[ , 2], decreasing = T) , ]
saveRDS(news_one_gram, "news_one_gram.rds")
save(news_one_gram, file = "news_one_gram.RData")
remove(news_one_gram)
news_two_gram <- dfm(news_tokenized, ngrams = 2L, tolower = T, stem = F)
news_two_gram <- dfm_weight(x = news_two_gram, scheme = "prop")
news_two_gram <- t(rbind(names(news_two_gram), colSums(news_two_gram)))
news_two_gram <- as.data.frame(news_two_gram)
news_two_gram[ , 1] <- as.numeric(news_two_gram[ , 1])
news_two_gram <- cbind(rownames(news_two_gram), news_two_gram)
news_two_gram <- news_two_gram[order(news_two_gram[ , 2], decreasing = T) , ]
saveRDS(news_two_gram, "news_two_gram.rds")
save(news_two_gram, file = "news_two_gram.RData")
remove(news_two_gram)
news_three_gram <- dfm(news_tokenized, ngrams = 3L, tolower = T, stem = F)
news_three_gram <- dfm_weight(x = news_three_gram, scheme = "prop")
news_three_gram <- t(rbind(names(news_three_gram), colSums(news_three_gram)))
news_three_gram <- as.data.frame(news_three_gram)
news_three_gram[ , 1] <- as.numeric(news_three_gram[ , 1])
news_three_gram <- cbind(rownames(news_three_gram), news_three_gram)
news_three_gram <- news_three_gram[order(news_three_gram[ , 2], decreasing = T) , ]
saveRDS(news_three_gram, "news_three_gram.rds")
save(news_three_gram, file = "news_three_gram.RData")
remove(news_three_gram)
news_four_gram <- dfm(news_tokenized, ngrams = 4L, tolower = T, stem = F)
news_four_gram <- dfm_weight(x = news_four_gram, scheme = "prop")
news_four_gram <- t(rbind(names(news_four_gram), colSums(news_four_gram)))
news_four_gram <- as.data.frame(news_four_gram)
news_four_gram[ , 1] <- as.numeric(news_four_gram[ , 1])
news_four_gram <- cbind(rownames(news_four_gram), news_four_gram)
news_four_gram <- news_four_gram[order(news_four_gram[ , 2], decreasing = T) , ]
saveRDS(news_four_gram, "news_four_gram.rds")
save(news_four_gram, file = "news_four_gram.RData")
remove(news_four_gram)

#Combine all 1 gram and all other grams after creating N-gram index dictionary
load("blogs_one_gram.RData")
colnames(blogs_one_gram) <- c("Unigram", "Count")
blogs_one_gram$Unigram <- as.character(blogs_one_gram$Unigram)
load("twitter_one_gram.RData")
colnames(twitter_one_gram) <- c("Unigram", "Count")
twitter_one_gram$Unigram <- as.character(twitter_one_gram$Unigram)
load("news_one_gram.RData")
colnames(news_one_gram) <- c("Unigram", "Count")
news_one_gram$Unigram <- as.character(news_one_gram$Unigram)
Unigram <- bind_rows(blogs_one_gram, twitter_one_gram, news_one_gram)
Unigram <- na.omit(Unigram, cols = "Unigram")
Unigram_dic <- Unigram
save(Unigram_dic, file = "Unigram_dic.RData")

#Creation of N-gram index dictionary
dict_look_up <- 1:nrow(Unigram_dic)
names(dict_look_up) <- Unigram_dic$Unigram
word_match <- function(v, dict_look_up) {
  cols <- length(sapply(names(v), function(x) strsplit(x, '_'))[[1]])
  output_matrix <- matrix(0, length(v), cols)
  for (i in 1:cols) {
    output_matrix[,i] <- dict_look_up[sapply(sapply(names(v), function(x) strsplit(x, '_')), function(x) x[[i]])]
  }
  unname(cbind(output_matrix, v))
}
one_gram_vector <- Unigram$Count
names(one_gram_vector) <- Unigram$Unigram
Unigram <- word_match(one_gram_vector, dict_look_up)
colnames(Unigram) <- c("word1", "Count")
one_gram_data_table <- as.data.table(Unigram)
saveRDS(one_gram_data_table, "one_gram_data_table.RDS")
save(one_gram_data_table, file = "one_gram_data_table.RDdata")


load("blogs_two_gram.RData")
colnames(blogs_two_gram) <- c("Bigram", "Count")
blogs_two_gram$Bigram <- as.character(blogs_two_gram$Bigram)
load("twitter_two_gram.RData")
colnames(twitter_two_gram) <- c("Bigram", "Count")
twitter_two_gram$Bigram <- as.character(twitter_two_gram$Bigram)
load("news_two_gram.RData")
colnames(news_two_gram) <- c("Bigram", "Count")
news_two_gram$Bigram <- as.character(news_two_gram$Bigram)
Bigram <- bind_rows(blogs_two_gram, twitter_two_gram, news_two_gram)
Bigram <- na.omit(Bigram, cols = "Bigram")


two_gram_vector <- Bigram$Count
names(two_gram_vector) <- Bigram$Bigram
Bigram <- word_match(two_gram_vector, dict_look_up)
colnames(Bigram) <- c("word1","word2", "Count")
two_gram_data_table <- as.data.table(Bigram)
saveRDS(two_gram_data_table, "two_gram_data_table.RDS")
save(two_gram_data_table, file = "two_gram_data_table.RData")

load("blogs_three_gram.RData")
colnames(blogs_three_gram) <- c("Trigram", "Count")
blogs_three_gram$Trigram <- as.character(blogs_three_gram$Trigram)
load("twitter_three_gram.RData")
colnames(twitter_three_gram) <- c("Trigram", "Count")
twitter_three_gram$Trigram <- as.character(twitter_three_gram$Trigram)
load("news_three_gram.RData")
colnames(news_three_gram) <- c("Trigram", "Count")
news_three_gram$Trigram <- as.character(news_three_gram$Trigram)
Trigram <- bind_rows(blogs_three_gram, twitter_three_gram, news_three_gram)
Trigram <- na.omit(Trigram, cols = "Trigram")

three_gram_vector <- Trigram$Count
names(three_gram_vector) <- Trigram$Trigram
Trigram <- word_match(three_gram_vector, dict_look_up)
colnames(Trigram) <- c("word1", "word2","word3", "Count")
three_gram_data_table <- as.data.table(Trigram)
saveRDS(three_gram_data_table, "three_gram_data_table.RDS")
save(three_gram_data_table, file = "three_gram_data_table.RData")


load("blogs_four_gram.RData")
colnames(blogs_four_gram) <- c("Quadgram", "Count")
blogs_four_gram$Quadgram <- as.character(blogs_four_gram$Quadgram)
load("twitter_four_gram.RData")
colnames(twitter_four_gram) <- c("Quadgram", "Count")
twitter_four_gram$Quadgram <- as.character(twitter_four_gram$Quadgram)
load("news_four_gram.RData")
colnames(news_four_gram) <- c("Quadgram", "Count")
news_four_gram$Quadgram <- as.character(news_four_gram$Quadgram)
Quadgram <- bind_rows(blogs_four_gram, twitter_four_gram, news_four_gram)
Quadgram <- na.omit(Quadgram, cols = "Quadgram")
save(Quadgram, file = "four_gram_english_dict.RData")

partition <- round(nrow(Quadgram)/3)
partition_1 <- Quadgram[1:partition,]
partition_2 <- Quadgram[(partition + 1):(partition * 2),]
partition_3 <- Quadgram[((partition * 2) + 1):nrow(Quadgram),]


partition_vector_1 <- partition_1$Count
names(partition_vector_1) <- partition_1$Quadgram
partition_1 <- word_match(partition_vector_1, dict_look_up)
colnames(partition_1) <- c("word1", "word2", "word3", "word4", "Count")

partition_vector_2 <- partition_2$Count
names(partition_vector_2) <- partition_2$Quadgram
partition_2 <- word_match(partition_vector_2, dict_look_up)
colnames(partition_2) <- c("word1", "word2", "word3", "word4", "Count")

partition_vector_3 <- partition_3$Count 
names(partition_vector_3) <- partition_3$Quadgram
partition_3 <- word_match(partition_vector_3, dict_look_up)
colnames(partition_3) <- c("word1", "word2", "word3", "word4", "Count")

four_gram_english_final <- rbind(partition_1, partition_2, partition_3)
four_gram_english_final <- as.data.table(four_gram_english_final)
saveRDS(four_gram_english_final, "four_gram_english_final.RDS")
save(four_gram_english_final, file = "four_gram_english_final.RData")



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
