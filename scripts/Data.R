#**************************************************************************************#
#                                    TALLER 3 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Tweeter                                             #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

setwd("C:/Users/lmrod/OneDrive/Documentos/GitHub/Taller-4-BDML")
#setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-4-BDML")
#setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-4-BDML")

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "plotly", "corrplot", "Hmisc", "tm", "tidytext", 
                     "wordcloud", "SentimentAnalysis")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)


# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# Predecir qué político posteó cada tweet en el test set (Claudia Lopez, Gustavo
#Petro o Alvaro Uribe)

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

# Template submission: cuenta de tweeter 
cuenta <- read.csv("./data/sample_submission.csv")

# Train
train <- read_csv("./data/train.csv", col_types = cols(
  id = col_character(),
  name = col_character(),
  text = col_character()
))

# Test 
test <- read_csv("./data/test.csv", col_types = cols(
  id = col_character(),
  text = col_character()
))

# 2.1 Preprocesamiento --------------------------------------------------------
# 2.1.1                                 Train
tweets_train <- train$text

tweets_train <- removeNumbers(tweets_train)
tweets_train <- removePunctuation(tweets_train)
tweets_train <- tolower(tweets_train)
tweets_train <- stripWhitespace(tweets_train)
tweets_train <- iconv(tweets_train, from = "UTF-8", to = "ASCII//TRANSLIT")


tweets_train_tidy <- as.data.frame(tweets_train) %>% unnest_tokens( "word", tweets_train)

dim(tweets_train_tidy)
head(tweets_train_tidy, n = 30)

tweets_train_tidy2 <- tweets_train_tidy  %>% 
  as.data.frame() %>%
  rename(tweets = 1) %>%
  unnest_tokens(output = "word", input = "tweets")

dim(tweets_train_tidy2)
head(tweets_train_tidy3, n = 30)

tweets_train_tidy2 %>% 
  count(word, sort = TRUE)   %>% 
  head()

tweets_train_tidy3 <- tweets_train_tidy2  %>% 
  anti_join(tibble(word =stopwords("spanish")))

# 2.1.2                                 Test
tweets_test <- test$text

tweets_test <- removeNumbers(tweets_test)
tweets_test <- removePunctuation(tweets_test)
tweets_test <- tolower(tweets_test)
tweets_test <- stripWhitespace(tweets_test)
tweets_test <- iconv(tweets_test, from = "UTF-8", to = "ASCII//TRANSLIT")

tweets_test_tidy <- as.data.frame(tweets_test) %>% unnest_tokens( "word", tweets_test)

dim(tweets_test_tidy)
head(tweets_test, n = 30)

tweets_test_tidy2 <- tweets_test_tidy  %>% 
  as.data.frame() %>%
  rename(tweets = 1) %>%
  unnest_tokens(output = "word", input = "tweets")

dim(tweets_test_tidy2)
head(tweets_test_tidy2, n = 30)

tweets_test_tidy2 %>% 
  count(word, sort = TRUE)   %>% 
  head()

tweets_test_tidy3 <- tweets_test_tidy2  %>% 
  anti_join(tibble(word =stopwords("spanish")))



