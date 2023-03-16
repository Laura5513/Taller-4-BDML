#**************************************************************************************#
#                                    TALLER 4 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Twitter                                             #
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
                     "wordcloud", "SentimentAnalysis", "stopwords", "stringi", "text2vec")

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

# 2.1.1 ----------------------------  Train -----------------------------------

tweets_train <- train

p_load("stringi")
p_load(tm)
p_load(SnowballC)
p_load(dplyr)
p_load(tidyr)
p_load(Matrix)
p_load(NLP)
p_load(text2vec)

# Eliminamos tildes y caracteres especiales del español
tweets_train <- stri_trans_general(str = train$text, id = "Latin-ASCII")
# Sin URL
tweets_train <- gsub("http\\S+", "", tweets_train)
# Sin tetweets_trainto de retwitteo
tweets_train <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", tweets_train)
# Quitamos @ (@usuario)
tweets_train <- gsub("@\\w+", "", tweets_train)
# Quitamos espacios en blanco extras
tweets_train <- gsub("\\s+", " ", tweets_train)
# Recortar los espacios en blanco iniciales y finales
tweets_train <- gsub("^\\s+|\\s+$", "", tweets_train)
# Quitamos stop words
p_load(stopwords)
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

tweets_train <- removeWords(tweets_train, lista_palabras)

# Todo minúscula
tweets_train <- tm_map(tweets_train,content_transformer(tolower)) 
#Quitamos números
#tweets_train <- tm_map(tweets_train,content_transformer(removeNumbers))
# Quitamos puntuación
tweets_train <- tm_map(tweets_train,content_transformer(removePunctuation))
# Quitamos números
#tweets_train <- tm_map(tweets_train,content_transformer(removeNumbers))
# Quitamos espacios en blanco en medio
tweets_train <- tm_map(tweets_train,content_transformer(stripWhitespace))

# Stemmizamos 
tweets_train <- wordStem(tweets_train, language = "spanish")
length(tweets_train)
corpus_train <- Corpus(VectorSource(tweets_train))

dtm_idf_train<-DocumentTermMatrix(tweets_train,control=list(weighting=weightTfIdf))
inspect(dtm_idf_train[100:103,])

dim(tweets_train)

# Eliminamos términos que son poco frecuentes en todo el corpus
dtm_idf_train <- removeSparseTerms(dtm_idf_train, sparse = 0.95)
inspect(dtm_idf_train[100:103,])


# 2.1.2 -------------------------  Test  ---------------------------------------

tweets_test <- test

p_load("stringi")
p_load(tm)
p_load(SnowballC)
p_load(dplyr)
p_load(tidyr)
p_load(Matrix)
p_load(NLP)
p_load(text2vec)

# Eliminamos tildes y caracteres especiales del español
tweets_test <- stri_trans_general(str = test$text, id = "Latin-ASCII")
# Sin URL
tweets_test <- gsub("http\\S+", "", tweets_test)
# Sin tetweets_testto de retwitteo
tweets_test <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", tweets_test)
# Quitamos @ (@usuario)
tweets_test <- gsub("@\\w+", "", tweets_test)
# Quitamos espacios en blanco extras
tweets_test <- gsub("\\s+", " ", tweets_test)
# Recortar los espacios en blanco iniciales y finales
tweets_test <- gsub("^\\s+|\\s+$", "", tweets_test)
# Quitamos stop words
p_load(stopwords)
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

tweets_test <- removeWords(tweets_test, lista_palabras)

# Todo minúscula
tweets_test <- tm_map(tweets_test,content_transformer(tolower)) 
#Quitamos números
#tweets_test <- tm_map(tweets_test,content_transformer(removeNumbers))
# Quitamos puntuación
tweets_test <- tm_map(tweets_test,content_transformer(removePunctuation))
# Quitamos números
#tweets_test <- tm_map(tweets_test,content_transformer(removeNumbers))
# Quitamos espacios en blanco en medio
tweets_test <- tm_map(tweets_test,content_transformer(stripWhitespace))

# Stemmizamos 
tweets_test <- wordStem(tweets_test, language = "spanish")
length(tweets_test)
corpus_test <- Corpus(VectorSource(tweets_test))

dtm_idf_test<-DocumentTermMatrix(tweets_test,control=list(weighting=weightTfIdf))
inspect(dtm_idf_test[100:103,])

dim(tweets_test)

# Eliminamos términos que son poco frecuentes en todo el corpus
dtm_idf_test <- removeSparseTerms(dtm_idf_test, sparse = 0.95)
inspect(dtm_idf_test[100:103,])















