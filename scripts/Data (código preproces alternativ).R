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

# 2.1.1 ----------------------------  Train -----------------------------------
tweets_train <- train # Importamos los datos de training

tweets_train <- removeNumbers(tweets_train) # Quitamos números
tweets_train <- tolower(tweets_train) # Hacemos que todo esté en minúscula 
tweets_train <- stripWhitespace(tweets_train) # Quitamos los múltiples espacios
tweets_train <- iconv(tweets_train, from = "UTF-8", to = "ASCII//TRANSLIT") # Convertimos a ASCII para un mejor análsis con R

# Tokenizamos
tweets_train_tidy <- as.data.frame(tweets_train) %>% unnest_tokens( "word", tweets_train)

# Removemos toda puntuación excepto @, letras con tílde y diéresis y comienzos de signo de pregunta y exclamación.
#También quitamos stopwords y rt que suele ser irrelevante
head(stopwords("spanish"))

tweets_train_tidy2 <- tweets_train_tidy %>% 
  mutate(word = gsub("[^[:alnum:]#@áéíóúüÁÉÍÓÚÜ¿¡]+", " ", word)) %>%
  filter(!word %in% c(stopwords("spanish"), "rt")) 

dim(tweets_train_tidy)
head(tweets_train_tidy, n = 30)

#Veamos cuáles son las palabras más frecuentes
tweets_train_tidy2 %>% 
  dplyr::count(word, sort = TRUE)   %>% 
  head()

dim(tweets_train_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_train_tidy2, n = 30


# 2.1.2 -----------------------------  Test ------------------------------------

tweets_test <- test$text # Importamos los datos de testing

tweets_test <- removeNumbers(tweets_test) # Quitamos números
tweets_test <- tolower(tweets_test) # Hacemos que todo esté en minúscula 
tweets_test <- stripWhitespace(tweets_test) # Quitamos los múltiples espacios
tweets_test <- iconv(tweets_test, from = "UTF-8", to = "ASCII//TRANSLIT") # Convertimos a ASCII para un mejor análsis con R

# Tokenizamos
tweets_test_tidy <- as.data.frame(tweets_test) %>% unnest_tokens( "word", tweets_test)

# Removemos toda puntuación excepto @, letras con tílde y diéresis y comienzos de signo de pregunta y exclamación.
#También quitamos stopwords y rt que suele ser irrelevante
head(stopwords("spanish"))

tweets_test_tidy2 <- tweets_test_tidy %>% 
  mutate(word = gsub("[^[:alnum:]#@áéíóúüÁÉÍÓÚÜ¿¡]+", " ", word)) %>%
  filter(!word %in% c(stopwords("spanish"), "rt")) 

dim(tweets_test_tidy)
head(tweets_test_tidy, n = 30)

#Veamos cuáles son las palabras más frecuentes
tweets_test_tidy2 %>% 
  dplyr::count(word, sort = TRUE)   %>% 
  head()

dim(tweets_test_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_test_tidy2, n = 30
     
#############################################################33333
library(tm)
library(SnowballC)

# Define the preprocessing function
my_preprocessing_function <- function(x) {
  # Convert to lowercase
  x <- tolower(x)
  # Quitamos tildes
  x <- stri_trans_general(str = train$text, id = "Latin-ASCII")
  # Remove URLs
  x <- gsub("http\\S+", "", x)
  # Remove retweet text
  x <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
  # Remove Twitter handles (@user)
  x <- gsub("@\\w+", "", x)
  # Remove punctuation
  x <- gsub("[[:punct:]]", " ", x)
  # Remove numbers
  #x <- gsub("\\b\\d+\\b", "", x)
  # Remove extra white spaces
  x <- gsub("\\s+", " ", x)
  # Trim leading and trailing white spaces
  x <- gsub("^\\s+|\\s+$", "", x)
  # Remove stop words
  x <- removeWords(x, lista_palabras)
  # Stem the words
  x <- wordStem(x, language = "spanish")
  # Return preprocessed text
  return(x)
}

# Create a corpus from the text column
corpus <- Corpus(VectorSource(tweets_train$text))

# Apply the preprocessing function to the corpus
corpus <- tm_map(corpus, my_preprocessing_function)

#La anterior droppea: empty document(s): 411 2121 2152 2682 2806 3558 4715 6493 9036

#removeEmptyDocs <- function(corpus) {
  #to_remove <- which(sapply(corpus, function(x) length(content(x)) == 0))
  #if (length(to_remove) > 0) {
    #corpus <- corpus[-to_remove]
  #}
  #return(corpus)
#}


# Quitamos esos documentos vacíos
corpus1 <- tm_map(corpus, removeEmptyDocs)

# Create a document-term matrix using TF-IDF weighting
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

# Convert the DTM to a matrix
matrix_dtm <- as.matrix(dtm)

# Print the resulting matrix
matrix_dtm



