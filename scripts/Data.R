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
tweets_train <- train$text # Importamos los datos de training

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

comment({

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
  head()})

comment({dim(tweets_test_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_test_tidy2, n = 30
})


# 2.1.1 ----------------------------  Train -----------------------------------

tweets_train <- train$text

tweets_train <- removeNumbers(tweets_train)
tweets_train <- removePunctuation(tweets_train)
tweets_train <- tolower(tweets_train)
tweets_train <- stripWhitespace(tweets_train)
tweets_train <- iconv(tweets_train, from = "UTF-8", to = "ASCII//TRANSLIT")

#tokenizamos
tweets_train_tidy <- as.data.frame(tweets_train) %>% unnest_tokens( "word", tweets_train)

dim(tweets_train_tidy)
head(tweets_train_tidy, n = 30)

#Veamos cuáles son las palabras más frecuentes
tweets_train_tidy %>% 
  dplyr::count(word, sort = TRUE)   %>% 
  head()

#quitamos stopwords
head(stopwords('spanish'))

tweets_train_tidy2 <- tweets_train_tidy  %>% 
  anti_join(tibble(word =stopwords("spanish")))

dim(tweets_train_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_train_tidy2, n = 30) 

# vemos cuáles son las palabras más comunes
wordcloud(tweets_train_tidy2$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

class(tweets_train_tidy2$word)

# 2.1.2 -------------------------  Test  ---------------------------------------
tweets_test <- test$text

tweets_test <- removeNumbers(tweets_test)
tweets_test <- removePunctuation(tweets_test)
tweets_test <- tolower(tweets_test)
tweets_test <- stripWhitespace(tweets_test)
tweets_test <- iconv(tweets_test, from = "UTF-8", to = "ASCII//TRANSLIT")

#tokenizamos
tweets_test_tidy <- as.data.frame(tweets_test) %>% unnest_tokens( "word", tweets_test)

dim(tweets_test_tidy)
head(tweets_test_tidy, n = 30)

#Veamos cuáles son las palabras más frecuentes
tweets_test_tidy %>% 
  dplyr::count(word, sort = TRUE)   %>% 
  head()

#quitamos stopwords
head(stopwords('spanish'))

tweets_test_tidy2 <- tweets_test_tidy  %>% 
  anti_join(tibble(word =stopwords("spanish")))

dim(tweets_test_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_test_tidy2, n = 30) 

# vemos cuáles son las palabras más comunes
wordcloud(tweets_train_tidy2$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

class(tweets_test_tidy2$word)

