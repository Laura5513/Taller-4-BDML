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

#Removemos números, puntuación y espacios en blanco excesivos. Ponemos todo en minúsculas y formato ASCII para mejor manejo.
tweets_train <- stri_trans_general(str = train$text, id = "Latin-ASCII")
tweets_train <- removeNumbers(tweets_train) # Quitamos números
tweets_train <- tolower(tweets_train) # Hacemos que todo esté en minúscula 
tweets_train <- stripWhitespace(tweets_train) # Quitamos los múltiples espacios
tweets_train <- iconv(tweets_train, from = "UTF-8", to = "ASCII//TRANSLIT") # Convertimos a ASCII para un mejor análsis con R

# Tokenizamos
tweets_train_tidy <- as.data.frame(tweets_train) %>% unnest_tokens( "word", tweets_train)

dim(tweets_train_tidy)
head(tweets_train_tidy, n = 30)

# Veamos cuáles son las palabras más frecuentes
tweets_train_tidy %>% 
  dplyr::count(word, sort = TRUE)   %>% 
  head()

# Quitamos stopwords
head(stopwords('spanish'))

tweets_train_tidy2 <- tweets_train_tidy  %>% 
  anti_join(tibble(word =stopwords("spanish")))

dim(tweets_train_tidy2) # se reducen casi a la mitad la cantidad de palabras
head(tweets_train_tidy2, n = 30) 

# Vemos cuáles son las palabras más comunes
wordcloud(tweets_train_tidy2$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

class(tweets_train_tidy2$word)

# Sacamos la raíz de las palabras (lematización)

tweets_train_tidy2$radical <- stemDocument(tweets_train_tidy2$word, language="spanish")
tweets_train_tidy2 %>% head(n=30)

# Generar bigramas a partir del texto de las críticas
bigrams <- as.data.frame(tweets_train) %>%
  unnest_tokens(bigram, tweets_train, token = "ngrams", n = 2)


stop_words <- data.frame(word1 = stopwords("es"), 
                         word2 = stopwords("es"))


# Eliminar los bigramas que contengan palabras de parada
bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words, by = "word1") %>%
  anti_join(stop_words, by = "word2") %>%
  unite(bigram, word1, word2, sep = " ")

#CÓDIGO QUE FALLA 
#bigram_freq <- bigrams %>%
# count(bigram)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

# Creamos la Document Term Matrix (DTM)

# Creamos una copia del texto lematizado
tweets_train_preprocesado <- tweets_train_tidy2$radical

# Creamos un corpus de ese texto
corpus <- Corpus(VectorSource(tweets_train_preprocesado))

# Creamos un DTM
dtm <- DocumentTermMatrix(corpus)

matrix_dtm <- as.matrix(dtm)

head(matrix_dtm)


# 2.1.2 -------------------------  Test  ---------------------------------------

tweets_test <- test

#Removemos números, puntuación y espacios en blanco excesivos. Ponemos todo en minúsculas y formato ASCII para mejor manejo.
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
wordcloud(tweets_test_tidy2$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

class(tweets_test_tidy2$word)


# Sacamos la raíz de las palabras

tweets_test_tidy2$radical <- stemDocument(tweets_test_tidy2$word, language="spanish")
tweets_test_tidy2 %>% head(n=30)

# Generar bigramas a partir del texto de las críticas
bigrams <- as.data.frame(tweets_test) %>%
  unnest_tokens(bigram, tweets_test, token = "ngrams", n = 2)


stop_words <- data.frame(word1 = stopwords("es"), 
                         word2 = stopwords("es"))


# Eliminar los bigramas que contengan palabras de parada
bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words, by = "word1") %>%
  anti_join(stop_words, by = "word2") %>%
  unite(bigram, word1, word2, sep = " ")

#CÓDIGO QUE FALLA 
#bigram_freq <- bigrams %>%
# count(bigram)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")
