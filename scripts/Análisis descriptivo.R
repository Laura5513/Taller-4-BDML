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
setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-4-BDML")
setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-4-BDML")

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "tidyr",
                     "caret", "glmnet", "MLmetrics", "skimr", "stargazer", 
                     "ggplot2", "plotly",  "Hmisc", "tm", "tidytext", 
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

p_load(SnowballC)
p_load(Matrix)
p_load(NLP)

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
 
####

tweets_train <- removeWords(tweets_train, lista_palabras)

tweets_train <- removeNumbers(tweets_train)
tweets_train <- removePunctuation(tweets_train)
tweets_train <- tolower(tweets_train)
tweets_train <- stripWhitespace(tweets_train)

tweets <- as.data.frame(tweets_train) %>% unnest_tokens( "word", tweets_train)

dim(tweets) 

tweets  %>% 
  count(word, sort = TRUE)   %>% 
  head()

head(stopwords('spanish'))

tweets <- tweets %>% 
  anti_join(tibble(word =stopwords("spanish")))

dim(tweets)

# Definimos stop words adicionales 
custom_stopwords <- c("hoy", "solo", "gran", "ayer", "hacía", "san", "ahora", "debe")

# Eliminamos los nuevos stop words
tweets <- anti_join(tweets, data.frame(word = custom_stopwords))

wordcloud(tweets$word, min.freq = 90, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

tweets$radical <- stemDocument( tweets$word, language="spanish")
tweets %>% head()

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

# Calcular la frecuencia de los bigramas
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

# Cargar bibliotecas necesarias
library(tm)
library(dplyr)

# Convertir la lista de tweets a un objeto Corpus
corpus <- Corpus(VectorSource(tweets))

# Preprocesar el texto
corpus <- corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower)

# Extraer todas las palabras del Corpus
palabras <- unlist(strsplit(as.character(corpus), "\\s+"))

# Contar el número de veces que aparece cada palabra
tabla_palabras <- table(palabras)

# Ordenar la tabla de frecuencia de mayor a menor
tabla_palabras_ordenada <- sort(tabla_palabras, decreasing = TRUE)

# Convertir la tabla en un data frame para graficar
df_palabras <- data.frame(palabra = names(tabla_palabras_ordenada),
                          frecuencia = as.numeric(tabla_palabras_ordenada))

# Graficar el número de veces que aparece cada palabra
library(ggplot2)
ggplot(df_palabras[1:20, ], aes(x = palabra, y = frecuencia)) +
  geom_bar(stat = "identity") +
  xlab("Palabras") +
  ylab("Frecuencia") +
  ggtitle("Frecuencia de palabras en los tweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Crear la tabla de frecuencia ordenada de mayor a menor
tabla_palabras <- table(tolower(palabras))
tabla_palabras_ordenada <- sort(tabla_palabras, decreasing = TRUE)

# Convertir la tabla en un data frame para graficar
df_palabras <- data.frame(palabra = names(tabla_palabras_ordenada),
                          frecuencia = as.numeric(tabla_palabras_ordenada))

# Graficar el número de veces que aparece cada palabra
library(ggplot2)
ggplot(df_palabras[1:20, ], aes(x = palabra, y = frecuencia, fill = "blue")) +
  geom_bar(stat = "identity") +
  xlab("Palabras") +
  ylab("Frecuencia") +
  ggtitle("Frecuencia de palabras en los tweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))



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

#Removemos números, puntuación y espacios en blanco excesivos. Ponemos todo en minúsculas y formato ASCII para mejor manejo.
tweets_train <- stri_trans_general(str = train$text, id = "Latin-ASCII")
tweets_train <- removeNumbers(tweets_train) # Quitamos números
tweets_train <- tolower(tweets_train) # Hacemos que todo esté en minúscula 
tweets_train <- stripWhitespace(tweets_train) # Quitamos los múltiples espacios
tweets_train <- iconv(tweets_train, from = "UTF-8", to = "ASCII//TRANSLIT") # Convertimos a ASCII para un mejor análsis con R

corpus_train <- Corpus(VectorSource(tweets_train))

corpus_train<-tm_map(corpus_train,removeWords,lista_palabras)

# Vemos cómo va nuestro preprocesamiento con la observación 10
doc <- corpus_train[[10]]
cat(paste("Metadata:\n", meta(doc), "\n"))
cat(paste("Content:\n", content(doc), "\n"))

dtm_idf_train<-DocumentTermMatrix(corpus_train,control=list(weighting=weightTfIdf))
#Inspeccionemos las filas 100 a 103 de esta matriz
inspect(dtm_idf_train[100:103,])

train$text[4]
tweets_train[4]


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

bigram_freq <- bigrams %>%
 dplyr::count(bigram, sort = TRUE)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

# Cargar la librería
p_load(udpipe)

# Cargar modelo pre-entrenado para español
model <- udpipe_download_model(language = "spanish")

model <- udpipe_load_model(model$file_model)

# Separar bigramas 
bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 

# Anotar las partes del discurso en las reseñas
reviews_annotated1 <- udpipe_annotate(model, bigrams_sep$word1)


# Convertir los resultados a formato tibble
reviews_tibble1 <- as.data.frame(reviews_annotated1) %>%
  select(token, upos)

# Filtrar solo los adjetivos
adjetivos1 <- reviews_tibble1 %>%
  filter(upos == "ADJ") %>%
  select(token)

p_load(syuzhet)

# Asociar los sentiemintos (esto puede ser demorado)
sentimientos_df <- get_nrc_sentiment(adjetivos1$token, lang="spanish")

sentimientos_df$adjetivos  = adjetivos1$token

head(sentimientos_df, 10)

# Seleccionar adjetivos malos 
adjetivos_buenos <-sentimientos_df[sentimientos_df$positive==1, "adjetivos"]
adjetivos_malos  <-sentimientos_df[sentimientos_df$negative==0, "adjetivos"]

wordcloud(adjetivos_malos, min.freq = 10000, colors = c(rgb(72/255, 191/255, 169/255), rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255)), width = 800, height = 800)

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

tweets_test <- test$text

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


bigram_freq <- bigrams %>%
 dplyr::count(bigram, sort = TRUE)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

# Creamos la Document Term Matrix (DTM)

# Creamos una copia del texto lematizado
tweets_test_preprocesado <- tweets_test_tidy2$radical

# Creamos un corpus de ese texto
corpus <- Corpus(VectorSource(tweets_test_preprocesado))

# Creamos un DTM
dtm <- DocumentTermMatrix(corpus)

inspect(dtm)


inspect(tweets_test1)

p_load(stopwords)
tweets_test[[101]]
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

corpus_test <- Corpus(VectorSource(tweets_test))

tweets_test1<-tm_map(corpus_test,removeWords,lista_palabras)
tweets_test[[101]]





