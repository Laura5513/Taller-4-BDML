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


list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "tidyr", "fastDummies",
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
# Petro o Alvaro Uribe)

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

#Para tener las mismas columnas en train y test, preprocesaremos una base con ambas juntas.
#Generamos un identificador cuando ya las vayamos a utilizar por separado en los modelos

train$train <- 1 
test$train <- 0

# Para tener mismo número de columnas
train1 <- train[,-2]
head(train1)
dim(train1) # 9349 observaciones
head(test)
dim(test) # 1500 observaciones

# Juntamos train y test
junto <- rbind(train1, test)
dim(junto)

#Después de la posición 9349 en junto, tenemos las observaciones de test
junto$train[9349]
junto$train[9350]


p_load(SnowballC)
p_load(Matrix)
p_load(NLP)

# Eliminamos tildes y caracteres especiales del español
junto <- stri_trans_general(str = junto$text, id = "Latin-ASCII")
# Sin URL
junto <- gsub("http\\S+", "", junto)
# Sin tejuntoto de retwitteo
junto <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", junto)
# Quitamos @ (@usuario)
junto <- gsub("@\\w+", "", junto)
# Quitamos espacios en blanco extras
junto <- gsub("\\s+", " ", junto)
# Recortar los espacios en blanco iniciales y finales
junto <- gsub("^\\s+|\\s+$", "", junto)
# Remover emojis
junto <- gsub("[^\x01-\x7F]", "", junto)
# Remover puntuación 
junto <- gsub("[[:punct:]]", "", junto)
# Remover números
junto <- gsub('[0-9]+', '', junto)
# Quitamos stop words
p_load(stopwords)
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

junto <- removeWords(junto, lista_palabras)

length(junto) #10849

# Stemmizamos 
junto <- wordStem(junto, language = "spanish")
length(junto)
corpus_junto <- Corpus(VectorSource(junto))

dtm_idf_junto<-DocumentTermMatrix(junto,control=list(weighting=weightTfIdf))
inspect(dtm_idf_junto[100:103,]) # Los documentos 411 1240 2152 2682 2806 3558 4715 6493 9036 9460 quedan vacíos

# Eliminamos términos que son poco frecuentes en todo el corpus
dtm_idf_junto <- removeSparseTerms(dtm_idf_junto, sparse = 0.999)
inspect(dtm_idf_junto[100:103,]) 

dim(dtm_idf_junto)

# Test final, recoge las observaciones correspondientes a test
test_final1 <- as.data.frame(as.matrix(dtm_idf_junto[9350:nrow(dtm_idf_junto),]), stringsAsFactors = FALSE)
test_final1 <- cbind(test$id, test_final1)
colnames(test_final1) <- c("id", colnames(test_final1[2:ncol(test_final1)]))
head(test_final1)

# Train final, recoge las observaciones correspondientes a train
dummy <- ifelse(train$name=="Petro", 1, ifelse(train$name=="Lopez", 2, ifelse(train$name=="Uribe", 3, 0)))
train_final1 <- as.data.frame(as.matrix(dtm_idf_junto[1:9349, ]), stringsAsFactors=False)
train_final1 <- train_final1[, intersect(colnames(test_final1), colnames(train_final1))]
train_final1 <- cbind(train$id, train$name,  dummy, train_final1)
colnames(train_final1) <- c("id", "name", "dummy", colnames(train_final1[4:ncol(train_final1)]))

head(test_final1)
dim(test_final1)
head(train_final1)
dim(train_final1)

# La única diferencia es que train tiene dumy y name
diff_cols <- setdiff(names(train_final1), names(test_final1))

# Train
write.csv(train_final1,"./data/train_final1.csv", row.names = FALSE)

# Test
write.csv(test_final1,"./data/test_final1.csv", row.names = FALSE)

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
# Remover emojis
tweets_train <- gsub("[^\x01-\x7F]", "", tweets_train)
# Remover puntuación 
tweets_train <- gsub("[[:punct:]]", "", tweets_train)
# Remover números
tweets_train <- gsub('[0-9]+', '', tweets_train)
# Quitamos stop words
p_load(stopwords)
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

tweets_train <- removeWords(tweets_train, lista_palabras)

# Stemmizamos 
tweets_train <- wordStem(tweets_train, language = "spanish")
length(tweets_train)
corpus_train <- Corpus(VectorSource(tweets_train))

dtm_idf_train<-DocumentTermMatrix(tweets_train,control=list(weighting=weightTfIdf))
inspect(dtm_idf_train[100:103,])

# Eliminamos términos que son poco frecuentes en todo el corpus
dtm_idf_train <- removeSparseTerms(dtm_idf_train, sparse = 0.999)
inspect(dtm_idf_train[100:103,])

# 2.1.2 -------------------------  Test  ---------------------------------------

tweets_test <- test

p_load(SnowballC)
p_load(Matrix)
p_load(NLP)

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
# Remover emojis
tweets_test <- gsub("[^\x01-\x7F]", "", tweets_test)
# Remover puntuación 
tweets_test <- gsub("[[:punct:]]", "", tweets_test)
# Remover números
tweets_test <- gsub('[0-9]+', '', tweets_test)
# Quitamos stop words
p_load(stopwords)
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

tweets_test <- removeWords(tweets_test, lista_palabras)

# Stemmizamos 
tweets_test <- wordStem(tweets_test, language = "spanish")
length(tweets_test)
corpus_test <- Corpus(VectorSource(tweets_test))

dtm_idf_test<-DocumentTermMatrix(tweets_test,control=list(weighting=weightTfIdf))
inspect(dtm_idf_test[100:103,])
dim(tweets_test)

# Eliminamos términos que son poco frecuentes en todo el corpus
dtm_idf_test <- removeSparseTerms(dtm_idf_test, sparse = 0.999)
inspect(dtm_idf_test[100:103,])

##############################################################################

wordcloud(tweets_train$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255)))

# 2.2 Exportar bases de datos finales --------------------------------------------

# Test final
test_final <- as.data.frame(as.matrix(dtm_idf_test), stringsAsFactors=False)
test_final <- cbind(test$id, test_final)
colnames(test_final) <- c("id", colnames(test_final[2:ncol(test_final)]))

# Train final
dummy <- ifelse(train$name=="Petro", 1, ifelse(train$name=="Lopez", 2, ifelse(train$name=="Uribe", 3, 0)))
train_final <- as.data.frame(as.matrix(dtm_idf_train), stringsAsFactors=False)
train_final <- train_final[, intersect(colnames(test_final), colnames(train_final))]
train_final <- cbind(train$id, train$name,  dummy, train_final)
colnames(train_final) <- c("id", "name", "dummy", colnames(train_final[4:ncol(train_final)]))

# Train
write.csv(train_final,"./data/train_final.csv", row.names = FALSE)

# Test
write.csv(test_final,"./data/test_final.csv", row.names = FALSE)














