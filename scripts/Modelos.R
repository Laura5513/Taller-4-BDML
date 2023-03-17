#**************************************************************************************#
#                                    TALLER 3 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Properati                                           #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-4-BDML")

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "plotly", "corrplot", "Hmisc", "sf", "tmaptools", 
                     "osmdata", "leaflet", "rgeos", "yardstick", "SuperLearner", 
                     "adabag")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

# Train
train_ori <- read_csv("./data/train_final.csv")
id_train <- train_ori$`train$id`
train_ori <- train_ori[, -c(1,3)]
train_ori$name <- as.factor(train_ori$name)

# Test 
test_ori <- read_csv("./data/test_final.csv")

# ¿Hay datos vacios?
any(is.na(train_ori)) # No.
any(is.na(test_ori)) # No.

# ------------------------------------------------------------------------------------ #
# 3. Modelos
# ------------------------------------------------------------------------------------ #

set.seed(0000)

# Partición de la base de datos train, con el objetivo de evaluar el performance de los modelos.
inTrain <- createDataPartition(
  y = train_ori$name,## Nuestra  
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

training <- train_ori[ inTrain,] # Set de datos de entrenamiento
testing  <- train_ori[-inTrain,] # Set de datos de evaluación
nrow(training) # El conjunto de entrenamiento contiene el 70% de la base original

# Cross-validation
ctrl <- trainControl(
  method = "cv", 
  number = 10) # número de folds

### 3.1 Logit ------------------------------------------------------------



### 3.2 Lasso -----------------------------------------------------------------------------------------

ModeloLS<-train(name~.,
                   data=training,
                   method = 'glmnet', 
                   trControl = ctrl,
                   tuneGrid = expand.grid(alpha = 1, #lasso
                                          lambda = seq(0.001,1,by = 0.001)),
                   metric = "Accuracy"
) 

summary(ModeloLS) # Resumen del modelo
coef_lasso<-coef(ModeloLS$finalModel, ModeloLS$bestTune$lambda)
coef_lasso

## Predicción 1: Predicciones con testing
pred_test1_Modelolasso <- predict(ModeloLS, newdata = testing) # Predicción
metrics_Modelolasso <- confusionMatrix(pred_test1_Modelolasso, testing$name); metrics_Modelolasso # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test
pred_test2_Modelolasso <- predict(ModeloLS, newdata = test_ori)

# Exportar para prueba en Kaggle
Kaggle_Modelolasso <- data.frame(id=test_ori$id, name=pred_test2_Modelolasso)
write.csv(Kaggle_Modelolasso,"./stores/Kaggle_ModeloLS.csv", row.names = FALSE)

# Accuracy: 0.60333

### 3.3 Ridge -------------------------------------------------------------------------------------------
grid=10^seq(50,-50,length=1000)

ModeloRidge<- train(name~.,
                    data = training,
                    method = 'glmnet', 
                    tuneGrid = expand.grid(alpha = 0, lambda = grid), 
                    preProcess = c("center", "scale"),
                    metric = "Accuracy"
                    
)

## Predicción 1: Predicciones con testing
pred_test1_ModeloRidge <- predict(ModeloRidge, newdata = testing, type="raw")
metrics_ModeloRidge <- confusionMatrix(pred_test1_ModeloRidge, testing$name); metrics_Modelolasso # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
pred_test2_ModeloRidge <- predict(ModeloRidge, newdata = test_ori)

# Exportar para prueba en Kaggle
Kaggle_ModeloRidge <- data.frame(id=test_ori$id, name=pred_test2_ModeloRidge)
write.csv(Kaggle_ModeloRidge,"./stores/Kaggle_ModeloRidge.csv", row.names = FALSE)
# Accuracy: 0.60333

### 3.4 Elastic net -----------------------------------------------------------------------------------
ModeloEN<-caret::train(name~.,
                       data=training,
                       method = 'glmnet', 
                       trControl = ctrl,
                       tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #Lasso
                                              lambda = seq(0.001,1,by = 0.001)),
                       preProcess = c("center", "scale"), 
                       metric = "Accuracy"
) 

summary(ModeloEN) # Resumen del modelo
ggplot(varImp(ModeloEN)) # Gráfico de importancia de las variables
ModeloEN$bestTune

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloEN <- predict(ModeloEN, newdata = testing) # Predicción
metrics_ModeloEN <- confusionMatrix(pred_test1_ModeloEN, testing$name); metrics_ModeloEN # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloEN <- predict(ModeloEN, newdata = test_ori)

# Exportar para prueba en Kaggle
Kaggle_ModeloEN <- data.frame(id=test_ori$id, name=pred_test2_ModeloEN)
write.csv(Kaggle_ModeloEN,"./stores/Kaggle_ModeloEN.csv", row.names = FALSE)
# Accuracy:0.6

### 3.5 GBM -------------------------------------------------------------------------------------------
p_load(gbm)
grid_gbm<-expand.grid(n.trees=c(300,700,1000),interaction.depth=c(1:4),shrinkage=seq(0.1,1,by = 0.1),n.minobsinnode
                      =seq(10,40,by = 10))

ModeloGBM <- train(name~.,
                   data = training, 
                   method = "gbm", 
                   trControl = ctrl,
                   tuneGrid=grid_gbm,
                   metric = "Accuracy"
)            

ModeloGBM #mtry es el número de predictores.
plot(ModeloGBM)
ModeloGBM$finalModel

### Variable Importance
plot(varImp(ModeloGBM,scale=TRUE))

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloGBM <- predict(ModeloGBM, newdata = testing, type="raw")
metrics_ModeloGBM <- confusionMatrix(pred_test1_ModeloGBM, testing$name); metrics_ModeloGBM # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloGBM <- predict(ModeloGBM, newdata = test_ori)

# Exportar para prueba en Kaggle
Kaggle_ModeloGBM <- data.frame(id=test_ori$id, name=pred_test2_ModeloGBM)
write.csv(Kaggle_ModeloGBM,"./stores/Kaggle_ModeloGBM.csv", row.names = FALSE)
# Accuracy: 

### 3.5 Superlearner -------------------------------------------------------------------------------------------
p_load("SuperLearner")
ySL<-training$price
XSL<- training  %>% select(surface_covered2,bedrooms,bathrooms2,Chapinero,property_type,terraza,social,parqueadero,
                        distancia_parque,distancia_gym,distancia_transmi,distancia_cai,distancia_cc,distancia_bar,distancia_SM,
                        distancia_colegios,distancia_universidades,distancia_hospitales)

sl.lib <- c("SL.glmnet", "SL.lm", "SL.ridge", "SL.gbm" ) #lista de los algoritmos a correr

# Fit using the SuperLearner package,
ModeloSL <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

ModeloSL

## Predicción 1: Predicciones con testing
testing <- testing  %>%  mutate(yhat_Sup=predict(ModeloSL, newdata = data.frame(testing), onlySL = T)$pred)
pred_test1_ModeloSL <- testing$yhat_Sup
eva_ModeloSL <- data.frame(obs=testing$price, pred=pred_test1_ModeloSL) # Data frame con observados y predicciones
metrics_ModeloSL <- metrics(eva_ModeloSL, obs, pred); metrics_ModeloSL # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
test_bog <- test_bog  %>%  mutate(yhat_Sup=predict(ModeloSL, newdata = data.frame(test_bog), onlySL = T)$pred)
pred_test2_ModeloSL <- test_bog$yhat_Sup

# Exportar para prueba en Kaggle
Kaggle_ModeloSL <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloSL)
write.csv(Kaggle_ModeloSL,"./stores/Kaggle_ModeloSL.csv", row.names = FALSE)
# MAE: 279848874.92923


p_load("SuperLearner")
sl.lib <- c("SL.glmnet", "SL.lm", "Sl.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

Super1 <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

Super1

## NET 
# Customize the defaults for random forest.
custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))

# Look at the object.
custon_glmnet$names


sl.net <- c("SL.glmnet_1", "SL.glmnet_2", "SL.glmnet_3", "SL.glmnet_4", "SL.glmnet_5") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

Super2 <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.net)

Super2

## Predicción 1: Predicciones con testing
testing <- testing  %>%  mutate(yhat_Sup=predict(Super2, newdata = data.frame(testing), onlySL = T)$pred)
pred_test1_Super2 <- testing$yhat_Sup
eva_ModeloSuper2 <- data.frame(obs=testing$price, pred=pred_test1_ModeloSuper2) # Data frame con observados y predicciones
metrics_ModeloSuper2 <- metrics(eva_ModeloSuper2, obs, pred); metrics_ModeloSuper2 # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
test_bog <- test_bog  %>%  mutate(yhat_Sup=predict(ModeloSuper2, newdata = data.frame(test_bog), onlySL = T)$pred)
pred_test2_ModeloSuper2 <- test_bog$yhat_Sup

# Exportar para prueba en Kaggle
Kaggle_ModeloSuper2 <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloSuper2)
write.csv(Kaggle_ModeloSuper2,"./stores/Kaggle_ModeloSuper2.csv", row.names = FALSE)

### 3.6 PCA -------------------------------------------------------------------------------------------


### 3.7 Red neuronal -------------------------------------------------------------------------------------------






