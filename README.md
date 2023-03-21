#**************************************************************************************#
#                                    Taller-4-BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Tweeter                                             #
#**************************************************************************************#


# Predicting tweets. "A rose by any other name would smell as sweet"

# Abstract

El objetivo del ejercicio es predecir qué cuenta de una figura política realizó cada tweet, las cuales son Claudia Lopez, Gustavo
#Petro o Alvaro Uribe.

# Document

La carpeta de "document" contiene el documento final con el desarrollo del ejercicio.

# Data files

Los datos fueron obtenidos del sitio oficial de la competición en "Kaggle" https://www.kaggle.com/competitions/uniandes-bdml-20231-ps4/
Además, se hicieron múltiples procedimientos para la obtención de la base final, los cuales están consigados en el código "DATA" en la carpeta de "scripts".
Asímismo, las variables se describen en el anexo 1 del documento pdf final en la carpeta "document".

# Code files

El ejercicio de predicción de los precios se desarrolla en R version 4.2.2 (2022-10-31 ucrt).
Los códigos para la corrida del ejercicio se encuentran almacenados en la carpeta "scripts", archivo "Modelos". 
Por lo tanto, la carpeta de "scripts" contiene: 
- DATA: El código en el script "Data" contiene nuevamente el procesamiento de las bases de datos, para facilidad en el uso de los autores.
- ESTADÍSTICAS DESCRIPTIVAS: El código en el script " " contiene estadísticas descriptivas.
- MODELOS: El código en el script "Modelos" contiene los modelos utilizados.

# Graphs

Todas las gráficas se pueden encontrar en la carpeta "views". En este se encuentran las gráficas de estadísticas descriptivas y algúnos gráficos para la definición en PCA. 

# Data dictionary

## Variable explicada Y
Corresponde a una variable de clasificación donde puede tomar el valor de Petro, Uribe, Lopez.

## Variables dependientes X 

*nota* = Algunos modelos se entrenaron bajo PCA, en este caso las variables explicativas cambian a componentes. 

 
