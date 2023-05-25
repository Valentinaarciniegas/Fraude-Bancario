setwd("C:/Users/Acer/Documents/DiplomadoAnalitica")
library(rpart)  # implementaci?n de ?rboles de decisi?n
library(rpart.plot)  # graficar los resultados de rpart
library(tidyverse)
library(caret)  
library(randomForest)
library(neuralnet)
library(ggplot2)
library(gplots)

Conjunto1 <- read.csv("Conjunto1.csv")
Conjunto2 <- read.csv("Conjunto2.csv")
Conjunto3 <- read.csv("Conjunto3.csv")
Conjunto4 <- read.csv("Conjunto4.csv")
Conjunto5 <- read.csv("Conjunto5.csv")

dentrenamiento1 <- read.csv("dentrenamientoC1.csv")
dentrenamiento2 <- read.csv("dentrenamientoC2.csv")
dentrenamiento3 <- read.csv("dentrenamientoC3.csv")
dentrenamiento4 <- read.csv("dentrenamientoC4.csv")
dentrenamiento5 <- read.csv("dentrenamientoC5.csv")

dprueba1 <- read.csv("dpruebaC1.csv")
dprueba2 <- read.csv("dpruebaC2.csv")
dprueba3 <- read.csv("dpruebaC3.csv")
dprueba4 <- read.csv("dpruebaC4.csv")
dprueba5 <- read.csv("dpruebaC5.csv")

#IMPLEMENTACION BOSQUES ALEATORIOS PARA CADA CONJUNTO DE DATOS

conjuntos_entrenamiento <- list(dentrenamiento1, 
                                dentrenamiento2, 
                                dentrenamiento3, 
                                dentrenamiento4, 
                                dentrenamiento5)

bosques_modelos <- function(conjuntos_entrenamiento) {
  modelosrf <- list()
  
  for (i in 1:length(conjuntos_entrenamiento)) {
    modelosrf[[i]] <- randomForest(as.factor(isFraud)~., 
                                   data=conjuntos_entrenamiento[[i]],  
                                   ntree=200, mtry=4, replace=T) 
  }
  
  return(modelosrf)
}

modelos_entrenadosrf <- bosques_modelos(conjuntos_entrenamiento)

#BOSQUES ALEATORIOS PARA EL CONJUNTO 1
modelorf_conjunto_1 <- modelos_entrenadosrf[[1]]
predic_classrf1 <- predict(modelorf_conjunto_1, dprueba1, type='class')
predic_probrf1 <- predict(modelorf_conjunto_1, dprueba1, type='prob')
MCBA1 <- table(dprueba1$isFraud, predic_classrf1)
MCBA1
confusionMatrix(MCBA1)
precisionBA1 <- 100*(sum(diag(MCBA1)))/sum(MCBA1)
precisionBA1

importancia1 <- as.data.frame(modelorf_conjunto_1$importance)
importancia1 <- tibble::rownames_to_column(importancia1,var = "variable")


ggplot(data = importancia1, aes(x = reorder(variable, MeanDecreaseGini),
                               y = MeanDecreaseGini,
                               fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Indice de Gini Conjunto 1") +
  geom_col() +
  coord_flip() +
  theme_bw()

#BOSQUES ALEATORIOS PARA EL CONJUNTO 2
modelorf_conjunto_2 <- modelos_entrenadosrf[[2]]
predic_classrf2 <- predict(modelorf_conjunto_2, dprueba2, type='class')
predic_probrf2 <- predict(modelorf_conjunto_2, dprueba2, type='prob')
MCBA2 <- table(dprueba2$isFraud, predic_classrf2)
MCBA2
confusionMatrix(MCBA2)
precisionBA2 <- 100*(sum(diag(MCBA2)))/sum(MCBA2)
precisionBA2

importancia2 <- as.data.frame(modelorf_conjunto_2$importance)
importancia2 <- tibble::rownames_to_column(importancia2,var = "variable")


ggplot(data = importancia2, aes(x = reorder(variable, MeanDecreaseGini),
                                y = MeanDecreaseGini,
                                fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Indice de Gini Conjunto 2") +
  geom_col() +
  coord_flip() +
  theme_bw()

#BOSQUES ALEATORIOS PARA EL CONJUNTO 3
modelorf_conjunto_3 <- modelos_entrenadosrf[[3]]
predic_classrf3 <- predict(modelorf_conjunto_3, dprueba3, type='class')
predic_probrf3 <- predict(modelorf_conjunto_3, dprueba3, type='prob')
MCBA3 <- table(dprueba3$isFraud, predic_classrf3)
MCBA3
confusionMatrix(MCBA3)
precisionBA3 <- 100*(sum(diag(MCBA3)))/sum(MCBA3)
precisionBA3

importancia3 <- as.data.frame(modelorf_conjunto_3$importance)
importancia3 <- tibble::rownames_to_column(importancia3,var = "variable")


ggplot(data = importancia3, aes(x = reorder(variable, MeanDecreaseGini),
                                y = MeanDecreaseGini,
                                fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Indice de Gini Conjunto 3") +
  geom_col() +
  coord_flip() +
  theme_bw()

#BOSQUES ALEATORIOS PARA EL CONJUNTO 4
modelorf_conjunto_4 <- modelos_entrenadosrf[[4]]
predic_classrf4 <- predict(modelorf_conjunto_4, dprueba4, type='class')
predic_probrf4 <- predict(modelorf_conjunto_4, dprueba4, type='prob')
MCBA4 <- table(dprueba4$isFraud, predic_classrf4)
MCBA4
confusionMatrix(MCBA4)
precisionBA4 <- 100*(sum(diag(MCBA4)))/sum(MCBA4)
precisionBA4

importancia4 <- as.data.frame(modelorf_conjunto_4$importance)
importancia4 <- tibble::rownames_to_column(importancia4,var = "variable")


ggplot(data = importancia4, aes(x = reorder(variable, MeanDecreaseGini),
                                y = MeanDecreaseGini,
                                fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Indice de Gini Conjunto 4") +
  geom_col() +
  coord_flip() +
  theme_bw()

#BOSQUES ALEATORIOS PARA EL CONJUNTO 5
modelorf_conjunto_5 <- modelos_entrenadosrf[[5]]
predic_classrf5 <- predict(modelorf_conjunto_5, dprueba5, type='class')
predic_probrf5 <- predict(modelorf_conjunto_5, dprueba5, type='prob')
MCBA5 <- table(dprueba5$isFraud, predic_classrf5)
MCBA5
confusionMatrix(MCBA5)
precisionBA5 <- 100*(sum(diag(MCBA5)))/sum(MCBA5)
precisionBA5

importancia5 <- as.data.frame(modelorf_conjunto_5$importance)
importancia5 <- tibble::rownames_to_column(importancia5,var = "variable")


ggplot(data = importancia5, aes(x = reorder(variable, MeanDecreaseGini),
                                y = MeanDecreaseGini,
                                fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Indice de Gini Conjunto 5") +
  geom_col() +
  coord_flip() +
  theme_bw()

