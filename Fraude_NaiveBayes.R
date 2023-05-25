setwd("C:/Users/Acer/Documents/DiplomadoAnalitica")
library(rpart)  # implementaci?n de ?rboles de decisi?n
library(rpart.plot)  # graficar los resultados de rpart
library(tidyverse)
library(caret)  
library(randomForest)
library(neuralnet)
library(ggplot2)
library(gplots)
library(e1071)

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

naivebayes_modelos <- function(conjuntos_entrenamiento) {
  modelosnb <- list()
  
  for (i in 1:length(conjuntos_entrenamiento)) {
    modelosnb[[i]] <- naiveBayes(as.factor(isFraud)~., 
                                   data=conjuntos_entrenamiento[[i]]) 
  }
  
  return(modelosnb)
}

modelos_entrenadosnb <- naivebayes_modelos(conjuntos_entrenamiento)

#NAIVE BAYES PARA EL CONJUNTO 1
modelonb_conjunto_1 <- modelos_entrenadosnb[[1]]
predic_classnb1 <- predict(modelonb_conjunto_1, dprueba1, type='class')
predic_probnb1 <- predict(modelonb_conjunto_1, dprueba1, type='raw')
MCNB1 <- table(dprueba1$isFraud, predic_classnb1)
MCNB1
confusionMatrix(MCNB1)
precisionNB1 <- 100*(sum(diag(MCNB1)))/sum(MCNB1)
precisionNB1

#NAIVE BAYES PARA EL CONJUNTO 2
modelonb_conjunto_2 <- modelos_entrenadosnb[[2]]
predic_classnb2 <- predict(modelonb_conjunto_2, dprueba2, type='class')
predic_probnb2 <- predict(modelonb_conjunto_2, dprueba2, type='raw')
MCNB2 <- table(dprueba2$isFraud, predic_classnb2)
MCNB2
confusionMatrix(MCNB2)
precisionNB2 <- 100*(sum(diag(MCNB2)))/sum(MCNB2)
precisionNB2

#NAIVE BAYES PARA EL CONJUNTO 3
modelonb_conjunto_3 <- modelos_entrenadosnb[[3]]
predic_classnb3 <- predict(modelonb_conjunto_3, dprueba3, type='class')
predic_probnb3 <- predict(modelonb_conjunto_3, dprueba3, type='raw')
MCNB3 <- table(dprueba3$isFraud, predic_classnb3)
MCNB3
confusionMatrix(MCNB3)
precisionNB3 <- 100*(sum(diag(MCNB3)))/sum(MCNB3)
precisionNB3

#NAIVE BAYES PARA EL CONJUNTO 4
modelonb_conjunto_4 <- modelos_entrenadosnb[[4]]
predic_classnb4 <- predict(modelonb_conjunto_4, dprueba4, type='class')
predic_probnb4 <- predict(modelonb_conjunto_4, dprueba4, type='raw')
MCNB4 <- table(dprueba4$isFraud, predic_classnb4)
MCNB4
confusionMatrix(MCNB4)
precisionNB4 <- 100*(sum(diag(MCNB4)))/sum(MCNB4)
precisionNB4

#NAIVE BAYES PARA EL CONJUNTO 5
modelonb_conjunto_5 <- modelos_entrenadosnb[[5]]
predic_classnb5 <- predict(modelonb_conjunto_5, dprueba5, type='class')
predic_probnb5 <- predict(modelonb_conjunto_5, dprueba5, type='raw')
MCNB5 <- table(dprueba5$isFraud, predic_classnb5)
MCNB5
confusionMatrix(MCNB5)
precisionNB5 <- 100*(sum(diag(MCNB5)))/sum(MCNB5)
precisionNB5
