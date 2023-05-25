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

redesneuronales_modelos <- function(conjuntos_entrenamiento) {
  modelosrn <- list()
  
  for (i in 1:length(conjuntos_entrenamiento)) {
    modelosrn[[i]] <- neuralnet(isFraud~., 
                                data=conjuntos_entrenamiento[[i]],  
                                hidden = c(10,10,10), rep=10, 
                                linear.output = T) 
  }
  
  return(modelosrn)
}

modelos_entrenadosrn <- redesneuronales_modelos(conjuntos_entrenamiento)

#REDES NEURONALES PARA EL CONJUNTO 1
modelorn_conjunto_1 <- modelos_entrenadosrn[[1]]
modelorn_conjunto_1$act.fct
modelorn_conjunto_1$weights
