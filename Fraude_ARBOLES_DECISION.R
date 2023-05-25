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


#CREAR DATASETS DE PRUEBA Y ENTRENAMIENTO PARA CADA CONJUNTO DE DATOS

nfilas <- nrow(Conjunto1) #numero de filas del primer conjunto de datos 
pp <- trunc(nfilas*0.3)  # Devuelve la parte entera de la operaci?n
#30 por ciento del dataset

#CONJUNTO 1:
datosaleatorios1 <- sample(1:nfilas, pp) 
dprueba1 <- Conjunto1[datosaleatorios1,]
dentrenamiento1 <- Conjunto1[-datosaleatorios1,]

write.csv(dprueba1, file="dpruebaC1.csv", row.names = F)
write.csv(dentrenamiento1, file="dentrenamientoC1.csv", row.names = F)

#CONJUNTO 2:
datosaleatorios2 <- sample(1:nfilas, pp) 
dprueba2 <- Conjunto2[datosaleatorios2,]
dentrenamiento2 <- Conjunto2[-datosaleatorios2,]

write.csv(dprueba2, file="dpruebaC2.csv", row.names = F)
write.csv(dentrenamiento2, file="dentrenamientoC2.csv", row.names = F)

#CONJUNTO 3:
datosaleatorios3 <- sample(1:nfilas, pp) 
dprueba3 <- Conjunto3[datosaleatorios3,]
dentrenamiento3 <- Conjunto3[-datosaleatorios3,]

write.csv(dprueba3, file="dpruebaC3.csv", row.names = F)
write.csv(dentrenamiento3, file="dentrenamientoC3.csv", row.names = F)

#CONJUNTO 4:
datosaleatorios4 <- sample(1:nfilas, pp) 
dprueba4 <- Conjunto4[datosaleatorios4,]
dentrenamiento4 <- Conjunto4[-datosaleatorios4,]

write.csv(dprueba4, file="dpruebaC4.csv", row.names = F)
write.csv(dentrenamiento4, file="dentrenamientoC4.csv", row.names = F)

#CONJUNTO 5:
datosaleatorios5 <- sample(1:nfilas, pp) 
dprueba5 <- Conjunto5[datosaleatorios5,]
dentrenamiento5 <- Conjunto5[-datosaleatorios5,]

write.csv(dprueba5, file="dpruebaC5.csv", row.names = F)
write.csv(dentrenamiento5, file="dentrenamientoC5.csv", row.names = F)

#IMPLEMENTACION ARBOLES DE DECISION PARA CADA CONJUNTO DE DATOS

# Crear una lista de tus cinco conjuntos de datos de entrenamiento
conjuntos_entrenamiento <- list(dentrenamiento1, 
                                dentrenamiento2, 
                                dentrenamiento3, 
                                dentrenamiento4, 
                                dentrenamiento5)

arboles_modelos <- function(conjuntos_entrenamiento) {
  modelos <- list()
  
  for (i in 1:length(conjuntos_entrenamiento)) {
    modelos[[i]] <- rpart(isFraud ~ ., data = conjuntos_entrenamiento[[i]])
  }
  
  return(modelos)
}

modelos_entrenados <- arboles_modelos(conjuntos_entrenamiento)

#ARBOLES CONJUNTO 1 

modelo_conjunto_1 <- modelos_entrenados[[1]]
predic_class1 <- predict(modelo_conjunto_1, dprueba1, type='class')
predic_prob1 <- predict(modelo_conjunto_1, dprueba1, type='prob')
MCAD1 <- table(dprueba1$isFraud, predic_class1)
MCAD1
confusionMatrix(MCAD1)
precisionAD1 <- 100*(sum(diag(MCAD1)))/sum(MCAD1)
precisionAD1
rpart.plot(modelo_conjunto_1,type = 5)

#ARBOLES CONJUNTO 2

modelo_conjunto_2 <- modelos_entrenados[[2]]
predic_class2 <- predict(modelo_conjunto_2, dprueba2, type='class')
predic_prob2 <- predict(modelo_conjunto_2, dprueba2, type='prob')
MCAD2 <- table(dprueba2$isFraud, predic_class2)
MCAD2
confusionMatrix(MCAD2)
precisionAD2 <- 100*(sum(diag(MCAD2)))/sum(MCAD2)
precisionAD2
rpart.plot(modelo_conjunto_2,type = 5)

#ARBOLES CONJUNTO 3

modelo_conjunto_3 <- modelos_entrenados[[3]]
predic_class3 <- predict(modelo_conjunto_3, dprueba3, type='class')
predic_prob3 <- predict(modelo_conjunto_3, dprueba3, type='prob')
MCAD3 <- table(dprueba3$isFraud, predic_class3)
MCAD3
confusionMatrix(MCAD3)
precisionAD3 <- 100*(sum(diag(MCAD3)))/sum(MCAD3)
precisionAD3
rpart.plot(modelo_conjunto_3,type = 5)

#ARBOLES CONJUNTO 4

modelo_conjunto_4 <- modelos_entrenados[[4]]
predic_class4 <- predict(modelo_conjunto_4, dprueba4, type='class')
predic_prob4 <- predict(modelo_conjunto_4, dprueba4, type='prob')
MCAD4 <- table(dprueba4$isFraud, predic_class4)
MCAD4
confusionMatrix(MCAD4)
precisionAD4 <- 100*(sum(diag(MCAD4)))/sum(MCAD4)
precisionAD4
rpart.plot(modelo_conjunto_4,type = 5)

#ARBOLES CONJUNTO 5

modelo_conjunto_5 <- modelos_entrenados[[5]]
predic_class5 <- predict(modelo_conjunto_5, dprueba5, type='class')
predic_prob5 <- predict(modelo_conjunto_5, dprueba5, type='prob')
MCAD5 <- table(dprueba5$isFraud, predic_class5)
MCAD5
confusionMatrix(MCAD5)
precisionAD5 <- 100*(sum(diag(MCAD5)))/sum(MCAD5)
precisionAD5
rpart.plot(modelo_conjunto_5,type = 5)
