library(rpart)  # implementaci?n de ?rboles de decisi?n
library(rpart.plot)  # graficar los resultados de rpart
library(tidyverse)
library(caret)  
library(randomForest)
library(neuralnet)
library(ggplot2)
#install.packages("scales")
library(scales)

#DETECCION DE FRAUDE 
datosOriginales <- read.csv("datasetfraude.csv")
colnames(datosOriginales)[colnames(datosOriginales) == "isFlaggedFraud"] <- "bandera"
#table(datosOriginales$isFraud)
#table(datosOriginales$bandera)
datosOriginales$bandera <- NULL

#ANALISIS DE COLUMNAS EN CERO 
columnas_interes <- c("amount", "oldbalanceOrg", "newbalanceOrig", "oldbalanceDest", "newbalanceDest")
registros_ceros <- subset(datosOriginales, 
                          amount == 0 & oldbalanceOrg == 0 & newbalanceOrig == 0 & 
                            oldbalanceDest == 0 & newbalanceDest==0)

datosOriginales <- subset(datosOriginales, !(amount == 0 & oldbalanceOrg == 0 & newbalanceOrig == 0 & 
                            oldbalanceDest == 0 & newbalanceDest==0))

#Monto en cero y fraude 1
registros_raros <- subset(datosOriginales, amount==0 & isFraud==1)
datosOriginales <- subset(datosOriginales, !(amount==0 & isFraud==1))

#cuenta vieja y nueva destino iguales 
registosdest_iguales <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & 
                                 (oldbalanceDest == newbalanceDest) & isFraud==0)
datosOriginales <- subset(datosOriginales, !((oldbalanceOrg==newbalanceOrig) & 
                                               (oldbalanceDest == newbalanceDest) & isFraud==0))

#analisis de namecuenta igual entrada y salida del dinero
registrosnames_iguales <- subset(datosOriginales, nameOrig == nameDest) #cero

#duplicados origen 
registros_duplicados <- subset(datosOriginales, duplicated(datosOriginales$nameOrig))

#monto entre 0 y 1000 con fraude 
registrosmonto_fraude <- subset(datosOriginales, (amount >0 & amount<100) & isFraud==1)

registrosmonto_nofraude <- subset(datosOriginales, (amount >0 & amount<100) & isFraud==0)

#saldo viejo y nuevo origen en 0 y fraude 0 Y tipo cash out
registros_1 <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="CASH_OUT")
datosOriginales <- subset(datosOriginales, !((oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="CASH_OUT"))

#saldo viejo y nuevo origen en 0 y fraude 0 Y tipo cash in
registros_2 <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="CASH_IN")
datosOriginales <- subset(datosOriginales, !((oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="CASH_IN"))

#saldo viejo y nuevo origen en 0 y fraude 0 Y tipo transfer
registros_3 <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="TRANSFER")
datosOriginales <- subset(datosOriginales, !((oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="TRANSFER"))

#saldo viejo y nuevo origen en 0 y fraude 0 Y tipo payment esta en cero 
registros_4 <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="PAYMENT")

#saldo viejo y nuevo origen en 0 y fraude 0 Y tipo debit
registros_5 <- subset(datosOriginales, (oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="DEBIT")
datosOriginales <- subset(datosOriginales, !((oldbalanceOrg==newbalanceOrig) & isFraud==0 & type=="DEBIT"))


#saldo nuevo y viejo dest cero fraude o y cash out
registro_6 <- subset(datosOriginales, (oldbalanceDest==newbalanceDest) & isFraud==0 & type=="CASH_OUT")

registro_7 <- subset(datosOriginales, (oldbalanceDest==newbalanceDest) & isFraud==0 & type=="CASH_IN")

registro_8 <- subset(datosOriginales, (oldbalanceDest==newbalanceDest) & isFraud==0 & type=="TRANSFER")
datosOriginales <- subset(datosOriginales, !((oldbalanceDest==newbalanceDest) & isFraud==0 & type=="TRANSFER"))

registro_9 <- subset(datosOriginales, (oldbalanceDest==newbalanceDest) & isFraud==0 & type=="PAYMENT")
resta <- datosOriginales$oldbalanceOrg-datosOriginales$newbalanceOrig
#LOS QUE LA RESTA ES IGUAL AL MONTO
registro_11 <- subset(datosOriginales, ((resta==am2ount) & isFraud==0 & type=="PAYMENT"))
#LOS QUE LA RESTA ES DIFERENTE AL MONTO 
registro_12 <- subset(datosOriginales, ((resta!=amount) & isFraud==0 & type=="PAYMENT"))


registro_10 <- subset(datosOriginales, (oldbalanceDest==newbalanceDest) & isFraud==0 & type=="DEBIT") #cero

table(datosOriginales$isFraud)

write.csv(datosOriginales, file = "datos_limpios.csv", row.names = F)

#filtro montos altos 

max(datosOriginales$amount)
montobigotes <- boxplot(datosOriginales$amount)
#eliminar montos mayores de 45 millones que no son fraudulentos 
registro_13 <- subset(datosOriginales, amount>=42000000)
datosOriginales <- subset(datosOriginales, !(amount>=42000000))
boxplot(datosOriginales$amount)
#MINIMO Y MAXIMO DE CADA CUENTA

gvo <- boxplot(datosOriginales$oldbalanceOrg)
registro_14 <- subset(datosOriginales, oldbalanceOrg>=50000000)

#registro_15 <- subset(datosOriginales, isFraud==1)
#median(registro_15$oldbalanceOrg)

g1 <- boxplot(datosOriginales$newbalanceOrig)
registro_15 <- subset(datosOriginales, newbalanceOrig>=42000000)

g2 <- boxplot(datosOriginales$oldbalanceDest)
registros_16 <- subset(datosOriginales, oldbalanceDest>=250000000)
datosOriginales <- subset(datosOriginales, !(oldbalanceDest>=250000000))

g3 <- boxplot(datosOriginales$newbalanceDest)
registros_17 <- subset(datosOriginales, (newbalanceDest>=200000000 & newbalanceOrig==0))
datosOriginales <- subset(datosOriginales, !(newbalanceDest>=200000000 & newbalanceOrig==0))

min(datosOriginales$amount)
max(datosOriginales$amount)

min(datosOriginales$oldbalanceOrg)
max(datosOriginales$oldbalanceOrg)

min(datosOriginales$newbalanceOrig)
max(datosOriginales$newbalanceOrig)

min(datosOriginales$oldbalanceDest)
max(datosOriginales$oldbalanceDest)

min(datosOriginales$newbalanceDest)
max(datosOriginales$newbalanceDest)

table(datosOriginales$type)

#table(datosOriginales$step)

#NORMALIZACION DE VARIABLES AMOUNT, OLDBALANCEORIG, NEWBALANCEORIG, OLDBALANCEDEST, NEWBALANCEDEST
datosOriginales$amount <- trunc(datosOriginales$amount)
datosOriginales$oldbalanceOrg <- trunc(datosOriginales$oldbalanceOrg)
datosOriginales$newbalanceOrig <- trunc(datosOriginales$newbalanceOrig)
datosOriginales$oldbalanceDest <- trunc(datosOriginales$oldbalanceDest)
datosOriginales$newbalanceDest <- trunc(datosOriginales$newbalanceDest)

# Configurar la visualización en formato decimal completo
options(scipen = 999)

datosOriginales$amount <- rescale(datosOriginales$amount, to = c(0,1))
datosOriginales$oldbalanceOrg <- rescale(datosOriginales$oldbalanceOrg, to = c(0,1))
datosOriginales$newbalanceOrig <- rescale(datosOriginales$newbalanceOrig, to = c(0,1))
datosOriginales$oldbalanceDest <- rescale(datosOriginales$oldbalanceDest, to = c(0,1))
datosOriginales$newbalanceDest <- rescale(datosOriginales$newbalanceDest, to = c(0,1))

#GRAFICAR TIPO DE MOVIMIENTO DEL DATASET DATOS ORIGINALES 
#table(datosOriginales$type)
barplot((table(datosOriginales$type)),  ylim=c(0, 2500000),
        main = "Tipos de movimiento",
        ylab = "Frecuencia", col="aquamarine3")

a <- table(datosOriginales$isFraud, datosOriginales$type)
barplot(a, beside = TRUE)

barplot(a, beside = TRUE, las=1, 
        xlab='Movimiento', ylab='Frecuencia',
        col = c("lightblue", "mistyrose"),
        ylim = c(0, 2500000))
legend('topleft', legend=rownames(a), bty='n',
       fill=c("lightblue", "mistyrose"))

varsDummy <- model.matrix( ~type-1, data = datosOriginales)
datosOriginales <- cbind(datosOriginales,varsDummy)
datosOriginales$type <-NULL
#Crear una copia del dataset original para poder eliminar las variables categorias
datosOriginalesNum <- datosOriginales
datosOriginalesNum$nameOrig <- NULL
datosOriginalesNum$nameDest <- NULL 
(colSums(datosOriginalesNum) == 0)
#El dataset original tiene 6.354.407 registros NO fraudulentos y 8213 con fraude

#colSums(is.na(datosOriginales))

#MUESTREO ALEATORIO DE LOS DATOS CON NO FRAUDE 
#dividimos el dataset en dos: dataframe de fraude y dataframe no fraude
dfFraude <- subset(datosOriginalesNum, datosOriginalesNum$isFraud==1)
dfNOFraude <- subset(datosOriginalesNum, datosOriginalesNum$isFraud==0)
FilasNoFraude <- nrow(dfNOFraude)

#GUARDAR EL DATASET DE FRAUDE PARA IMPORTARLO FACILMENTE
write.csv(dfFraude, file="datosfraude.csv", row.names = F)

muestra <- sample(1:nrow(dfNOFraude), nrow(dfFraude)) #crea una muestra 
#con el mismo numero de registros del dataframe fraude

#DATASET NO Fraude 
muestre01 <- dfNOFraude[muestra,] #crea una muestra en no fraude  

#Primer conjunto de datos (BALANCEADOS 50 Y 50)

Conjunto1 <- rbind(dfFraude, muestre01) #unir la muestra y los fraudes
Conjunto1<-Conjunto1[sample(1:nrow(Conjunto1)), ] #desordena el dataset
Conjunto1$isFraud[Conjunto1$isFraud == "1"] <- "FRAUDE"
Conjunto1$isFraud[Conjunto1$isFraud == "0"] <- "NO FRAUDE"
Conjunto1 <- Conjunto1%>% select (-isFraud, isFraud) #Coloca isFraud en la ultima posicion

#GUARDO TAMBIEN EL PRIMER CONJUNTO DE DATOS EN MI CARPETA DE TRABAJO 
write.csv(Conjunto1, file="Conjunto1.csv", row.names = F)

# Segunda muestra
muestra2 <- sample(setdiff(1:nrow(dfNOFraude), muestre01), nrow(dfFraude))
muestre02 <- dfNOFraude[muestra2,]
Conjunto2 <- rbind(dfFraude, muestre02) #unir la muestra y los fraudes
Conjunto2<-Conjunto2[sample(1:nrow(Conjunto2)), ] #desordena el dataset
Conjunto2$isFraud[Conjunto2$isFraud == "1"] <- "FRAUDE"
Conjunto2$isFraud[Conjunto2$isFraud == "0"] <- "NO FRAUDE"
Conjunto2 <-Conjunto1%>% select (-isFraud, isFraud) #Coloca isFraud en la ultima posicion

#GUARDO TAMBIEN EL SEGUNDO CONJUNTO DE DATOS EN MI CARPETA DE TRABAJO 
write.csv(Conjunto2, file="Conjunto2.csv", row.names = F)

# Tercera muestra
muestra3 <- sample(setdiff(1:nrow(dfNOFraude), c(muestre01, muestra2)), nrow(dfFraude))
muestre03 <- dfNOFraude[muestra3,]
Conjunto3 <- rbind(dfFraude, muestre03) #unir la muestra y los fraudes
Conjunto3<-Conjunto3[sample(1:nrow(Conjunto3)), ] #desordena el dataset
Conjunto3$isFraud[Conjunto3$isFraud == "1"] <- "FRAUDE"
Conjunto3$isFraud[Conjunto3$isFraud == "0"] <- "NO FRAUDE"
Conjunto3 <- Conjunto3%>% select (-isFraud, isFraud) #Coloca isFraud en la ultima posicion

#GUARDO TAMBIEN EL TERCER CONJUNTO DE DATOS EN MI CARPETA DE TRABAJO 
write.csv(Conjunto3, file="Conjunto3.csv", row.names = F)

# Cuarta muestra
muestra4 <- sample(setdiff(1:nrow(dfNOFraude), c(muestre01, muestra2, muestra3)), nrow(dfFraude))
muestre04 <- dfNOFraude[muestra4,]
Conjunto4 <- rbind(dfFraude, muestre04) #unir la muestra y los fraudes
Conjunto4<-Conjunto4[sample(1:nrow(Conjunto4)), ] #desordena el dataset
Conjunto4$isFraud[Conjunto4$isFraud == "1"] <- "FRAUDE"
Conjunto4$isFraud[Conjunto4$isFraud == "0"] <- "NO FRAUDE"
Conjunto4 <- Conjunto4%>% select (-isFraud, isFraud) #Coloca isFraud en la ultima posicion

#GUARDO TAMBIEN EL CUARTO CONJUNTO DE DATOS EN MI CARPETA DE TRABAJO 
write.csv(Conjunto4, file="Conjunto4.csv", row.names = F)

# Quinta muestra
muestra5 <- sample(setdiff(1:nrow(dfNOFraude), c(muestre01, muestra2, muestra3, muestra4)), nrow(dfFraude))
muestre05 <- dfNOFraude[muestra5,]
Conjunto5 <- rbind(dfFraude, muestre05) #unir la muestra y los fraudes
Conjunto5<-Conjunto5[sample(1:nrow(Conjunto5)), ] #desordena el dataset
Conjunto5$isFraud[Conjunto5$isFraud == "1"] <- "FRAUDE"
Conjunto5$isFraud[Conjunto5$isFraud == "0"] <- "NO FRAUDE"
Conjunto5 <- Conjunto5%>% select (-isFraud, isFraud) #Coloca isFraud en la ultima posicion

#GUARDO TAMBIEN EL QUINTO CONJUNTO DE DATOS EN MI CARPETA DE TRABAJO 
write.csv(Conjunto5, file="Conjunto5.csv", row.names = F)

#graficas tipo y fraude
valores <- c(sum(Conjunto1$typeCASH_IN == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeCASH_OUT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeDEBIT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typePAYMENT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeTRANSFER == "1" & Conjunto1$isFraud == "FRAUDE"))

valores_no_fraude <- c(sum(Conjunto1$typeCASH_IN == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeCASH_OUT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeDEBIT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typePAYMENT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeTRANSFER == "1" & Conjunto1$isFraud == "NO FRAUDE"))

barplot(rbind(valores, valores_no_fraude),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = "topright"))


#en este espacio podemos explicar los datos anomalos, que aunque son anomalos no puedes ser 
#eliminados por el contexto del dataset
#seria necesario revisar y documentar el tema de los cuartiles y si es posible sacar una grafica
#de esa distribucion.

#grafico de dispersion entre la variable monto y saldo antiguo
plot(Conjunto1$amount, Conjunto1$oldbalanceOrg, 
     main = "Relación entre monto y saldo anterior", 
     xlab = "Monto de la transacción", 
     ylab = "Saldo anterior del cliente que inicia la transacción")

plot(Conjunto1$amount, Conjunto1$oldbalanceDest, 
     main = "Relación entre monto y saldo anterior", 
     xlab = "Monto de la transacción", 
     ylab = "Saldo anterior del cliente que recibe la transacción")

#grafico de lineas de "saldoantiguoorigen" y el "conjuntonuevoorigen" con el paso del tiempo
plot(type = "l", x =  Conjunto1$step, y = Conjunto1$oldbalanceOrg, 
     main = "Evolución del saldo del cliente que inicia la transacción", 
     xlab = "Paso de tiempo", 
     ylab = "Saldo del cliente")
lines(x = Conjunto1$step, y = Conjunto1$newbalanceOrig, col = "red")

#grafico de lineas de "saldoantiguodestino" y el "conjuntonuevodestino" con el paso del tiempo
plot(type = "l", x =  Conjunto1$step, y = Conjunto1$oldbalanceDest, 
     main = "Evolución del saldo del cliente que recibe la transacción", 
     xlab = "Paso de tiempo", 
     ylab = "Saldo del cliente que recibe")
lines(x = Conjunto1$step, y = Conjunto1$newbalanceDest, col = "red")

#REDES NEURONALES
# Tres capas ocultas cada una con 10 neuronas: hidden = c(10,10,10)
modeloRN1 <- neuralnet(isFraud~., data = dentrenamiento, 
                    hidden = c(10,10,10), rep=10, 
                    linear.output = T)
