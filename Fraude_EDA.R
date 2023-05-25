Conjunto1 <- read.csv("Conjunto1.csv")
Conjunto2 <- read.csv("Conjunto2.csv")
Conjunto3 <- read.csv("Conjunto3.csv")
Conjunto4 <- read.csv("Conjunto4.csv")
Conjunto5 <- read.csv("Conjunto5.csv")

#GRAFICAS DE BARRAS RELACION TIPO Y ISFRAUD
#GRAFICAS CONJUNTO 1 

valores_fraude <- c(sum(Conjunto1$typeCASH_IN == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeCASH_OUT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeDEBIT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typePAYMENT == "1" & Conjunto1$isFraud == "FRAUDE"),
             sum(Conjunto1$typeTRANSFER == "1" & Conjunto1$isFraud == "FRAUDE"))

valores_no_fraude <- c(sum(Conjunto1$typeCASH_IN == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeCASH_OUT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeDEBIT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typePAYMENT == "1" & Conjunto1$isFraud == "NO FRAUDE"),
                       sum(Conjunto1$typeTRANSFER == "1" & Conjunto1$isFraud == "NO FRAUDE"))

barplot(rbind(valores_fraude, valores_no_fraude),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = 10.5, y=5000.5))

#GRAFICAS CONJUNTO 2

valores_fraude2 <- c(sum(Conjunto2$typeCASH_IN == "1" & Conjunto2$isFraud == "FRAUDE"),
                    sum(Conjunto2$typeCASH_OUT == "1" & Conjunto2$isFraud == "FRAUDE"),
                    sum(Conjunto2$typeDEBIT == "1" & Conjunto2$isFraud == "FRAUDE"),
                    sum(Conjunto2$typePAYMENT == "1" & Conjunto2$isFraud == "FRAUDE"),
                    sum(Conjunto2$typeTRANSFER == "1" & Conjunto2$isFraud == "FRAUDE"))

valores_no_fraude2 <- c(sum(Conjunto2$typeCASH_IN == "1" & Conjunto2$isFraud == "NO FRAUDE"),
                       sum(Conjunto2$typeCASH_OUT == "1" & Conjunto2$isFraud == "NO FRAUDE"),
                       sum(Conjunto2$typeDEBIT == "1" & Conjunto2$isFraud == "NO FRAUDE"),
                       sum(Conjunto2$typePAYMENT == "1" & Conjunto2$isFraud == "NO FRAUDE"),
                       sum(Conjunto2$typeTRANSFER == "1" & Conjunto2$isFraud == "NO FRAUDE"))

barplot(rbind(valores_fraude2, valores_no_fraude2),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = 10.5, y=5000.5))

#CONJUNTO 3

valores_fraude3 <- c(sum(Conjunto3$typeCASH_IN == "1" & Conjunto3$isFraud == "FRAUDE"),
                     sum(Conjunto3$typeCASH_OUT == "1" & Conjunto3$isFraud == "FRAUDE"),
                     sum(Conjunto3$typeDEBIT == "1" & Conjunto3$isFraud == "FRAUDE"),
                     sum(Conjunto3$typePAYMENT == "1" & Conjunto3$isFraud == "FRAUDE"),
                     sum(Conjunto3$typeTRANSFER == "1" & Conjunto3$isFraud == "FRAUDE"))

valores_no_fraude3 <- c(sum(Conjunto3$typeCASH_IN == "1" & Conjunto3$isFraud == "NO FRAUDE"),
                        sum(Conjunto3$typeCASH_OUT == "1" & Conjunto3$isFraud == "NO FRAUDE"),
                        sum(Conjunto3$typeDEBIT == "1" & Conjunto3$isFraud == "NO FRAUDE"),
                        sum(Conjunto3$typePAYMENT == "1" & Conjunto3$isFraud == "NO FRAUDE"),
                        sum(Conjunto3$typeTRANSFER == "1" & Conjunto3$isFraud == "NO FRAUDE"))

barplot(rbind(valores_fraude3, valores_no_fraude3),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = 10.5, y=5000.5))

#CONJUNTO 4

valores_fraude4 <- c(sum(Conjunto4$typeCASH_IN == "1" & Conjunto4$isFraud == "FRAUDE"),
                     sum(Conjunto4$typeCASH_OUT == "1" & Conjunto4$isFraud == "FRAUDE"),
                     sum(Conjunto4$typeDEBIT == "1" & Conjunto4$isFraud == "FRAUDE"),
                     sum(Conjunto4$typePAYMENT == "1" & Conjunto4$isFraud == "FRAUDE"),
                     sum(Conjunto4$typeTRANSFER == "1" & Conjunto4$isFraud == "FRAUDE"))

valores_no_fraude4 <- c(sum(Conjunto4$typeCASH_IN == "1" & Conjunto4$isFraud == "NO FRAUDE"),
                        sum(Conjunto4$typeCASH_OUT == "1" & Conjunto4$isFraud == "NO FRAUDE"),
                        sum(Conjunto4$typeDEBIT == "1" & Conjunto4$isFraud == "NO FRAUDE"),
                        sum(Conjunto4$typePAYMENT == "1" & Conjunto4$isFraud == "NO FRAUDE"),
                        sum(Conjunto4$typeTRANSFER == "1" & Conjunto4$isFraud == "NO FRAUDE"))

barplot(rbind(valores_fraude4, valores_no_fraude4),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = 10.5, y=5000.5))

#CONJUNTO 5 

valores_fraude5 <- c(sum(Conjunto5$typeCASH_IN == "1" & Conjunto5$isFraud == "FRAUDE"),
                     sum(Conjunto5$typeCASH_OUT == "1" & Conjunto5$isFraud == "FRAUDE"),
                     sum(Conjunto5$typeDEBIT == "1" & Conjunto5$isFraud == "FRAUDE"),
                     sum(Conjunto5$typePAYMENT == "1" & Conjunto5$isFraud == "FRAUDE"),
                     sum(Conjunto5$typeTRANSFER == "1" & Conjunto5$isFraud == "FRAUDE"))

valores_no_fraude5 <- c(sum(Conjunto5$typeCASH_IN == "1" & Conjunto5$isFraud == "NO FRAUDE"),
                        sum(Conjunto5$typeCASH_OUT == "1" & Conjunto5$isFraud == "NO FRAUDE"),
                        sum(Conjunto5$typeDEBIT == "1" & Conjunto5$isFraud == "NO FRAUDE"),
                        sum(Conjunto5$typePAYMENT == "1" & Conjunto5$isFraud == "NO FRAUDE"),
                        sum(Conjunto5$typeTRANSFER == "1" & Conjunto5$isFraud == "NO FRAUDE"))

barplot(rbind(valores_fraude5, valores_no_fraude5),
        beside = TRUE,
        names.arg = c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER"),
        xlab = "Tipo de transacción",
        ylab = "Número de transacciones",
        col = c("red", "lightblue"),
        legend.text = c("Fraudulentas", "No fraudulentas"),
        args.legend = list(x = 10.5, y=5000.5))

#FUNCION PARA VISUALIZAR MIN Y MAXIMO DE LAS VARIABLES OLDBALANCEORG, NEWBALANCEORG
#OLDBALANCEDEST Y NEWBALANCEDEST

min_max_balances <- function(datos) {
  min_oldbalance <- min(datos$oldbalanceOrg)
  max_oldbalance <- max(datos$oldbalanceOrg)
  min_newbalance <- min(datos$newbalanceOrig)
  max_newbalance <- max(datos$newbalanceOrig)
  
  return(list(min_oldbalance, max_oldbalance, min_newbalance, max_newbalance))
}

conjuntos <- list(Conjunto1, Conjunto2, Conjunto3, Conjunto4, Conjunto5)

for (i in 1:length(conjuntos)) {
  print(paste("Conjunto", i))
  print(min_max_balances(conjuntos[[i]]))
}


#FUNCION PARA VISUALIZAR MIN Y MAXIMO DE LA VARIABLE MONTO DE CADA CONJUNTO DE DATOS
min_max_amount <- function(data) {
  return(c(min(data$amount), max(data$amount)))
}
min_max_amount(Conjunto1)
min_max_amount(Conjunto2)
min_max_amount(Conjunto3)
min_max_amount(Conjunto4)
min_max_amount(Conjunto5)

#GRAFICA QUE RELACIONE step con isfraud conjunto 1
ggplot(data = Conjunto1, aes(x = step, fill = isFraud)) + 
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("red", "blue")) + 
  labs(x = "Step (hora)", y = "Frecuencia", fill = "Clase") + 
  theme_bw()

#GRAFICA QUE RELACIONE step con isfraud conjunto 2
ggplot(data = Conjunto2, aes(x = step, fill = isFraud)) + 
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("red", "blue")) + 
  labs(x = "Step (hora)", y = "Frecuencia", fill = "Clase") + 
  theme_bw()

#GRAFICA QUE RELACIONE step con isfraud conjunto 3
ggplot(data = Conjunto3, aes(x = step, fill = isFraud)) + 
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("red", "blue")) + 
  labs(x = "Step (hora)", y = "Frecuencia", fill = "Clase") + 
  theme_bw()

#GRAFICA QUE RELACIONE step con isfraud conjunto 4
ggplot(data = Conjunto4, aes(x = step, fill = isFraud)) + 
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("red", "blue")) + 
  labs(x = "Step (hora)", y = "Frecuencia", fill = "Clase") + 
  theme_bw()

#GRAFICA QUE RELACIONE step con isfraud conjunto 5
ggplot(data = Conjunto5, aes(x = step, fill = isFraud)) + 
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = c("red", "blue")) + 
  labs(x = "Step (hora)", y = "Frecuencia", fill = "Clase") + 
  theme_bw()

#DIAGRAMA DE DISPERSION CONJUNTO 1, RELACION MONTO, TIEMPO E ISFRAUD
ggplot(data = Conjunto1, aes(x = step, y = amount, color = isFraud)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tiempo", y = "Monto", color = "Fraude") +
  theme_bw()

#DIAGRAMA DE DISPERSION CONJUNTO 2, RELACION MONTO, TIEMPO E ISFRAUD
ggplot(data = Conjunto2, aes(x = step, y = amount, color = isFraud)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tiempo", y = "Monto", color = "Fraude") +
  theme_bw()

#DIAGRAMA DE DISPERSION CONJUNTO 3, RELACION MONTO, TIEMPO E ISFRAUD
ggplot(data = Conjunto3, aes(x = step, y = amount, color = isFraud)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tiempo", y = "Monto", color = "Fraude") +
  theme_bw()

#DIAGRAMA DE DISPERSION CONJUNTO 4, RELACION MONTO, TIEMPO E ISFRAUD
ggplot(data = Conjunto4, aes(x = step, y = amount, color = isFraud)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tiempo", y = "Monto", color = "Fraude") +
  theme_bw()

#DIAGRAMA DE DISPERSION CONJUNTO 5, RELACION MONTO, TIEMPO E ISFRAUD
ggplot(data = Conjunto5, aes(x = step, y = amount, color = isFraud)) +
  geom_point(alpha = 0.5) +
  labs(x = "Tiempo", y = "Monto", color = "Fraude") +
  theme_bw()


#DIAGRAMA DE CAJA Y BIGOTES 
boxplot(Conjunto5$amount,col="burlywood1",
        main = "Caja y bigotes de la variable monto Conjunto 5",
        horizontal=TRUE,xlab = "Monto")

#
muestras <- datosOriginales %>%
  distinct(nameOrig) %>%
  sample_n(100)  # Ajusta el número de muestras según tus necesidades

dataset_filtrado <- datosOriginales %>%
  filter(nameOrig %in% muestras$nameOrig)

#grafico de lineas de dataset filtrado
ggplot(data = dataset_filtrado, aes(x = step)) +
  geom_line(aes(y = amount), color = "blue", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = oldbalanceOrg), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = newbalanceOrig), color = "green", linetype = "dotted", linewidth = 1) +
  labs(x = "Tiempo (paso)", y = "Valor") +
  theme_bw()




