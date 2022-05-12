library(data.table)
library(tidyverse)

#Establezco el Working Directory
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/" )

#lectura de los datasets
data  <- fread("./datasets/paquete_premium_202011.csv")

# mpasivos_margen: monto total de la ganancia que el banco ha obtenido
# por el dinero/inversiones ue el cliente tiene en el banco
ggplot(dataset1) +
	aes(x = mpasivos_margen, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mpasivos_margen", y = "Clase")

ggplot(dataset1) +
	aes(x = mpasivos_margen, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mpasivos_margen", y = "Clase") +
	scale_x_continuous(limits = c(0,10000))
	
as.data.frame(dataset1) %>% group_by(clase_ternaria) %>%
	summarise(media = mean(mpasivos_margen),
		sd = sd(mpasivos_margen),
		mna = median(mpasivos_margen),
		q1 = quantile(mpasivos_margen, 0.25),
		q3 = quantile(mpasivos_margen, 0.75))


# mcaja_ahorro: monto total de la caja de horro del paquete premium
ggplot(dataset1) +
	aes(x = mcaja_ahorro, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mcaja_ahorro", y = "Clase")

ggplot(dataset1) +
	aes(x = mcaja_ahorro, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mcaja_ahorro", y = "Clase") +
	scale_x_continuous(limits = c(0,500000))

as.data.frame(dataset1) %>% group_by(clase_ternaria) %>%
	summarise(media = mean(mcaja_ahorro),
		sd = sd(mcaja_ahorro),
		mna = median(mcaja_ahorro),
		q1 = quantile(mcaja_ahorro, 0.25),
		q3 = quantile(mcaja_ahorro, 0.75))

# mcuentas_saldo: saldo total de todas las cuentas del cliente, cajas de ahorr
# cuentas corrientes, pesos y dolares, convertifo a pesos
ggplot(dataset1) +
	aes(x = mcuentas_saldo, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mcuentas_saldo", y = "Clase")

ggplot(dataset1) +
	aes(x = mcuentas_saldo, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "mcuentas_saldo", y = "Clase") +
	scale_x_continuous(limits = c(0,150000))

as.data.frame(dataset1) %>% group_by(clase_ternaria) %>%
	summarise(media = mean(mcuentas_saldo),
		sd = sd(mcuentas_saldo),
		mna = median(mcuentas_saldo),
		q1 = quantile(mcuentas_saldo, 0.25),
		q3 = quantile(mcuentas_saldo, 0.75))


# ctrx_quarter: Cantidad de movimientos voluntarios en las cuentas 
# bancarias ( no tarjeta de credito ) que el cliente 
# realizó en los ultimos 90 dias
ggplot(dataset1) +
	aes(x = ctrx_quarter, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "ctrx_quarter", y = "Clase")

ggplot(dataset1) +
	aes(x = ctrx_quarter, y = clase_ternaria) +
	geom_boxplot(show.legend = F) +
	labs(x = "ctrx_quarter", y = "Clase") +
	scale_x_continuous(limits = c(0,500))

as.data.frame(dataset1) %>% group_by(clase_ternaria) %>%
	summarise(media = mean(ctrx_quarter),
	          sd = sd(ctrx_quarter),
	          mna = median(ctrx_quarter),
	          q1 = quantile(ctrx_quarter, 0.25),
	          q3 = quantile(ctrx_quarter, 0.75))

# Función riesgo: score de factores de riesgo. Un cliente se considera en riesgo
# si se encuentra por encima del tercer cuartil para una serie de variables
# para las cuales se ha verificado que los clientes de la clase CONTINUA poseen
# valores significativamente más altos que los clientes BAJA+1 y BAJA+2
# Clientes con 0 no se encuentran en riesgo en ninguna variable
# Clientes con valores distintos de 0 indican la cantidad de variables en las
# que se encuentran en riesgo

riesgo <- function(dataset, campos){
	
	lims <- dataset %>% filter(clase_ternaria == "BAJA+2") %>%
		
	
	dataset[ , paste0( campos, "_lim") := ifelse()]
	
	
	
	dataset[, riesgo := 0]
	cortes <- matrix(NA)
	
	# Calculo los límites
	for (i in 1:length(campos)){
		cortes[i,1] <- paste0(campos[i],"_lim")
		cortes[i,2] <- dataset[clase_ternaria == "BAJA+2",
			        quantile(campos[1], 0.75)]
	}
	
	for (vble in campos) { 
	# Agrego campos
	dataset[, ifelse(vble <= paste0(vble,"_lim"), riesgo := riesgo + 1)]
	}

	ReportarCampos( dataset )
}

campos <- c("ctrx_quarter")
vbles <- "ctrx_quarter"

riesgo(dataset1, c("ctrx_quarter"))

dataset <- dataset1

dataset1 %>% group_by(numero_de_cliente) %>%
	summarise(n = n())

table(dataset1$clase_ternaria)


ids <- fread("./src/FeatureEngineering/algunos_ids.txt")

# cambios en cantidad de productos
dataset1 <- dataset %>% arrange(numero_de_cliente, foto_mes)
clientes <- unique(dataset$numero_de_cliente)
productos <- dataset %>% group_by(numero_de_cliente, foto_mes) %>%
	summarise(max = max(cproductos), min = min(cproductos)) %>%
	select(numero_de_cliente, max, min, foto_mes)
productos <- dataset %>% select(numero_de_cliente, cproductos, foto_mes) %>%
	group_by(numero_de_cliente, foto_mes) %>%
	summarise(max = max(cproductos), min = min(cproductos)) %>%
	select(numero_de_cliente, max, min, foto_mes)

for (cliente in clientes){
	
}

dataset[, cambio_prod := min(cprod), by = numero_de_cliente]



dataset[ foto_mes==201806,  
         tcallcenter   :=  (dataset[ foto_mes == 201805, tcallcenter] +
         	    	dataset[foto_mes == 201807, tcallcenter]) / 2 ]

dataset %>% filter(foto_mes == 201806 | foto_mes == 201805 | foto_mes == 201807) %>% 
	select(numero_de_cliente, foto_mes, tcallcenter) %>%
	arrange(numero_de_cliente, foto_mes)


dataset1 <- dataset1 %>% arrange(numero_de_cliente)
dataset1[, reingreso := 0]
for (i in 2:nrow(dataset1)){
	if (dataset1[i, cliente_antiguedad] < dataset1[i-1, cliente_antiguedad]) {
		dataset1[i, reingreso := 1] }
}
for (i in 1:nrow(dataset1)){
	if (dataset1[i, reingreso] == 1) {
		num <- dataset1[i, numero_de_cliente]
	}
	for (j in i:nrow(dataset1)) {
		if (dataset1[j, numero_de_cliente] == num){
			dataset1[j, reingreso] = 1
		}
	}
}


dataset1 %>% group_by(numero_de_cliente, foto_mes, cliente_antiguedad, reingreso) %>%
	arrange(numero_de_cliente) %>% summarise(n = 1) 




