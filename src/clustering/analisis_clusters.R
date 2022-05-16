#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Cargo paquetes
require("data.table")
require("tidyverse")
require("gridExtra")

#Establezco el Working Directory
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/" )

#lectura del dataset completo
dataset  <- fread("./datasets/paquete_premium_202011.csv")

prop.table(table(dataset$clase_ternaria))*100

table(dataset$clase_ternaria)[1]*60000

# CLUSTERING 1: TODOS LOS BAJA+1, TODOS LOS BAJA+2 Y EL 2% DE LOS CONTINUA
# DEL PERÍODO 202001 A 202011
# clus1  <- fread("./labo/exp/ST4611/cluster_de_bajas1.txt")
# 
# clus1$clase
# #Veo variables con mayor variabilidad entre medias para
# medias1 <- clus1[, lapply(.SD, function(x){mean(x)}), 
#               by= cluster2]
# cv1 <- medias1[, lapply(.SD, function(x){sd(x)/abs(mean(x))})]
# sort(cv1)


# CLUSTERING 2: TODOS LOS BAJA+1, TODOS LOS BAJA+2 Y EL 10% DE LOS CONTINUA
# DEL MES 202011
clus2  <- fread("./labo/exp/ST4612/cluster_de_bajas2.txt")

d1 <- dataset %>% select(numero_de_cliente, clase_ternaria)
clus2 <- d1[clus2,,on=c("numero_de_cliente")]

# Composición de los clusters según la probabilidad de baja
table(clus2$cluster2, clus2$clase_ternaria)

# Creo cluster3 para separar los CONTINUA de los BAJA y obtener 
# medidas resumen par estos dos clusters
clus2[, cluster3 := 1]
clus2[ cluster2 == 4, cluster3 := 2]

# Importancia
impo2 <- fread("./labo/exp/ST4612/importancia2.txt")
impo2[, importancia := abs(MeanDecreaseAccuracy)]
impo2$vble <- colnames(dataset)
vbles_clus <- impo2 %>% select(vble, importancia) %>% 
	filter(rank(importancia)>140) %>% arrange(desc(importancia))

medias2 <- clus2[, lapply(.SD, function(x){mean(x)}),
              by= cluster3]
medias2[,clase_ternaria := 0]
medias2[cluster3 == 2, clase_ternaria := (511+648)/(511+648+2)]
medias2[ cluster3 == 1, n := 5886 + 1770 + 6817 + 1377]
medias2[ cluster3 == 2, n := 511 + 648 + 2]

sd2 <- clus2[, lapply(.SD, function(x){sd(x)}),
	  by= cluster3]

medias2 %>% select(vbles_clus$vble)

ratios2 <- medias2[cluster3 == 1,] / medias2[cluster3 == 2,]

g1 <- ggplot() +
  geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$mtarjeta_visa_consumo), 
            stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
  	    ymin=medias2$mtarjeta_visa_consumo - (sd2$mtarjeta_visa_consumo)/sqrt(medias2$n), 
  	    ymax=medias2$mtarjeta_visa_consumo + (sd2$mtarjeta_visa_consumo)/sqrt(medias2$n)), 
  	width=0.4, colour="orange", alpha=0.9, size=1.3) +
  coord_flip() +
  labs( x = "", title = "Consumo de tarjeta VISA", y = "Pesos ($)") + 
  theme_classic()
ratios2$mtarjeta_visa_consumo

g2 <- ggplot() +
	geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$ctarjeta_visa_trx), 
	          stat="identity", fill="skyblue", alpha=0.7) +
	geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
		    ymin=medias2$ctarjeta_visa_trx - (sd2$ctarjeta_visa_trx)/sqrt(medias2$n), 
		    ymax=medias2$ctarjeta_visa_trx + (sd2$ctarjeta_visa_trx)/sqrt(medias2$n)), 
		width=0.4, colour="orange", alpha=0.9, size=1.3) +
	scale_y_continuous(breaks = seq(0,14,2), labels = seq(0,14,2)) +
	coord_flip() +
	labs( x = "", title = "Transacciones con tarjeta VISA", y = "Cantidad") + 
	theme_classic()
ratios2$ctarjeta_visa_trx
	
g3 <- ggplot() +
	geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$ctrx_quarter), 
	          stat="identity", fill="skyblue", alpha=0.7) +
	geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
		    ymin=medias2$ctrx_quarter - (sd2$ctrx_quarter)/sqrt(medias2$n), 
		    ymax=medias2$ctrx_quarter + (sd2$ctrx_quarter)/sqrt(medias2$n)), 
		width=0.4, colour="orange", alpha=0.9, size=1.3) +
	#scale_y_continuous(breaks = seq(0,14,2), labels = seq(0,14,2)) +
	coord_flip() +
	labs( x = "", y = "Cantidad",
	      title = "Movimientos voluntarios en las cuentas bancarias") + 
	theme_classic()
ratios2$ctrx_quarter

grid.arrange(g1, g2, g3, ncol = 1)

ratios2$Visa_msaldototal
ratios2$Visa_mconsumospesos
ratios2$Visa_mconsumototal 

ratios2$mcomisiones_otras

ratios2 <- t(rbind(colnames(ratios2), t(ratios2)))


# CLUSTERING 3: TODOS LOS BAJA+2 DEL PERÍODO 201912 A 202011
clus3  <- fread("./labo/exp/ST4613/cluster_de_bajas.txt")










# APARTADO PARA GOOGLE CLOUD
require("data.table")

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

#evolucion de la cantidad de clientes
table(dataset$foto_mes, dataset$clase_ternaria)

