#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Cargo paquetes
require("data.table")
require("tidyverse")

#Establezco el Working Directory
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/" )

#lectura del dataset completo
dataset  <- fread("./datasets/paquete_premium_202011.csv")

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
  labs( x = "", y = "Consumo de tarjeta VISA") + 
  theme_grey()


g2 <- ggplot() +
	geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$ctarjeta_visa_trx), 
	          stat="identity", fill="skyblue", alpha=0.7) +
	geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
		    ymin=medias2$ctarjeta_visa_trx - (sd2$ctarjeta_visa_trx)/sqrt(medias2$n), 
		    ymax=medias2$ctarjeta_visa_trx + (sd2$ctarjeta_visa_trx)/sqrt(medias2$n)), 
		width=0.4, colour="orange", alpha=0.9, size=1.3) +
	coord_flip() +
	labs( x = "", y = "Transacciones con tarjeta VISA")



# CLUSTERING 3: TODOS LOS BAJA+2 DEL PERÍODO 201912 A 202011
clus3  <- fread("./labo/exp/ST4613/cluster_de_bajas.txt")
