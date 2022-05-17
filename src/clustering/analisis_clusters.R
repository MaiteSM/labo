#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Cargo paquetes
require("data.table")
require("tidyverse")
require("gridExtra")

options(scipen = 9999)

#Establezco el Working Directory
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/" )

#lectura del dataset completo
dataset  <- fread("./datasets/paquete_premium_202011.csv")

# INTRODUCCION
t1 <- fread("./labo/src/clustering/tabla1.txt")
t1 <- cbind(t1[36:70, 3],t1[71:105, 3],t1[106:140, 3])
colnames(t1) <- c("BAJA+1", "BAJA+2", "CONTINUA")

t1$total <- t1$`BAJA+1` + t1$`BAJA+2` + t1$CONTINUA
t1$foto_mes <- as.character(c(seq(201801, 201812,1),seq(201901, 201912,1),seq(202001, 202011,1)))

ggplot(t1, aes(x=foto_mes, y=total)) + 
	geom_bar(stat = "identity", fill = "orange") +
	theme_classic() +
	theme(axis.text.x = element_text( 
		angle = 90, )) + 
	labs(y = "Cantidad de clientes", x = "")

t1[.N, total]

t1 %>% mutate(prop = `BAJA+1`/total*100) %>%
	ggplot(aes(x=foto_mes, y= prop)) + 
	geom_bar(stat = "identity", fill = "orange") +
	theme_classic() +
	theme(axis.text.x = element_text( 
		angle = 90, )) + 
	labs(y = "Cantidad de bajas", x = "")

t1 %>% mutate(prop = `BAJA+1`/total) %>% select(prop) %>%
	summarise(media = mean(prop)*100)

t1 %>% mutate(perd = `BAJA+1`*120000/1000000) %>%
	ggplot(aes(x=foto_mes, y= perd)) + 
	geom_bar(stat = "identity", fill = "orange") +
	theme_classic() +
	theme(axis.text.x = element_text( 
		angle = 90, )) + 
	labs(y = "Ganancia perdida (en millones de $)", x = "")

t1 %>% mutate(perd = `BAJA+1`*120000/1000000) %>%
	summarise(media = mean(perd))
		


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
            stat="identity", fill="orange", alpha=0.7) +
  geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
  	    ymin=medias2$mtarjeta_visa_consumo - (sd2$mtarjeta_visa_consumo)/sqrt(medias2$n), 
  	    ymax=medias2$mtarjeta_visa_consumo + (sd2$mtarjeta_visa_consumo)/sqrt(medias2$n)), 
  	width=0.4, colour="red", alpha=0.9, size=1.3) +
  coord_flip() +
  labs( x = "", title = "Consumo de tarjeta VISA", y = "Pesos ($)") + 
  theme_classic()
ratios2$mtarjeta_visa_consumo

g2 <- ggplot() +
	geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$ctarjeta_visa_trx), 
	          stat="identity", fill="orange", alpha=0.7) +
	geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
		    ymin=medias2$ctarjeta_visa_trx - (sd2$ctarjeta_visa_trx)/sqrt(medias2$n), 
		    ymax=medias2$ctarjeta_visa_trx + (sd2$ctarjeta_visa_trx)/sqrt(medias2$n)), 
		width=0.4, colour="red", alpha=0.9, size=1.3) +
	scale_y_continuous(breaks = seq(0,14,2), labels = seq(0,14,2)) +
	coord_flip() +
	labs( x = "", title = "Transacciones con tarjeta VISA", y = "Cantidad") + 
	theme_classic()
ratios2$ctarjeta_visa_trx
	
g3 <- ggplot() +
	geom_bar( aes(x=c("CONTINUA","BAJA"), y=medias2$ctrx_quarter), 
	          stat="identity", fill="orange", alpha=0.7) +
	geom_errorbar( aes(x=c("CONTINUA","BAJA"), 
		    ymin=medias2$ctrx_quarter - (sd2$ctrx_quarter)/sqrt(medias2$n), 
		    ymax=medias2$ctrx_quarter + (sd2$ctrx_quarter)/sqrt(medias2$n)), 
		width=0.4, colour="red", alpha=0.9, size=1.3) +
	#scale_y_continuous(breaks = seq(0,14,2), labels = seq(0,14,2)) +
	coord_flip() +
	labs( x = "", y = "Cantidad",
	      title = "Movimientos voluntarios en las cuentas bancarias") + 
	theme_classic()
ratios2$ctrx_quarter

grid.arrange(g1, g2, g3, ncol = 1)

ratios2



# CLUSTERING 3: TODOS LOS BAJA+2 DEL PERÍODO 201912 A 202011
#clus3  <- fread("./labo/exp/ST4613/cluster_de_bajas.txt")

# clus3 %>% group_by(cluster2) %>% summarise(n = n())
# 
# # Importancia
# impo3 <- fread("./labo/exp/ST4613/importancia2.txt")
# impo3[, importancia := abs(MeanDecreaseAccuracy)]
# impo3$vble <- colnames(dataset)
# vbles_clus3 <- impo3 %>% select(vble, importancia) %>% 
# 	filter(rank(importancia)>140) %>% arrange(desc(importancia))
# 
# medias3 <- clus3[, lapply(.SD, function(x){mean(x)}), by = cluster2]
# 
# medias3 %>% select(vbles_clus3$vble) %>% summarise(function(x){cv = sd(x)/mean(x)})
# 
# cv3 <- medias3[, lapply(.SD, function(x){sd(x)/abs(mean(x))})]
# cv3 <- t(cv3) 
# colnames(c) <- "cv"

clus3  <- fread("./labo/exp/ST4614/cluster_de_bajas3.txt")

clus4  <- fread("./labo/exp/ST4614/cluster_de_bajas4.txt")

clus5  <- fread("./labo/exp/ST4614/cluster_de_bajas5.txt")

clus6  <- fread("./labo/exp/ST4614/cluster_de_bajas6.txt")

medias3 <- dataset[, lapply(.SD, function(x){mean(x)}), by = cluster2]	
# Ejes a analizar
# RENTABILIDAD: mrentabilidad_annual, mrentabilidad, mpasivos_margen, mactivos_margen
g1 <-ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mrentabilidad_annual), 
	          stat="identity", fill="orange", alpha=0.7)+
	#scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g2 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mrentabilidad), 
	          stat="identity", fill="orange", alpha=0.7)+
	#scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()

g3 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mpasivos_margen), 
	          stat="identity", fill="orange", alpha=0.7)+
	#scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()

g4 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mactivos_margen), 
	          stat="identity", fill="orange", alpha=0.7)+
	#scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()

# COMISIONES: mcomisiones, mcomisiones_mantenimiento, mcomisiones_otras, 
g5 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcomisiones), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g6 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcomisiones_otras), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g7 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcomisiones_mantenimiento), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
# SALDO en cuenta: mcuentas_saldo, mcaja_ahorro
g8 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcuentas_saldo), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g9 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcaja_ahorro), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
# SERVICIOS DIGITALES: chomebanking_trx, mtransferencias_recibidas, mtransferencias_emitidas, 
#  ccuenta_debitos_automaticos, mcuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos,
#  mtarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, 
#  mttarjeta_master_debitos_automaticos, cpagodeservicios, mpagodeservicios
#  cpagomiscuentas, mpagomiscuentas
g10 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=chomebanking_trx), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g11 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtransferencias_recibidas), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g12 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtransferencias_emitidas), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g13 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ccuenta_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g14 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcuenta_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g15 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctarjeta_visa_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g16 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtarjeta_visa_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g17 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctarjeta_master_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g18 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mttarjeta_master_debitos_automaticos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g19 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=cpagodeservicios), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g20 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mpagodeservicios), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g21 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=cpagomiscuentas), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g22 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mpagomiscuentas), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
# MOVIMIENTO EN CUENTAS: ctrx_quarter
g23 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctrx_quarter), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
# ACREDITACION HABERES: cpayroll_trx, mpayroll, mpayroll2, cpayroll2_trx

g24 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=cpayroll_trx), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g25 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mpayroll), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g26 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mpayroll2), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g27 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=cpayroll2_trx), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()

# TARJETA VISA: Visa_msaldototal, Visa_msaldopesos, mtarjeta_visa_consumo, 
#  Visa_mconsumospesos, ctarjeta_visa_trx, Visa_mconsumototal,
#  Visa_cconsumos, Visa_mpagospesos
g28 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_msaldototal), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g29 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_msaldopesos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g30 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtarjeta_visa_consumo), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g31 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_mconsumospesos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g32 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctarjeta_visa_trx), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g33 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_mconsumototal), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g34 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_cconsumos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g35 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=Visa_mpagospesos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()

# Descuentos: ccajeros_propios_descuentos, mcajeros_propios_descuentos,
#   ctarjeta_visa_descuentos, mtarjeta_visa_descuentos, ctarjeta_master_descuentos,
#   mtarjeta_master_descuentos
g36 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ccajeros_propios_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g37 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mcajeros_propios_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g38 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctarjeta_visa_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g39 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtarjeta_visa_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g40 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=ctarjeta_master_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()
g41 <- ggplot(medias3) +
	geom_bar( aes(x=cluster2, y=mtarjeta_master_descuentos), 
	          stat="identity", fill="orange", alpha=0.7)+
	scale_x_continuous(breaks = 1:10, labels = 1:10) +
	coord_flip() +
	theme_classic()







# APARTADO PARA GOOGLE CLOUD
require("data.table")

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

#evolucion de la cantidad de clientes
t1 <- table(dataset$foto_mes, dataset$clase_ternaria)
fwrite( t1,
        file= "tabla1.txt",
        sep= "\t" )

