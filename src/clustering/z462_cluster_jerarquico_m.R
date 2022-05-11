#este script necesita para correr en Google Cloud
# RAM     128 GB
# vCPU     8
# disco  256 GB

# PRUEBA 2: todos los BAJA+1, todos los BAJA+2 y el 10% de los CONTINUA del 202011

#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

install.packages("~/labo/randomForest_4.6-14.tar.gz", repos = NULL, type = "source")

require("data.table")
require("randomForest")
require("ranger")


setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

# Creo una uniforme para seleccionar aleatoriamente al 2% de los CONTINUA
# A los BAJA+1 y BAJA+2 les pongo 0 adrede para que queden todos seleccionados
dataset <- dataset[  foto_mes>=202011  & foto_mes<=202011, ]
uni <- runif(nrow(dataset))
dataset <- cbind(dataset, uni)
dataset <- dataset [clase_ternaria != "CONTINUA", uni := 0]
dataset <- dataset [uni < 0.10 , ]
gc()

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )

#Pruebo con todas las variables, por eso comento estas líneas que siguen
#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
# campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_trx",
#                      "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
#                      "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_trx", "Visa_msaldopesos",
#                      "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
#                      "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
#                      "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
#                      "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
#                      "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
#                      "mpagomiscuentas")

# Saco la clase_ternaria del dataset para ponerla como clase objetivo del clustering
clase <- droplevels(dataset$clase_ternaria)

dataset <- dataset[, ":=" (clase_ternaria = NULL)]


#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo1  <- randomForest( x= dataset, #[  , campos_buenos, with=FALSE ], 
                         y= clase, 
                         ntree= 300, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE,
	          nodesize = 50,
	          importance = TRUE)

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo1$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )

#primero, creo la carpeta donde van los resultados
dir.create( "./exp/", showWarnings= FALSE )
dir.create( "./exp/ST4612", showWarnings= FALSE )
setwd( "~/buckets/b1/exp/ST4612" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 5 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=4 & distintos <=5 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas2.txt",
        sep= "\t" )

# grabo los archivos con la importancia
importancia <- modelo1$importance
fwrite( importancia,
        file= "importancia2.txt",
        sep= "\t" )

importanciaSD <- modelo1$importanceSD
fwrite( importanciaSD,
        file= "importanciaSD2.txt",
        sep= "\t" )

