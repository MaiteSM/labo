
#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/" )  #cambiar por la carpeta local

#leo el dataset
clus3  <- fread("./labo/exp/ST4614/cluster_de_bajas3.txt")

#me quedo SOLO con los del cluster 1 (redituables)
clus3  <- clus3[  cluster2 == 1, ]
gc()

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
clus3  <- na.roughfix( clus3 )


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



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= clus3, #[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE,
	          importance = TRUE)

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#primero, creo la carpeta donde van los resultados
dir.create( "./labo/", showWarnings= FALSE )
dir.create( "./labo/exp/", showWarnings= FALSE )
dir.create( "./labo/exp/ST4615", showWarnings= FALSE )
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/labo/exp/ST4615" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()



#genero 4 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=3 & distintos <=4 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  clus3[  , cluster2 := NULL ]
  clus3[  , cluster2 := rf.cluster ]

  distintos  <- nrow( clus3[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  clus3,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

clus3[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( clus3,
        file= "cluster_de_bajas_redit4.txt",
        sep= "\t" )


#genero 5 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=4 & distintos <=5 ) )
{
	h <- h - 1 
	rf.cluster  <- cutree( hclust.rf, h)
	
	clus3[  , cluster2 := NULL ]
	clus3[  , cluster2 := rf.cluster ]
	
	distintos  <- nrow( clus3[  , .N,  cluster2 ] )
	cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

clus3[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( clus3,
        file= "cluster_de_bajas_redit5.txt",
        sep= "\t" )


#genero 6 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=5 & distintos <=6 ) )
{
	h <- h - 1 
	rf.cluster  <- cutree( hclust.rf, h)
	
	clus3[  , cluster2 := NULL ]
	clus3[  , cluster2 := rf.cluster ]
	
	distintos  <- nrow( clus3[  , .N,  cluster2 ] )
	cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

clus3[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( clus3,
        file= "cluster_de_bajas_redit6.txt",
        sep= "\t" )
