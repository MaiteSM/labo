setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

ids <- dataset[numero_de_cliente %in% c(4580855,4605422,4607461,
		          4613352,4617524,4632622,4642732) ,]

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( ids,
        file= "algunos_ids.txt",
        sep= "\t" )
