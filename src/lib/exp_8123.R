source( "~/labo/src/lib/exp_lib.r" ) 
exp_start( "FE8123" )
exp_start( "TS9123" )
exp_start( "HT9123" )
exp_start( "FM9123" )
exp_start( "SC9123" )
exp_start( "KA9123" )

require("data.table")
require("tidyverse")

# Ensemble para predicción

# Enlisto los archivos
files <- list.files(path = "./exp/KA9123/", pattern = ".csv")

# Acomodo el directorio para poder leerlos
for (i in 1:length(files)) {
	files[i] <- paste0("./exp/KA9123/", 
		    files[i]) 
}
# Los leo en la lista d
d <- list()
for (i in 1:length(files)) { d[[i]] <- fread(files[i]) }

# # Ordeno cada uno por numero de cliente
for (i in 1:length(d)){
	d[[i]] <- arrange(d[[i]], numero_de_cliente)
}

# # Uno los tres conjuntos de datos de cada antidad de 1
# ka9000 <- cbind(d[[1]], d[[14]], d[[27]])
# ka9500 <- cbind(d[[2]], d[[15]], d[[28]])
# ka10000 <- cbind(d[[3]], d[[16]], d[[29]])
# ka10500 <- cbind(d[[4]], d[[17]], d[[30]])
# ka11000 <- cbind(d[[5]], d[[18]], d[[31]])
# ka11500 <- cbind(d[[6]], d[[19]], d[[32]])
# ka12000 <- cbind(d[[7]], d[[20]], d[[33]])
# ka12500 <- cbind(d[[8]], d[[21]], d[[34]])
# ka13000 <- cbind(d[[9]], d[[22]], d[[35]])
# ka13500 <- cbind(d[[10]], d[[23]], d[[36]])
# ka14000 <- cbind(d[[11]], d[[24]], d[[37]])
# ka14500 <- cbind(d[[12]], d[[25]], d[[38]])
# ka15000 <- cbind(d[[13]], d[[26]], d[[39]])
# 
# colnames(ka9000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka9500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka10000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka10500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka11000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka11500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka12000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka12500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka13000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka13500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka14000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka14500) <- c("numero_de_cliente", paste0("Predicted",1:5))
# colnames(ka15000) <- c("numero_de_cliente", paste0("Predicted",1:5))
# 
# ka9000 <- ka9000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka9500 <- ka9500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka10000 <- ka10000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka10500 <- ka10500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka11000 <- ka11000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka11500 <- ka11500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka12000 <- ka12000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka12500 <- ka12500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka13000 <- ka13000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka13500 <- ka13500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka14000 <- ka14000[, cant := Predicted1 + Predicted3 + Predicted5]
# ka14500 <- ka14500[, cant := Predicted1 + Predicted3 + Predicted5]
# ka15000 <- ka15000[, cant := Predicted1 + Predicted3 + Predicted5]
# 
# ka9000 <- ka9000 %>% select(numero_de_cliente, cant) 
# colnames(ka9000) <- c("numero_de_cliente","Predicted")
# ka <- ka[, ifese(Predicted < 39 & Predicted > 0)]


# Uno todos los conjuntos de datos
ka <- NA
for (i in 1:length(d)){ ka <- cbind(ka,d[[i]]) }
colnames(ka) <- c("ka", "numero_de_cliente", paste0("Predicted",1:77))
ka <- ka %>% select(c(2,seq(3,79,2)))

ka <- ka[, cant := Predicted1 + Predicted3 + Predicted5 + Predicted7 + Predicted9 + 
         	Predicted11 + Predicted13 + Predicted15 + Predicted17 + Predicted19 + 
         	Predicted21 + Predicted23 + Predicted25 + Predicted27 + Predicted29 + 
         	Predicted31 + Predicted33 + Predicted35 + Predicted37 + Predicted39 + 
         	Predicted41 + Predicted43 + Predicted45 + Predicted47 + Predicted49 + 
         	Predicted51 + Predicted53 + Predicted55 + Predicted57 + Predicted59 + 
         	Predicted61 + Predicted63 + Predicted65 + Predicted67 + Predicted69 + 
         	Predicted71 + Predicted73 + Predicted75 + Predicted77]

#table(ka9000[cant > 0,]$cant)
#cumsum(table(ka[Predicted > 0,]$Predicted))

colnames(ka) <- c("numero_de_cliente","Predicted")

count(ka[Predicted>27,])
count(ka[Predicted>15,])

setwd("./exp/KA9123")

ka27 <- ka
ka27$Predicted <- ka27[, .(fcase(Predicted > 27, 1,
	      Predicted < 28, 0))]
fwrite( ka27,
        file= "ka_ensemble_27.csv",
        sep= "," )


ka26 <- ka
ka26$Predicted <- ka26[, .(fcase(Predicted > 26, 1,
		   Predicted < 27, 0))]
fwrite( ka26,
        file= "ka_ensemble_26.csv",
        sep= "," )

ka25 <- ka
ka25$Predicted <- ka25[, .(fcase(Predicted > 25, 1,
		   Predicted < 26, 0))]
fwrite( ka25,
        file= "ka_ensemble_25.csv",
        sep= "," )

ka24 <- ka
ka24$Predicted <- ka24[, .(fcase(Predicted > 24, 1,
		   Predicted < 25, 0))]
fwrite( ka24,
        file= "ka_ensemble_24.csv",
        sep= "," )


ka23 <- ka
ka23$Predicted <- ka23[, .(fcase(Predicted > 23, 1,
		   Predicted < 24, 0))]
fwrite( ka23,
        file= "ka_ensemble_23.csv",
        sep= "," )

ka22 <- ka
ka22$Predicted <- ka22[, .(fcase(Predicted > 22, 1,
		   Predicted < 23, 0))]
fwrite( ka22,
        file= "ka_ensemble_22.csv",
        sep= "," )


ka21 <- ka
ka21$Predicted <- ka21[, .(fcase(Predicted > 21, 1,
		   Predicted < 22, 0))]
fwrite( ka21,
        file= "ka_ensemble_21.csv",
        sep= "," )


ka20 <- ka
ka20$Predicted <- ka20[, .(fcase(Predicted > 20, 1,
		   Predicted < 21, 0))]
fwrite( ka20,
        file= "ka_ensemble_20.csv",
        sep= "," )


ka19 <- ka
ka19$Predicted <- ka19[, .(fcase(Predicted > 19, 1,
		   Predicted < 20, 0))]
fwrite( ka19,
        file= "ka_ensemble_19.csv",
        sep= "," )


ka18 <- ka
ka18$Predicted <- ka18[, .(fcase(Predicted > 18, 1,
		   Predicted < 19, 0))]
fwrite( ka18,
        file= "ka_ensemble_18.csv",
        sep= "," )


ka17 <- ka
ka17$Predicted <- ka17[, .(fcase(Predicted > 17, 1,
		   Predicted < 18, 0))]
fwrite( ka17,
        file= "ka_ensemble_17.csv",
        sep= "," )


ka16 <- ka
ka16$Predicted <- ka16[, .(fcase(Predicted > 16, 1,
		   Predicted < 17, 0))]
fwrite( ka16,
        file= "ka_ensemble_16.csv",
        sep= "," )


ka15 <- ka
ka15$Predicted <- ka15[, .(fcase(Predicted > 15, 1,
		   Predicted < 16, 0))]
fwrite( ka15,
        file= "ka_ensemble_15.csv",
        sep= "," )


ka14 <- ka
ka14$Predicted <- ka14[, .(fcase(Predicted > 14, 1,
		   Predicted < 15, 0))]
fwrite( ka14,
        file= "ka_ensemble_14.csv",
        sep= "," )


ka13 <- ka
ka13$Predicted <- ka13[, .(fcase(Predicted > 13, 1,
		   Predicted < 14, 0))]
fwrite( ka13,
        file= "ka_ensemble_13.csv",
        sep= "," )


table(ka19$Predicted)
