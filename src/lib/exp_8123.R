source( "~/labo/src/lib/exp_lib.r" ) 
exp_start( "FE8123" )
exp_start( "TS9123" )
exp_start( "HT9123" )
exp_start( "FM9123" )
exp_start( "SC9123" )
exp_start( "KA9123" )

require("data.table")

# Ensemble para predicción

# Enlisto los archivos
files <- list.files(path = ".exp/KA8123/")

# Acomodo el directorio para poder leerlos
for (i in 1:length(files)) {
	files[i] <- paste0(".exp/KA8123/", 
		     files[i])
}

d <- list()
for (i in 1:length(files)) {
	d[[i]] <- fread(files[i])
}
