tabulador="	"
exp_name=ZZ8125
echo "experiment	timestamp	event"  >  log.txt 
fecha0=$(date +"%Y%m%d %H%M%S") 
echo "$exp_name""$tabulador""$fecha0""$tabulador""SH_START" >> log.txt 
Rscript --vanilla ~/labo/src/completo/z991_ZZ_lightgbm.r  ZZ8125  2>&1 | tee outfile 
fecha1=$(date +"%Y%m%d %H%M%S") 
echo "$exp_name""$tabulador""$fecha1""$tabulador""SH_END" >> log.txt 
find ./ ! -name "*.gz" ! -name . -exec cp -prt /media/expshared/mai_sanmartin/exp/ZZ8125/  {} +  

#suicidio
export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') 
export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') 
gcloud --quiet compute instances delete $NAME --zone=$ZONE 
