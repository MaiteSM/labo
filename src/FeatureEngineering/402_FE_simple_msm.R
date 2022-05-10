#Feature Engineering
#creo nuevas variables dentro del mismo mes

#este script se muestra como esqueleto para que los alumnos 
# agreguen sus propias variables
#ya sea basados en la teoria economica  o en el salvaje empiricismo
# "No es que la nueva variable creada, que funciona, no tenga sentido, 
#lo que sucede es que yo estoy siendo capaz de encontrarselo en este momento"

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



EnriquecerDataset  <- function( dataset , arch_destino )
{

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status

  dataset[ , mv_status01 := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02 := Master_status +  Visa_status ]
  dataset[ , mv_status03 := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04 := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05 := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06 := ifelse( is.na(Visa_status), 
                                    ifelse( is.na(Master_status), 10, Master_status), 
                                    Visa_status) ]

  dataset[ , mv_status07 := ifelse( is.na(Master_status), 
                                    ifelse( is.na(Visa_status), 10, Visa_status), 
                                    Master_status) ]


  #combino MasterCard y Visa , teniendo en cuenta los NA
  dataset[ , mv_mfinanciacion_limite  := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento          := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora           := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal           := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos           := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares         := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos        := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares      := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra         := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos        := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares      := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre        := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado               := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos           := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares         := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta             := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal         := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos             := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo    := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo           := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]


  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra := Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra   := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal          := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos          := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2         := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares        := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2       := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos       := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares     := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos       := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares     := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado              := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos          := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares        := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal        := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo          := mv_mpagominimo  / mv_mlimitecompra ]

  # cantidad de productos que tiene el cliente / ganancia del banco por inversiones
  dataset[, invers_x_prod := cproductos / mpasivos_margen]
  
  # cantidad de productos que tiene el cliente / ganancia del banco por intereses
  dataset[, inter_x_prod := cproductos / mactivos_margen]
  
  # cantidad de tarjetas master / ganancia del banco por inversiones
  dataset[, invers_x_master := ctarjeta_master / mpasivos_margen]

  # cantidad de tarjetas visa / ganancia del banco por inversiones
  dataset[, invers_x_visa := ctarjeta_visa / mpasivos_margen]
  
  # cantidad de tarjetas / ganancia del banco por inversiones
  dataset[, invers_x_tarje := (ctarjeta_visa + ctarjeta_master) / mpasivos_margen]
  
  # cantidad de tarjetas / ganancia del banco por intereses
  dataset[, inter_x_tarje := (ctarjeta_visa + ctarjeta_master) / mactivos_margen]
  
  # Cantidad de acreditaciones de haberes: 0 o más (de numérica a dicotómica)
  dataset[ , cpayroll_trx_dico := ifelse( cpayroll_trx == 0, 0, 1)]
  
  # antiguedad medida en años enteros
  dataset[, cliente_antig_anio := trunc(cliente_antiguedad / 12)]
  
  # todas las que reflejen montos, pasadas a dicotómicas (0 negativo, 1 positivo)
  dataset[, mrentabilidad_dico := ifelse(mrentabilidad < 0, 0, 1)]
  dataset[, mrentabilidad_annual_dico := ifelse(mrentabilidad_annual < 0, 0, 1)]
  dataset[, mactivos_margen_dico := ifelse(mactivos_margen < 0, 0, 1)]
  dataset[, mcuenta_corriente_adicional_dico := ifelse(mcuenta_corriente_adicional < 0, 0, 1)]
  dataset[, mcuenta_corriente_dico := ifelse(mcuenta_corriente < 0, 0, 1)]
  dataset[, mcaja_ahorro_dico := ifelse(mcaja_ahorro < 0, 0, 1)]
  dataset[, mcaja_ahorro_adicional_dico := ifelse(mcaja_ahorro_adicional < 0, 0, 1)]
  dataset[, mcaja_ahorro_dolares_dico := ifelse(mcaja_ahorro_dolares < 0, 0, 1)]
  dataset[, mcuentas_saldo_dico := ifelse(mcuentas_saldo < 0, 0, 1)]
  
  # cantidad de productos (tarjetas, cuentas, etc) a dicotómicas (0 o 1 y más)
  dataset[, cproductos_dico := ifelse(cproductos == 0, 0, 1)]
  dataset[, tcuentas_dico := ifelse(tcuentas == 0, 0, 1)]
  dataset[, ccuenta_corriente_dico := ifelse(ccuenta_corriente == 0, 0, 1)]
  dataset[, ccaja_ahorro_dico := ifelse(ccaja_ahorro == 0, 0, 1)]
  dataset[, ctarjeta_debito_dico := ifelse(ctarjeta_debito == 0, 0, 1)]
  dataset[, ctarjeta_master_dico := ifelse(ctarjeta_master == 0, 0, 1)]
  dataset[, cprestamos_personales_dico := ifelse(cprestamos_personales == 0, 0, 1)]
  dataset[, cprestamos_prendarios_dico := ifelse(cprestamos_prendarios == 0, 0, 1)]
  dataset[, cprestamos_hipotecarios_dico := ifelse(cprestamos_hipotecarios == 0, 0, 1)]
  dataset[, cplazo_fijo_dico := ifelse(cplazo_fijo == 0, 0, 1)]
  dataset[, cinversion1_dico := ifelse(cinversion1 == 0, 0, 1)]
  dataset[, cinversion2_dico := ifelse(cinversion2 == 0, 0, 1)]
  dataset[, cseguro_vida_dico := ifelse(cseguro_vida == 0, 0, 1)]
  dataset[, cseguro_auto_dico := ifelse(cseguro_auto == 0, 0, 1)]
  dataset[, cseguro_vivienda_dico := ifelse(cseguro_vivienda == 0, 0, 1)]
  dataset[, cseguro_accidentes_personales_dico := ifelse(cseguro_accidentes_personales == 0, 0, 1)]
  dataset[, ccuenta_debitos_automaticos_dico := ifelse(ccuenta_debitos_automaticos == 0, 0, 1)]
  dataset[, ctarjeta_visa_debitos_automaticos_dico := ifelse(ctarjeta_visa_debitos_automaticos == 0, 0, 1)]
  dataset[, ctarjeta_master_debitos_automaticos_dico := ifelse(ctarjeta_master_debitos_automaticos == 0, 0, 1)]
  dataset[, ctarjeta_debitos_automaticos_dico := ifelse((ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos) == 0, 0, 1)]
  dataset[, cpagodeservicios_dico := ifelse(cpagodeservicios == 0, 0, 1)]
  dataset[, cpagomiscuentas_dico := ifelse(cpagomiscuentas == 0, 0, 1)]
  dataset[, ccajeros_propios_descuentos_dico := ifelse(ccajeros_propios_descuentos == 0, 0, 1)]
  dataset[, ctarjeta_descuentos_dico := ifelse((ctarjeta_visa_descuentos + ctarjeta_master_descuentos) == 0, 0, 1)]
  dataset[, ccomisiones_dico := ifelse((ccomisiones_mantenimiento + ccomisiones_otras) == 0, 0, 1)]
  dataset[, cforex_dico := ifelse(cforex == 0, 0, 1)]
  dataset[, cforex_buy_dico := ifelse(cforex_buy == 0, 0, 1)]
  dataset[, cforex_sell_dico := ifelse(cforex_sell == 0, 0, 1)]
  dataset[, ctransferencias_recibidas_dico := ifelse(ctransferencias_recibidas == 0, 0, 1)]
  dataset[, ctransferencias_emitidas_dico := ifelse(ctransferencias_emitidas == 0, 0, 1)]
  dataset[, cextraccion_autoservicio_dico := ifelse(cextraccion_autoservicio == 0, 0, 1)]
  dataset[, ccheques_depositados_dico := ifelse(ccheques_depositados == 0, 0, 1)]
  dataset[, ccheques_emitidos_dico := ifelse(ccheques_emitidos == 0, 0, 1)]
  dataset[, ccheques_depositados_rechazados_dico := ifelse(ccheques_depositados_rechazados == 0, 0, 1)]
  dataset[, ccheques_emitidos_rechazados_dico := ifelse(ccheques_emitidos_rechazados == 0, 0, 1)]
  
  # Al menos un préstamo
  dataset[, algun_prestamo := ifelse(cprestamos_personales == 0 |
  																	 	cprestamos_prendarios == 0 |
  																	 	cprestamos_hipotecarios == 0, 0, 1)]
  
  # Al menos un seguro
  dataset[, algun_seguro := ifelse(cseguro_vida == 0 |
  																cseguro_auto == 0 |
  																cseguro_vivienda == 0 |
  																cseguro_accidentes_personales == 0, 0, 1 )]
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply( names(dataset),
                            function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )
  
  infinitos_qty  <- sum( unlist( infinitos ) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  # Paso los NaN a NA
  nans      <- lapply( names(dataset),
                       function(.name) dataset[ , sum( is.nan( get(.name) )) ] )
  
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a NA" )
#    cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
    dataset[mapply(is.nan, dataset)] <- NA
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  #grabo con nombre extendido
  fwrite( dataset,
          file= arch_destino,
          sep= "," )

}
#------------------------------------------------------------------------------

#aqui comienza el programa

#Establezco el Working Directory
setwd( "d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII" )


#lectura de los datasets
dataset1  <- fread("./datasets/paquete_premium_202011.csv")
dataset2  <- fread("./datasets/paquete_premium_202101.csv")


#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/FE4020/", showWarnings = FALSE )
setwd("d:/Datos User/Desktop/Datos esc/MCD/12 Laboratorio de Implementacion/LII/labo/exp/FE4020/")   #Establezco el Working Directory DEL EXPERIMENTO

EnriquecerDataset( dataset1, "paquete_premium_202011_ext.csv" )
EnriquecerDataset( dataset2, "paquete_premium_202101_ext.csv" )

