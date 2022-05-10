# Analisis descriptivo

attach(dataset)

# Indica si el cliente ha realizado transacciones voluntarias 
# en el ultimo timestre . Que el banco cobre la comisión de 
# mantenimiento de cuenta NO es un movimiento voluntario por 
# mas que es un debito en la cuenta.  Que se cobre una cuota 
# de una compra con tarjeta de crédito, tampoco es un movimiento 
# voluntario.
table(active_quarter)

# Indica si marketing considera a esa cliente un cliente vip al
# momento de obtención de la foto.  A pesar que todos estos son 
# clientes de Paquete Premium,  el area de marketing solo considera 
# vip a menos del  2% de los paquete premium , y da a estos clientes 
# VIP un trato a puertas cerradas.
table(cliente_vip)

#indica si el cliente usa servicios de HomeBanking o tiene 
# instalada la app
# ¿Por qué tiene 5 valores distintos? 0:5
table(internet) 

# Edad en años del cliente.
table(cliente_edad)

# Antiguedad medida en meses de el cliente.  Es de la ultima 
# vez que esa persona reingresó como cliente del banco.
table(cliente_antiguedad)

# Ganancia total que ha obtenido el banco de ese cliente, en ese mes.
summary(mrentabilidad)

# Ganancia total que el banco ha obtenido de ese cliente en el 
# ultimo año de relacion cliente-banco,  o meses desde que ingresó
# si es cliente reciente.
summary(mrentabilidad_annual)

# Monto total de las comisiones que ha ganado el banco por ese cliente.
summary(mcomisiones)

# Monto total de la ganancia que el banco ha obtenido en concepto de
# intereses que ha cobrado al cliente.
summary(mactivos_margen)

# Monto total de la ganancia que el banco ha obtenido por el 
# dinero/inversiones que el cliente tiene en el banco.
summary(mpasivos_margen)

# Cantidad de productos que el cliente posee con el banco. 
# Se cuentan las familias de productos.
table(cproductos)

# Sin descripción (la que aparece con mayor importancia está
# en orden 91 recién)
table(tpaquete1)
table(tpaquete2)
table(tpaquete7)
table(tpaquete9)

# Cantidad de cuentas que el cliente tiene,  vale 0 si no 
# tiene ninguna,  1 si solo tiene cajas de ahorro o solo 
# cuentas corrientes,  2 si tiene al menos una caja de ahorro 
# y tambien al menos una cuenta corriente.
table(tcuentas)

# Cantidad de cuentas corrientes que tiene el cliente. Hay 
# muy pocos clientes con mas de una cuenta corriente.
table(ccuenta_corriente)

# Monto total de las cuentas corrientes adicionales que 
# no forman parte del paquete.
summary(mcuenta_corriente_adicional)

# Monto total de las cuenta corriente del paquete premium
summary(mcuenta_corriente)

# Cantidad de cajas de ahorro que tiene el cliente.
table(ccaja_ahorro)

# Monto total de la caja de ahorro del Paquete Premium
summary(mcaja_ahorro)

# Monto total de las cajas otras cajas de ahorro que no 
# forman parte del paquete.
summary(mcaja_ahorro_adicional)

# Monto total de las cajas de ahorro en dólares.  El valor
# esta expresado en pesos, y se considera el valor del dolar 
# de cierre del último dia hábil del mes.
summary(mcaja_ahorro_dolares)

# Monto total de los descubiertos acordados en las cuentas corrientes.
summary(mdescubierto_preacordado)
table(mdescubierto_preacordado)

# Saldo total de TODAS las cuentas del cliente, cajas de ahorro, 
# cuentas corrientes, pesos y dolares. El valor esta convertido a pesos.
summary(mcuentas_saldo)

# Cantidad de tarjetas de débito que posee el cliente.
table(ctarjeta_debito)

# Cantidad de transacciones que hizo el cliente con su tarjeta 
# de débito durante el mes.
table(ctarjeta_debito_trx)
summary(ctarjeta_debito_trx)

# Monto total de las transacciones que hizo el cliente con su 
# tarjeta de débito duirante el mes.
summary(mautoservicio)




