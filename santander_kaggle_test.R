
################################################################
# Carga de paquetes
################################################################
packages = c("mice","openxlsx","data.table","ggplot2","lubridate")
new = packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

##############################################################
# Lectura de fichero de test. Trabajaremos inicialmente con dicho 
# fichero por ser más pequeño.
# Empleamos los métodos read.csv y fread para comparar. 
# fread es más rápido (unas 20 veces más) y ocupa menos memoria (33.5 Mbytes menos)
#############################################################
inicio=Sys.time()
stdr_test= read.csv("dat/test_ver2.csv",fileEncoding = "UTF-8")
time_r = as.numeric(Sys.time()-inicio)
time = Sys.time()-inicio
# Time difference of 15.52494 secs
summary(stdr_test)
str(stdr_test)
class(stdr_test)
# [1] "data.frame"
object.size(stdr_test)
# 130662880 bytes


inicio=Sys.time()
stdr_test_f= fread("dat/test_ver2.csv",stringsAsFactors = T, encoding = "UTF-8")
time_f = as.numeric(Sys.time()-inicio)
time = Sys.time()-inicio
# Time difference of 0.87924 secs
summary(stdr_test_f)
str(stdr_test_f)
class(stdr_test_f)
#[1] "data.table" "data.frame"
object.size(stdr_test_f)
# 97201968 bytes

################################################
# Revisión de la calidad del dato por columnas
# Comprobamos tipos, NA's, ausencia de valores,...
# para cada columna
################################################


table(stdr_test_f$ind_empleado)
sum(table(stdr_test_f$ind_empleado))

table(stdr_test_f$pais_residencia)
sum(table(stdr_test_f$pais_residencia))

table(stdr_test_f$sexo)
sum(table(stdr_test_f$sexo))

hist(stdr_test_f$age)
boxplot(stdr_test_f$age)
table(stdr_test_f$age)

table(stdr_test_f$antiguedad)

table(stdr_test_f$indrel)

table(stdr_test_f$ult_fec_cli_1t)

table(stdr_test_f$indrel_1mes)

table(stdr_test$tiprel_1mes)
sum(table(stdr_test$tiprel_1mes))

table(stdr_test$indresi)
sum(table(stdr_test$indresi))

table(stdr_test$indext)
sum(table(stdr_test$indext))

table(stdr_test$conyuemp)
sum(table(stdr_test$conyuemp))

table(stdr_test$canal_entrada)
sum(table(stdr_test$canal_entrada))

table(stdr_test$indfall)
sum(table(stdr_test$indfall))

table(stdr_test$tipodom)

table(stdr_test$cod_prov)
sum(table(stdr_test$cod_prov))

table(stdr_test$nomprov)
sum(table(stdr_test$nomprov))

table(stdr_test_f$ind_actividad_cliente)
sum(table(stdr_test_f$ind_actividad_cliente))


sum(is.na(stdr_test_f$renta))

table(stdr_test_f$segmento)
sum(table(stdr_test_f$segmento))



#####################################################
# Empezamos la limpieza
#####################################################


# Casos (1) de ind_empleado con "S" en lugar de "P" 
# No parece problemático
table(stdr_test_f$ind_empleado)
criterio2 = stdr_test_f$ind_empleado=="S"
solo_s = stdr_test_f[criterio2,]

# Casos (5) con sexo sin especificar
# De momento los mantenemos
table(stdr_test_f$sexo)
criterio3 = stdr_test_f$sexo==""
sin_sexo = stdr_test_f[criterio3,]
stdr_test_f$sexo = factor(stdr_test_f$sexo, levels = c("H", "V"))

# Analizamos el campo age. Hay muchos outliers.
plot(prop.table(table(stdr_test_f$age)))
g = ggplot2::ggplot(stdr_test_f,aes(stdr_test_f$age))
g+geom_density()

outliers_age = boxplot.stats(stdr_test_f$age)$out
boxplot(stdr_test_f$age,boxwex=0.1)

# Método para eliminar los outliers: Capping
# Recogido de http://r-statistics.co/Outlier-Treatment-With-R.html

x = stdr_test_f$age
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
caps = quantile(x, probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = caps[1]
x[x > (qnt[2] + H)] = caps[2]

plot(prop.table(table(x)))
g = ggplot2::ggplot(stdr_test_f,aes(x))
g+geom_density()

stdr_test_f$age = x

# ###############################################################
# Cambiamos formatos de fecha (as.POSIXct)
# ##############################################################

# fecha_dato
stdr_test_f$fecha_dato = as.POSIXct(stdr_test_f$fecha_dato, format="%Y-%m-%d")

# fecha_alta
stdr_test_f$fecha_alta = as.POSIXct(stdr_test_f$fecha_alta, format="%Y-%m-%d")

# ult_fec_cli_1t
stdr_test_f$ult_fec_cli_1t = as.POSIXct(stdr_test_f$ult_fec_cli_1t, format="%Y-%m-%d")

# ###############################################################
# Cambiamos formatos a booleanos (as.logical)
# ##############################################################

# ind_nuevo
stdr_test_f$ind_nuevo = as.logical(stdr_test_f$ind_nuevo)

# ind_actividad_cliente
stdr_test_f$ind_actividad_cliente = as.logical(stdr_test_f$ind_actividad_cliente)

# ###############################################################
# Cambiamos formatos a factor (as.factor)
# ##############################################################

# indrel
stdr_test_f$indrel=as.factor(stdr_test_f$indrel)


# indrel_1mes
stdr_test_f$indrel_1mes[stdr_test_f$indrel_1mes=="1.0"] = "1"
stdr_test_f$indrel_1mes[stdr_test_f$indrel_1mes=="2.0"] = "2"
stdr_test_f$indrel_1mes[stdr_test_f$indrel_1mes=="3.0"] = "3"
stdr_test_f$indrel_1mes[stdr_test_f$indrel_1mes=="4.0"] = "4"

stdr_test_f$indrel_1mes = factor(stdr_test_f$indrel_1mes, levels = c("1", "2", "3", "4", "P"))

# tiprel_1mes
stdr_test_f$tiprel_1mes = factor(stdr_test_f$tiprel_1mes, levels = c("A", "I", "N", "P", "R"))

# conyuemp
stdr_test_f$conyuemp = factor(stdr_test_f$conyuemp, levels = c("N", "S"))

# segmento
stdr_test_f$segmento = factor(stdr_test_f$segmento, levels = c("01 - TOP", "02 - PARTICULARES", "03 - UNIVERSITARIO"))


