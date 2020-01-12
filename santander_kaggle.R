
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
stdr_test_f= fread("dat/test_ver2.csv",stringsAsFactors = T)
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




##############################################################
# Lectura de fichero de train. Ahora comparamos la calidad del 
# fichero de train con la del fichero de test.
# Empleamos los métodos read.csv y fread para comparar tiempos de lectura. 
# fread es más rápido (unas 13 veces más) y ocupan similar cantidad de memoria
#############################################################
inicio=Sys.time()
stdr_train= read.csv("dat/train_ver2.csv",fileEncoding = "UTF-8")
time_r = as.numeric(Sys.time()-inicio)
time = Sys.time()-inicio
# Time difference of 3.998676 mins!!!!
summary(stdr_train)
str(stdr_train)
class(stdr_train)
# [1] "data.frame"
object.size(stdr_train)
# 2675438880 bytes


inicio=Sys.time()
stdr_train_f= fread("dat/train_ver2.csv",stringsAsFactors = T,encoding = "UTF-8")
time_f = as.numeric(Sys.time()-inicio)
time = Sys.time()-inicio
# Time difference of 17.60462 secs
summary(stdr_train_f)
str(stdr_train_f)
class(stdr_train_f)
#[1] "data.table" "data.frame"
object.size(stdr_train_f)
# 2675417904 bytes

################################################
# Revisión de la calidad del dato por columnas
# Comprobamos tipos, NA's, ausencia de valores,...
# para cada columna
################################################


table(stdr_train_f$ind_empleado)
sum(table(stdr_train_f$ind_empleado))

table(stdr_train_f$pais_residencia)
sum(table(stdr_train_f$pais_residencia))

table(stdr_train_f$sexo)
sum(table(stdr_train_f$sexo))

hist(stdr_train_f$age)
boxplot(stdr_train_f$age)
table(stdr_train_f$age)

table(stdr_train_f$antiguedad)

table(stdr_train_f$indrel)

table(stdr_train_f$ult_fec_cli_1t)

table(stdr_train_f$indrel_1mes)

table(stdr_train$tiprel_1mes)
sum(table(stdr_train$tiprel_1mes))

table(stdr_train$indresi)
sum(table(stdr_train$indresi))

table(stdr_train$indext)
sum(table(stdr_train$indext))

table(stdr_train$conyuemp)
sum(table(stdr_train$conyuemp))

table(stdr_train$canal_entrada)
sum(table(stdr_train$canal_entrada))

table(stdr_train$indfall)
sum(table(stdr_train$indfall))

table(stdr_train$tipodom)

table(stdr_train$cod_prov)
sum(table(stdr_train$cod_prov))

table(stdr_train$nomprov)
sum(table(stdr_train$nomprov))

table(stdr_train_f$ind_actividad_cliente)
sum(table(stdr_train_f$ind_actividad_cliente))


sum(is.na(stdr_train_f$renta))

table(stdr_train_f$segmento)
sum(table(stdr_train_f$segmento))


#####################################################
# Empezamos la limpieza
#####################################################

# Intentamos un reporte de valores perdidos, pero tarda demasiado...
# reporte = md.pattern(stdr_train_f)

# Filtramos las filas con los 27734 NA's
criterio1 = !is.na(stdr_train_f$age)
stdr_train_f = stdr_train_f[criterio1,]

summary(stdr_train_f)

# Casos (17) de ind_empleado con "S" en lugar de "P" 
# No parecen problemáticos
table(stdr_train_f$ind_empleado)
criterio2 = stdr_train_f$ind_empleado=="S"
solo_s = stdr_train_f[criterio2,]
stdr_train_f$ind_empleado = factor(stdr_train_f$ind_empleado, levels = c("A", "B", "F", "N", "S"))

# Casos (70) con sexo sin especificar
# De momento los mantenemos
table(stdr_train_f$sexo)
criterio3 = stdr_train_f$sexo==""
sin_sexo = stdr_train_f[criterio3,]
stdr_train_f$sexo = factor(stdr_train_f$sexo, levels = c("H", "V"))

# Analizamos el campo age. Hay muchos outliers.
plot(prop.table(table(stdr_train_f$age)))
g = ggplot2::ggplot(stdr_train_f,aes(stdr_train_f$age))
g+geom_density()

outliers_age = boxplot.stats(stdr_train_f$age)$out
boxplot(stdr_train_f$age,boxwex=0.1)

# Método para eliminar los outliers: Capping
# Recogido de http://r-statistics.co/Outlier-Treatment-With-R.html

x = stdr_train_f$age
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
caps = quantile(x, probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = caps[1]
x[x > (qnt[2] + H)] = caps[2]

plot(prop.table(table(x)))
g = ggplot2::ggplot(stdr_train_f,aes(x))
g+geom_density()

stdr_train_f$age = x

# ###############################################################
# Cambiamos formatos de fecha (as.POSIXct)
# ##############################################################

# fecha_dato
stdr_train_f$fecha_dato = as.POSIXct(stdr_train_f$fecha_dato, format="%Y-%m-%d")

# fecha_alta
stdr_train_f$fecha_alta = as.POSIXct(stdr_train_f$fecha_alta, format="%Y-%m-%d")

# ult_fec_cli_1t
stdr_train_f$ult_fec_cli_1t = as.POSIXct(stdr_train_f$ult_fec_cli_1t, format="%Y-%m-%d")

# ###############################################################
# Cambiamos formatos a booleanos (as.logical)
# ##############################################################

# ind_nuevo
stdr_train_f$ind_nuevo = as.logical(stdr_train_f$ind_nuevo)

# ind_actividad_cliente
stdr_train_f$ind_actividad_cliente = as.logical(stdr_train_f$ind_actividad_cliente)

# ###############################################################
# Cambiamos formatos a factor (as.factor)
# ##############################################################

# indrel
stdr_train_f$indrel=as.factor(stdr_train_f$indrel)


# indrel_1mes
stdr_train_f$indrel_1mes[stdr_train_f$indrel_1mes=="1.0"] = "1"
stdr_train_f$indrel_1mes[stdr_train_f$indrel_1mes=="2.0"] = "2"
stdr_train_f$indrel_1mes[stdr_train_f$indrel_1mes=="3.0"] = "3"
stdr_train_f$indrel_1mes[stdr_train_f$indrel_1mes=="4.0"] = "4"

stdr_train_f$indrel_1mes = factor(stdr_train_f$indrel_1mes, levels = c("1", "2", "3", "4", "P"))

# tiprel_1mes
stdr_train_f$tiprel_1mes = factor(stdr_train_f$tiprel_1mes, levels = c("A", "I", "N", "P", "R"))

# indresi
stdr_train_f$indresi = factor(stdr_train_f$indresi, levels = c("N", "S"))


# indext
stdr_train_f$indext = factor(stdr_train_f$indext, levels = c("N", "S"))

# conyuemp
stdr_train_f$conyuemp = factor(stdr_train_f$conyuemp, levels = c("N", "S"))

# indfall
stdr_train_f$indfall = factor(stdr_train_f$indfall, levels = c("N", "S"))

# segmento
stdr_train_f$segmento = factor(stdr_train_f$segmento, levels = c("01 - TOP", "02 - PARTICULARES", "03 - UNIVERSITARIO"))


