###############################################################
# Proyecto Santander - Jose Luis Apellániz Ortiz
###############################################################
################################################################
# Carga de paquetes
################################################################
cat("\014")
rm(list = ls())

packages = c("data.table","ggplot2","lubridate","mice")
new = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()


##############################################################
# Lectura de fichero.
# Empleamos los métodos read.csv y fread para comparar tiempos de lectura. 
# fread es más rápido (unas 13 veces más) y ocupan similar cantidad de memoria
#############################################################
inicio=Sys.time()
df= read.csv("dat/dataSant.csv",fileEncoding = "UTF-8")
time_r = as.numeric(Sys.time()-inicio)
# Time difference of 5.964072 secs
summary(df)
str(df)
class(df)
# [1] "data.frame"
object.size(df)
# 71449136 bytes


inicio=Sys.time()
df= fread("dat/dataSant.csv",stringsAsFactors = T,encoding = "UTF-8", fill = T)
time_f = as.numeric(Sys.time()-inicio)
# Time difference of 0.351125 secs
summary(df)
str(df)
class(df)
#[1] "data.table" "data.frame"
object.size(df)
# 71430592 bytes

################################################
# Revisión de la calidad del dato por columnas
# Comprobamos tipos, NA's, ausencia de valores,...
# para cada columna
################################################

# Hay 3763 valores vacios o NA's en casi todas las columnas descriptivas:
# ind_empleado, pais_residencia, sexo, age, fecha_alta, ind_nuevo, antiguedad
# indrel, indrel_1mes, tiprel_1mes, indresi, indext, canal_entrada (3786 vacios)
# indfall, cod_prov (4621 NA's), nomprov (4621 NA's), ind_actividad_cliente, 
# renta (65993 NA's), segmento (3797 sin clasificar)

# Extraemos un nuevo df1 para eliminar las 2763 filas 
# Filtramos las filas con los 2763 NA's en base a uno de los campos que presentan el problema
criterio1 = !is.na(df$antiguedad)
df1 = df[criterio1,]

# Comprobamos como queda el nuevo df
summary(df1)
head(df1)
str(df1)
# Se han eliminado sólo las filas que tenían los 3763 NA's o datos ausentes
# Tenemos todavía los siguientes vacios y NAs
# canal_entrada (23 vacios), cod_prov (858 NA's), nomprov (858 vacios),  
# renta (62230 NA's), segmento (34 vacios)

df = df1
rm (df1)
str(df)

# La columna conyuemp son todo NAs y la columna tipodom son todo "1", no aportan información
# La columna ult_fec_cli_1t tiene el 99,8% de sus datos NA, tampoco va a aportar nada al análisis
# Las eliminamos:
df = df[,-c(11,16,19)]


# Cambiamos el formato de algunas columnas
# ###############################################################
# Cambiamos formatos de fecha (as.POSIXct)
# ##############################################################

# fecha_dato
df$fecha_dato = as.POSIXct(df$fecha_dato, format="%Y-%m-%d")

# fecha_alta
df$fecha_alta = as.POSIXct(df$fecha_alta, format="%Y-%m-%d")


# ###############################################################
# Cambiamos y corregimos formatos factor (as.factor)
# ##############################################################

levels(df$ind_empleado)
df$ind_empleado = factor(df$ind_empleado, levels = c("A", "B", "F", "N", "S"))
table(df$ind_empleado)

# ind_nuevo
df$ind_nuevo = as.factor(df$ind_nuevo)
table(df$ind_nuevo)

# ind_actividad_cliente
df$ind_actividad_cliente = as.factor(df$ind_actividad_cliente)
table(df$ind_actividad_cliente)

# indrel
df$indrel=as.factor(df$indrel)
table(df$indrel)


# indrel_1mes
df$indrel_1mes = factor(df$indrel_1mes, levels = c("1", "2", "3", "4", "P"))
table(df$indrel_1mes)
 
# tiprel_1mes
levels(df$tiprel_1mes)
df$tiprel_1mes = factor(df$tiprel_1mes, levels = c("A", "I", "N", "P", "R"))
levels(df$tiprel_1mes)
table(df$tiprel_1mes)

# indresi
df$indresi = factor(df$indresi, levels = c("N", "S"))
table(df$indresi)

# indext
df$indext = factor(df$indext, levels = c("N", "S"))
table(df$indext)

# indfall
df$indfall = factor(df$indfall, levels = c("N", "S"))
table(df$indfall)

# segmento.
levels(df$segmento)
df$segmento = factor(df$segmento, levels = c("01 - TOP", "02 - PARTICULARES", "03 - UNIVERSITARIO"))
table(df$segmento)



######## Outliers ##########################################################
# Analizamos el campo age. Hay muchos outliers.
#############################################################################

# prop.table escala los valores entre 0 y 1 porque hace val/sum(val)
plot(prop.table(table(df$age)))
g = ggplot2::ggplot(df,aes(df$age))
g+geom_density()

outliers_age = boxplot.stats(df$age)$out
boxplot(df$age,boxwex=0.1)

# Método para eliminar los outliers
# Tomado de http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset

x = df$age
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = NA
x[x > (qnt[2] + H)] = NA

plot(prop.table(table(x)))
g = ggplot2::ggplot(df,aes(x))
g+geom_density()

df$age = x
boxplot(df$age,boxwex=0.1)

# Comprobamos dónde tenemos NA's
# Primero hacemos que la tabla df sea solo data.frame por esto:
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#j-num
df = as.data.frame(df)

total_na = 0
for (i in 1:ncol(df)) {
  if (sum(is.na(df[,i])) > 0){
  print(paste0("La columna nº", i, " ", colnames(df[i]), " tiene ", sum(is.na(df[,i])), " NA's."))
  }
  total_na = total_na + sum(is.na(df[,i]))
}
print(paste0("El número total de NAs es: ", total_na))


# Comprobamos donde tenemos valores vacios
for (j in 1:ncol(df)) {
    if (class(df[,j])[1] == "factor") {
        dfTemp <- subset(df, !is.na(df[,j])) # Hacemos un subset para evitar choque con los NA's
        vacios = sum(dfTemp[,j] == "")
        if (vacios != 0) {
        print(paste0("La columna nº", j, " ", colnames(df[j]), " tiene ", vacios, " valores vacios" ))
        } 
    }
}

# Analizamos los valores vacios de nomprov en función de pais_residencia
# Todos los valores menos 1 (857) se corresponden a paises diferentes a ES

table(df[df$nomprov=="",c("pais_residencia")])
sum(table(df[df$nomprov=="",c("pais_residencia")]))


which(levels(df$nomprov) == "") # Se trata del índice 1.
levels(df$nomprov)[1] = "EXTRANJERO" #  Lo cambiamos por EXTRAJERO
levels(df$nomprov)

# Pero hay un registro con pais_residencia="ES" que no puede ser EXTRAJERO
# Lo cambiamos a NA
df1=subset(df, df$nomprov=="EXTRANJERO")
df2=subset(df1, df1$pais_residencia=="ES")
df$nomprov[df$ncodpers==df2$ncodpers]=NA

# Ahora convertimos los 23 valores vacios de canal_entrada a NA's
which(levels(df$canal_entrada) == "") # Se trata del índice 1.
levels(df$canal_entrada)[1] = NA #  Lo cambiamos por EXTRAJERO
levels(df$canal_entrada)

# El campo antiguedad sigue una distribución muy extraña con 5 maximos, 4 agrupados en valores<50 y otro 
# más pequeño hacia 165. Podría deberse a diferentes periódos de captación de clientes.
# el grupo más antiguo merecería un tratamiento aparte, ya que probablemente tengan un comportamiento
# diferente del resto, por lo que podrían cosiderar outliers (hay 41400 ouliers).
hist(df$antiguedad, main = "Antigüedad")
plot(prop.table(table(df$antiguedad)))
g = ggplot2::ggplot(df,aes(df$antiguedad))
g+geom_density()

outliers_antiguedad = boxplot.stats(df$antiguedad)$out
boxplot(df$antiguedad,boxwex=0.1)

summary(df$antiguedad)


# Analizamos el campo renta
# El rango de este parámetro es muy grande, con muchos outliers (hay 184279). En este caso se podría, 
# dependiendo de la política del banco se podría hacer un análisis según el tipo de renta, haciendo 
# tres grupos: rentas bajas, medias y altas
plot(prop.table(table(df$renta)))
g = ggplot2::ggplot(df,aes(df$renta))
g+geom_density()

outliers_renta = boxplot.stats(df$renta)$out
boxplot(df$renta,boxwex=0.1)

summary(df$renta)



# Mediante los siguientes gráficos también podemos observar la cantidad de valores perdidos
library(VIM) # install.packages("VIM")
par(mfrow = c(1,1))
distro.na.v1 = aggr(df, col = c('navyblue', 'red'), numbers = TRUE,
                    sortVars = FALSE, labels = names(df), cex.axis = .7, gap = 3,
                    ylab = c("Histograma de valores perdidos","Patrón"))
# Otra configuración diferente:
distro.na.v2 <- aggr(df, col = c("lightblue", "darkorange"),
                     combined = TRUE,
                     numbers = TRUE,
                     prop = FALSE,
                     sortVars = TRUE,
                     labels = names(df),
                     cex.axis = .7,
                     cex.numbers = .7,
                     only.miss = TRUE,
                     ylab = "Patrón de NAs")
