###############################################################
# Proyecto Santander - Jose Luis Apellániz Ortiz
###############################################################
################################################################
# Carga de paquetes y directorio de trabajo
################################################################
cat("\014")
rm(list = ls())

packages = c("data.table","ggplot2","lubridate","mice")
new = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


##############################################################
# Lectura del fichero dataSant.csv.
# Empleo método fread. 
# fread es más rápido que read.csv (unas 13 veces más según mis cálculos)
# y ocupan similar cantidad de memoria
#############################################################
inicio=Sys.time()
df= fread("dat/dataSant.csv",stringsAsFactors = T,encoding = "UTF-8", fill = T)
time_f = as.numeric(Sys.time()-inicio)
time_f
class(df)
#[1] "data.table" "data.frame"
# Hacemos que la tabla df sea solo data.frame por esto:
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#j-num
df = as.data.frame(df)

object.size(df) # tamaño en memoria

# Resúmenes estadísticos de los datos del fichero
summary(df)
str(df)

##################################################################################
# He creado dos funciones para calcular el número de NAs y de valores nulos y otra
# para calcular la Moda
##################################################################################
calculaNAs = function(x) {
  total_na = 0
  for (i in 1:ncol(x)) {
    if (sum(is.na(x[,i])) > 0){
      print(paste0("La columna nº", i, " ", colnames(x[i]), " tiene ", sum(is.na(x[,i])), " NA's."))
    }
    total_na = total_na + sum(is.na(x[,i]))
  }
  print("-------------------------------------------")
  print(paste0("El número total de NAs es: ", total_na))
}

calculaNulos = function(x) {
  nohaynulos = TRUE
  for (j in 1:ncol(x)) {
      if (class(x[,j])[1] == "factor") {
      dfTemp <- subset(x, !is.na(x[,j])) # Hacemos un subset para evitar choque con los NA's
      vacios = sum(dfTemp[,j] == "")
      if (vacios != 0) {
        nohaynulos = FALSE
        print(paste0("La columna nº", j, " ", colnames(x[j]), " tiene ", vacios, " valores vacios" ))
      } 
    }
  }
  if (nohaynulos) print("No hay valores nulos en el df")
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Primera comprobación de NA's y nulos
calculaNAs(df)
calculaNulos(df)

################################################
# Revisión de la calidad del dato por columnas
# Comprobamos tipos, NA's, ausencia de valores,...
# para cada columna
################################################

# La columna conyuemp son todo NAs y la columna tipodom son todo "1", no aportan información
# La columna ult_fec_cli_1t tiene el 99,8% de sus datos NA, tampoco va a aportar nada al análisis
# Las eliminamos:
df = df[,-c(11,16,19)]

# Hay 3763 valores vacios o NA's en casi todas las columnas descriptivas:
# ind_empleado, pais_residencia, sexo, age, fecha_alta, ind_nuevo, antiguedad
# indrel, indrel_1mes, tiprel_1mes, indresi, indext, canal_entrada (3786 vacios)
# indfall, cod_prov (4621 NA's), nomprov (4621 NA's), ind_actividad_cliente, 
# renta (65993 NA's), segmento (3797 sin clasificar)

# Extraemos un nuevo df1 para eliminar las 3763 filas 
# Filtramos las filas con los 3763 NA's en base a uno de los campos que presentan el problema
criterio1 = !is.na(df$antiguedad)
df1 = df[criterio1,]

# Comprobamos como queda el nuevo df
calculaNAs(df1)
calculaNulos(df1)

# Se han eliminado sólo las filas que tenían los 3763 NA's o datos ausentes
# Tenemos todavía los siguientes vacios y NAs:
# canal_entrada (23 vacios), cod_prov (858 NA's), nomprov (858 vacios),  
# renta (62230 NA's), segmento (34 vacios)

df = df1
rm (df1)
str(df)


# Cambiamos el formato de algunas columnas
# ###############################################################
# Cambiamos formatos de fecha (as.POSIXct)
# También se podría hacer con as.Date
# ##############################################################

# fecha_dato
df$fecha_dato = as.POSIXct(df$fecha_dato, format="%Y-%m-%d")

# fecha_alta
df$fecha_alta = as.POSIXct(df$fecha_alta, format="%Y-%m-%d")


# ###############################################################
# Cambiamos y corregimos formatos factor (as.factor) para adaptarlos
# a las descripciones.
# ##############################################################

# ind_empleado
levels(df$ind_empleado)
df$ind_empleado = factor(df$ind_empleado, levels = c("A", "B", "F", "N", "S"))
table(df$ind_empleado)

# sexo
levels(df$sexo)
levels(df$sexo)[2] = "mujer" # Cambiar el primer nivel de "H" a "mujer"
levels(df$sexo)[3] = "hombre" # Cambiar el segundo nivel de "V" a "hombre"
df$sexo = factor(df$sexo, levels = c("mujer", "hombre"))
table(df$sexo)

# pais_residencia
table(df$pais_residencia) # hay un factor "" sin observaciones. Se puede eliminar
paises = levels(df$pais_residencia)
paises_ok = paises[2:length(paises)]
paises_ok # comprobamos
df$pais_residencia = factor(df$pais_residencia, levels = paises_ok)
table(df$pais_residencia)

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
table(df$tiprel_1mes)

# indresi
levels(df$indresi)
df$indresi = factor(df$indresi, levels = c("N", "S"))
table(df$indresi)

# indext
levels(df$indext)
df$indext = factor(df$indext, levels = c("N", "S"))
table(df$indext)

# indfall
levels(df$indfall)
df$indfall = factor(df$indfall, levels = c("N", "S"))
table(df$indfall)

# segmento.
levels(df$segmento)
df$segmento = factor(df$segmento, levels = c("01 - TOP", "02 - PARTICULARES", "03 - UNIVERSITARIO"))
table(df$segmento)

# Comprobamos evolución
calculaNAs(df)
calculaNulos(df)

# Analizamos los valores vacios de nomprov en función de pais_residencia
# De los 858 valores, todos los valores menos 1 (857), se corresponden a paises diferentes a ES

table(df[df$nomprov=="",c("pais_residencia")])
sum(table(df[df$nomprov=="",c("pais_residencia")]))

which(levels(df$nomprov) == "") # Se trata del índice 1.
levels(df$nomprov)[1] = "EXTRANJERO" #  Lo cambiamos por EXTRAJERO
levels(df$nomprov)

# Pero hay un registro con pais_residencia="ES" que no puede ser EXTRAJERO
# Lo cambiamos por la Moda
Mode(df$nomprov)
# MADRID
df1=subset(df, df$nomprov == "EXTRANJERO")
df2=subset(df1, df1$pais_residencia=="ES")
df$nomprov[df$ncodpers==df2$ncodpers]= Mode(df$nomprov)

# Ahora convertimos los 23 valores vacios de canal_entrada a NA's
ind = which(levels(df$canal_entrada) == "") # Se trata del índice 1.
levels(df$canal_entrada)[ind] = NA #  Lo cambiamos por NA
levels(df$canal_entrada)

# Comprobamos que ya no hay valores nulos y han aparecido algunos más NA's
calculaNAs(df)
calculaNulos(df)

rm(df1)
rm(df2)

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


################# Tratamiento de NA's########################################
# Para la variable renta vamos a usar la media como valor de reemplazo
# He descartado las otras opciones conocidas por las siguientes razones:
# 1) Eliminación de valores. Me ha parecido que son demasiadas observaciones
# para optar por descartarlas.
# 2) Regresión lineal múltiple. El ajuste (respecto a "age" y "antiguedad" los
# valores numéricos más razonables) era pésima (R^2 < 1%)
# 3)Imputación múltiple. La ejecución era demasiado lenta con el paquete "mice"
########### Sustitución por la media en la variable renta ###################
# Trabajamos con "df", que aún mantiene los NAs:
summary(df)
df21 = df # Duplicar "df" para no trabajar sobre él

mean(df21$renta, na.rm = TRUE) # Valor medio (atención a "na.rm")
# Si quitásemos "na.rm" o lo ponemos a FALSE el resultado sería NA
# Sustituir NA el valor medio de la columna a la que pertenece:
df21$renta[is.na(df21$renta)] =
  mean(df21$renta, na.rm = TRUE)

summary(df21$renta) # Observar que ya no existen NAs en renta

# Comparar las medias
mean(df$renta, na.rm = T)
mean(df21$renta)


# Otra forma de realizarlo seria con la libreria "Hmisc"
df22 = df
library(Hmisc) # install.packages("Hmisc")

df22$renta = impute(x = df22$renta, fun = mean)
summary(df22$renta) 

# Aplicamos los cambios a nuestro df
df = df21
rm(df21)
rm(df22)

# NAs en cod_prov
# Vamos a asignar el valor 0 a los NA's en cod_prov que se corresponden con
# los de pais_residencia = "EXTRANJERO"
table(df$cod_prov)
summary(df$cod_prov)
df31 = df
df31$cod_prov[is.na(df31$cod_prov)] = 0
table(df31$cod_prov)
summary(df31$cod_prov)
df = df31
# Pero, como pasó en nombre_prov, hay un registro con pais_residencia="ES" 
# que no puede tener cod_prov = 0
# Lo cambiamos por la Moda para lo cual usamos una función que la calcula
Mode(df$cod_prov)
# Moda = 28 (Madrid)
df32=subset(df31, df31$cod_prov== 0)
df33=subset(df32, df32$pais_residencia=="ES")

df$cod_prov[df$ncodpers==df33$ncodpers] = Mode(df$cod_prov)

rm(df31)
rm(df32)
rm(df33)
calculaNAs(df)

# Dejamos el cod_prov como factor
df$cod_prov=as.factor(df$cod_prov)


# NAs (34) en campo segmento #####################################
# Vemos la distribución de segmento por edades
df41 = df
plot(df41$segmento ~ df$age,
     main = "Distribución del segmento por edades",
     xlab = "Edad", ylab = "Segmento",
     col = c("red", "blue", "green"))
# A la vista de la gráfica anterior podemos considerar
# 02 - PARTICULARES a los mayores de 30
# 03 - UNIVERSITARIO a los menores de 30

df41$segmento[df41$age>30 & is.na(df$segmento)] = levels(df$segmento)[2]
df41$segmento[df41$age<=30 & is.na(df$segmento)] = levels(df$segmento)[3]
summary(df41$segmento)
df = df41

rm(df41)

# NA's (23) de canal_entrada ################################################
# Vamos a utilizar los 3 canales_entrada más utilizados según el segmento
# al que pertenecen los clientes con NA en canal_entrada.
# Para conocer los 3 canales_entrada emplearemos la función tabdom extraida de 
# https://github.com/cosname/art-r-translation/blob/master/code/Ch6/tabdom.R
# finds the cells in table tbl with the k highest frequencies; handling
# of ties is unrefined
tabdom <- function(tbl,k) {
  # create a data frame representation of tbl, adding a Freq column
  tbldf <- as.data.frame(tbl)
  # determine the proper positions of the frequencies in a sorted order
  freqord <- order(tbldf$Freq,decreasing=TRUE)
  # rearrange the data frame in that order, and take the first k rows
  dom <- tbldf[freqord,][1:k,]
  return(dom)
}

# Sacamos una tabla con las relaciones más frecuentes de los pares 
# canal_entrada, segmento
dfTot = tabdom(table(df[c(15,21)]),20)
dfTot

# Vemos también a nivel gráfico cómo es la distribución de los segmentos 
# por canal de entrada
plot(df$segmento ~ df$canal_entrada,
     main = "Distribución del segmento por canal de entrada",
     xlab = "Canal de Entrada", ylab = "Segmento",
     col = c("red", "blue", "green"))

df51 = df
df52 = df[is.na(df51$canal_entrada),c("canal_entrada", "segmento")]
summary(df52)
# Vemos que no hay TOPs entre los NAs de canal_entrada, luego:
# Asignamos el canal_entrada KHE las observaciones cuyo segmento sea 03 - UNIVERSITARIO
# Asignamos el canal_entrada KFC las observaciones cuyo segmento sea 02 - PARTICULARES

df52$canal_entrada[df52$segmento == "02 - PARTICULARES"] = "KFC"
df52$canal_entrada[df52$segmento == "03 - UNIVERSITARIO"] = "KHE"

df51$canal_entrada[rownames(df51) %in% rownames(df52)] = df52$canal_entrada
df = df51
rm(df52)
rm(dfTot)


# Miramos ahora los NAs (26) de los campos de etiqueta ind_nomina_ult1, ind_nom_pens_ult1  
summary(df)
# Primero pasamos todas las variables a factor
for (i in 22:ncol(df)) {
  df[,i] = as.factor(df[,i])
}
str(df)

# Dado que son pocos registros en comparción con todo el df y de que la proporción
# de "0" a "1" es de 24:1 y de 22:1, optamos por convertir en "0" los NAs
df$ind_nomina_ult1[is.na(df$ind_nomina_ult1)] = "0"
df$ind_nom_pens_ult1[is.na(df$ind_nom_pens_ult1)] = "0"

summary(df)


######## Tratamiento de Outliers ############################################
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

# Los eliminamos utilizando una función:
outliers2na <- function(atributo) {
  atributo[which(atributo %in% boxplot(atributo, plot = FALSE)$out)] <- NA
  return(atributo)
}

df$antiguedad = outliers2na(df$antiguedad)

summary(df$antiguedad)


# Analizamos el campo renta
# El rango de este parámetro es muy grande, con muchos outliers (hay 29186). En este caso, 
# dependiendo de la política del banco, se podría hacer un análisis según el tipo de renta, 
# haciendo tres grupos: rentas bajas, medias y altas
plot(prop.table(table(df$renta)))
g = ggplot2::ggplot(df,aes(df$renta))
g+geom_density()

outliers_renta = boxplot.stats(df$renta)$out
boxplot(df$renta,boxwex=0.1)

df$renta = outliers2na(df$renta)

summary(df$renta)


