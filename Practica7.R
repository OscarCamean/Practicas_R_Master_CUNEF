# Práctica 7 
# Programación en R
# Óscar Camean Eddine

#### LIBRERIAS ####
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(chron)
library(rmarkdown)
library(MASS)
library(car)

#### EJERCICIOS ####

# 1.-Crear un proyecto llamado práctica 7 
# 2.-Crear nuevo script y cargar librerías.
# 3.-Crear funcion que calcule el módulo de un número.
modulo <-  function(x){
  y <- x %% 4
  return(y)
}

modulo(45340903)

# 4.-Carga el dataset proporcionado “datos_XXX.csv”, A para módulo 4 igual a 0, B para módulo 4 igual a 1, 
# C para módulo 4 igual a 2 y D para módulo 4 igual a 3.

basedatos <- read.csv("/Users/oscarcameaneddine/Desktop/datos_XXD.csv", header = T)

# 5.-Echa un vistazo a los datos para familiarizarte con ellos. Escribe directivas para ello. 
# ¿Cuántas variables tiene? ¿Cuántos registros tiene? Visualiza en tipo de las variables y la estructura del dataset.

class(basedatos) # Es un dataframe
length(basedatos) #180 variables y 127 observaciones
str(basedatos)

# 6.-Elimina todas las variables que tengan mas de un 80% de NAs. Para ello, debes contar el número de NAs usando código R. 
# Almacena los resultados en "datos_1". Guardar el dataset en un archivo csv con el mismo nombre.

contarNA <- function(lista){
  return(sum(is.na(lista)))
}

porcentajeNA <- function(lista){
  return(contarNA(lista)/length(lista))
}


elimina <- c(0)

datos_1 <- basedatos

for (i in 1:length(basedatos)) {
  porcentaje <- porcentajeNA(basedatos[,i])
  if (porcentaje >= 0.8) elimina[i] = TRUE
  else elimina[i] = FALSE
}

datos_1 <-  datos_1[,-elimina == 0]

head(datos_1)

# 7.-Crea un nuevo dataset llamado “datos_2” con las primeras 13 variables, es decir, desde “fecha” hasta “lentitud”.
datos_2 <- read.csv("/Users/oscarcameaneddine/Desktop/datos_XXD.csv", header = T)
datos_2 <- datos_2[,2:13]

# 8.-Renombra en inglés las variables para evitar tildes y otros símbolos.
names(datos_2) = c("date","mood","anxiety","irritability","focus","tobacco",
                 "caffein","sleep","wake up","motivation","quality of sleep","slowness")

datos_2

# 9.-Utiliza “ggplot2” con  boxplot, etc.  para calcular la mediana, los cuartiles y los datos atípicos de 3 variables 
# de tu elección. Crea gráficos de frecuencias para las variables categóricas e histogramas para las variables continuas. 
# Comenta en cada caso tus observaciones. Guarda las imágenes en archivos png dentro de tu proyecto.

# Creo 3 objetos, cada uno con tres variables de la base de datos.
z <- datos_2["sleep"];y <- datos_2["caffein"];x <- datos_2["wake up"]

# Divido la ventana en 3 filas y una columna para representar los 3 gráficos.
par(mfrow = c(3,1))

# Represento con un boxplot los tres objetos.
gr1 <- boxplot(z,horizontal = T, xlab = "Sleep")
gr2 <- boxplot(y,horizontal = T, xlab = "Caffein")
gr3 <- boxplot(x,horizontal = T, xlab = "Wake up")

# Calcular mediana, cuartiles y datos atípicos
summary(z);summary(y);summary(x)

# Observamos algunos datos extremos en la variable 'sleep', ya que la mayoría de valores
# se encuentran  alrededor de 2 y sin embargo hay 8 observaciones de 23 h cada una.

# En cuanto a la segunda gráfica,variable 'caffein',se observa un dato atípico con valor=120
# y algunos valores extremos alrededor de 75 y 90.

# En la tercera representación gráfica no hay valores atípicos, el intevalo de valores va 
# desde 7 hasta 11.5

#Guardamos los gráficos como archivos .png
png("Gráfico boxplot.png")
gr1 <- boxplot(z,horizontal = T, xlab = "Sleep")
dev.off()

png("Gráfico boxplot 2.png")
gr2 <- boxplot(y,horizontal = T, xlab = "Caffein")
dev.off()

png("Gráfico boxplot 3.png")
gr3 <- boxplot(z,horizontal = T, xlab = "Sleep")
dev.off()

# 10.-Añade una nueva variable “sleep_time” que contenga la duración del sueño, es decir desde 
# “dormir” hasta “despertar”. Para ello, deberas escribir una función “total_sleep” con R 
# que dados esos datos calcule correctamente los minutos de sueño y aplicarla para crear 
# la nueva columna “sleep_time”.
datos_2["wake up"] <- round(datos_2["wake up"], 2)
datos_2["sleep"] <- round(datos_2["sleep"], 2)
datos_2["sleep_time"] <- datos_2['wake up'] - datos_2["sleep"]

# 11.-Encuentra la variable que mejor regresión lineal presente con “sleep_time”, muestra el resultado 
# gráficamente y guarda la imagen en un archivo png correctamente descrito (con título, ejes,…). 
scatterplotMatrix(datos_2)

par(mfrow = c(2,2))

regresion <- lm(sleep_time~sleep, data = datos_2)
plot(regresion)

png("Gráfico regresion.png")
gr4 <- plot(regresion)
dev.off()





