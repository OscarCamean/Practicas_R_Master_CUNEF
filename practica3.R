# Práctica 3 - Ejercicios dplyr Data Transformation
# Programación en R
# Óscar Camean Eddine

#### LIBRERIAS ####
library("nycflights13", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rmarkdown", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
#### EJERCICIOS CLASE ####

# 1.-Seleccionar todos los vuelos cuyo retraso sea igual o mayor a dos horas
arrdel <- (filter(flights,arr_delay >= 2))
arrdel

# 2.-Filtrar por destino, todos los que vayan dirección HOU o IAH
vuelos <- (filter(flights, dest == 'HOU' | dest == 'IAH' ) ) #filter(flights, dest %in% c('IAH' | 'HOU'))
vuelos

# 3.-Filtrar por operador. Los vuelos que hayan hecho las compañías "AA","UA" y "DL"
operador <- filter(flights, (carrier == "AA") | (carrier == "UA") | (carrier == "DL")) #filter(flights, carrier %in% c("AA","DL","UA"))
operador

# 4.-Filtrar por vuelos hechos en verano, los meses 7,8 y 9.
vuelosverano <- filter(flights, (month = 7) & (month = 8) & (month = 9)) #filter(flights, month>=7,month<=9)
vuelosverano

# 5.-Filtrar los vuelos que hayan sufrido algún retraso durante el vuelo.
vuelostarde <- filter(flights, (arr_delay >= 120) & (dep_delay <= 0))
vuelostarde

# 6.-Filtrar aquellos vuelos que hayan tenido una hora o más de retraso
vueldel30 <- filter(flights,dep_delay >= 60,dep_delay - arr_delay > 30)
vueldel30

# 7.-Vuelos realizados por la noche.
vuelomedianoche <- filter(flights,(dep_time <= 600) | (dep_time == 2400))  
vuelomedianoche

# 8.-Filtrar para encontrar el vuelo más retrasado y el que salió más tarde.
vuelotarde <- arrange(flights, desc(arr_delay))
vuelotarde
vuelopronto <- arrange(flights, desc(dep_time ))
vuelopronto

# 9.-Filtra para encontrar el vuelo más rápido
vuelorapido <- arrange(flights, desc(distance/air_time*60))
vuelorapido

# 10.-¿Qué vuelo ha recorrido más distancia y cual menos?
largo <- arrange(flights, desc(arr_time - dep_time))
largo
corto <- arrange(flights, (arr_time - dep_time))
corto

#### EJERCICIOS DIAPOSITIVAS ####

### 1.-Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.

# Modo 1
select(flights, 4, 5, 6, 9)

# Modo 2
variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
variables

# Modo 3
select(flights, dep_time, dep_delay, arr_time, arr_delay)

# Modo 4
select(flights, one_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))

# Modo 5
select(flights, one_of(variables))

# Modo 6
select(flights, starts_with("dep_"), starts_with("arr_"))

### 2.-Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
flights_airtime <- mutate(flights,
                          dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                          arr_time_min = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                          air_time_diff = air_time - arr_time + dep_time)
flights_airtime

### 3.-Agrupar por destino
destino <- group_by(flights, dest)
destino

# 2. Summarize to compute distance, average delay, and number of flights
delay <- summarize(by_dest, count = n(), 
                   dist = mean(distance, na.rm = TRUE), 
                   delay = mean(arr_delay, na.rm = TRUE))

# 3. Filter to remove noisypoints and Honolulu airport, which is almost twice as far awayas the next closest airport.
delay <- filter(delay,count > 20,dest != 'HNL')
delay

# Otro modo
delays <- flights %>%
  group_by(dest) %>%
  summarize(count = n(), dist = mean(distance,na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != 'HNL')

delays

# Resumen del retraso medio diario de los vuelos no cancelados
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled

not_cancelled %>%
  group_by(year, month,day) %>%
  summarize(mean = mean(dep_delay))

not_cancelled

#### EJERCICIOS PRACTICA 3 ####

nds <- arrange(flights,desc(arr_delay))
nds <- arrange(nds,desc(dep_delay))
nds <- filter(nds,!is.na(dep_delay))
head(nds,10)

## 1.-Seleccionar todas las variables que acaben en ‘time’ de la base de datos flights de nycflights13
TimeVar <- select(flights, ends_with('time'))
TimeVar

## 2.-Renombrar la variable arr_delay con retraso_llegada de flights y almacenar en un nuevo objeto llamado Vuelos
Vuelos <- mutate(flights, retraso_llegada = arr_delay)
Vuelos

## 3.-Ordenar Vuelos de menor a mayor distancia recorrida
tabla1 <- Vuelos %>% arrange(Vuelos$distance,!desc(Vuelos$distance))
tabla1

## 4.-Encontrar el origen y el destino del vuelo de mayor distancia recorrida y el de menor distancia recorrida
head(tabla1,1)
#vuelo número 1632 con operador US y distancia 17.
tail(tabla1,1)
#vuelo número 51 con operador HA y distancia 4983.

## 5.- Añadir a la base de datos Vuelos la velocidad media del vuelo como una variable calculada a partir del espacio y el tiempo.
Vuelos2 <- mutate(flights, velocidadmedia = distance / air_time * 60)
Vuelos2





