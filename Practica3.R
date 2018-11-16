#Ejercicio 1
library("nycflights13", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rmarkdown", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
arrdel <- (filter(flights,arr_delay>=2))
arrdel
vuelos <- (filter(flights, dest=='HOU'| dest=='IAH' ) ) #filter(flights, dest %in% c('IAH' | 'HOU'))
vuelos
operador <- filter(flights, (carrier=="AA") | (carrier=="UA") | (carrier=="DL")) #filter(flights, carrier %in% c("AA","DL","UA"))
operador
vuelosverano <- filter(flights, (month=7) & (month=8) & (month=9)) #filter(flights, month>=7,month<=9)
vuelosverano
vuelostarde <- filter(flights, (arr_delay>=120) & (dep_delay<=0))
vuelostarde
vueldel30 <- filter(flights,dep_delay>=60,dep_delay-arr_delay>30)
vueldel30
vuelomedianoche <- filter (flights,(dep_time<=600) | (dep_time==2400))  
#c (600,1200,2400) %% 2400
#filter(flights,dep_time %% 2400 <= 600)

vuelomedianoche
#Ejercicios 2
nds <- arrange(flights,desc(arr_delay))
nds <- arrange(nds,desc(dep_delay))
tail(nds,10)
nds <- filter(nds,!is.na(dep_delay))
head(nds,10)
#nds <- arrange(flights,desc(arr_delay))
#nds <- arrange(nds,!desc(dep_delay)) para ordenarlos en orden ascendente.
#nds <- filter(nds,!is.na(dep_delay))
#tail(nds,10) o head(nds,10) dependiendo de si tiene más o menos retraso
##ejercicio 2 virtualpc para mac
flights_sml <- select(flights,year:day, ends_with("delay"), distance, air_time)
select(flights,dep_time,dep_delay,arr_time,arr_delay) #select(flights,starts_with("dep"),starts_with("arr")) ***sería lo mismo***
#Specifying the column numbers of the variables ***sólo si sabes en qué columna se encuentran los valores que quieres***
select(flights, 4,5,6,9)
select(flights, matches("^(dep|arr)_(time|delay)$"))
select(flights,ends_with("delay"),ends_with("time"))##ésta más o menos te enseña lo mismo
##Todo lo anterior muestra la misma tabla.
#otro ejercicio declarando un objeto datos y añadir una columna
datos <- select(flights,air_time,arr_time,dep_time)
datos
mutate(datos,diferencia=arr_time-dep_time)##acabo de añadir una nueva columna con la diferencia de arr_time-dep_time
datos2 <- select(flights,contains("dep_"))
datos2
##PRACTICA 3.

#Ejercicio 1
select(flights,ends_with("time"))

#Ejercicio 2
#Vuelos <- colnames(flights)[colnames(flights)=="arr_delay"] <- "retraso_llegadas"
Vuelos <- (rename(flights,arr_delay="retraso_llegadas"))
flights

#Ejercicio 3
tabla1 <- arrange(flights,!desc(distance))
tabla2 <- select(tabla1,origin,dest)
head(tabla2,1)
tail(tabla2,1)

#Ejercicio 4
flights_times <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 + 
                                                 sched_dep_time %% 100) %% 1440
)


