dni <- c(4,5,3,4,0,9,0,3)
unicos <- length(unique(dni))
unicos

ordenado <- sort(dni)
ordenado

cuenta <- 1
for(i in 1:(length(dni)-1)){
  if(ordenado[i]<ordenado[i+1])
    cuenta <- cuenta+1
}
cuenta

secuencia <- seq(0.5,4,0.1)
logaritmos <- log10(secuencia)
exponenciales <- exp(secuencia)
tabla <- data.frame(secuencia,logaritmos,exponenciales)
tabla
plot(secuencia,logaritmos,col="red",type="l")
abline(h=0,col=3)
plot(secuencia,exponenciales,col="red",type="l")
abline(h=0,col=3)