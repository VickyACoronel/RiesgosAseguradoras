## La función sería de x sería:
rpareto<-function(n,alfa,beta) {
  u<-runif(n)
  x<-beta*((1/u^(1/alfa))-1)
  return(x)
}

# y para propósitos de comparación:
dpareto<-function(x,alfa,beta) {
  t<-(alfa*beta^alfa)/(x+beta)^(alfa+1)
  return(t)
  
}

# ejecuta las funciones anteriores y produce un gráfico de comparación:
set.seed(123)
beta<-100000
alfa<-3
n<-1000
muestra<-rpareto(n,alfa,beta)
hist(muestra,prob=T,breaks="Scott",xlab="Monto reclamo",main="Densidad de Pareto (3,100000)")
curve(dpareto(x,alfa,beta),add=T,col="red")

# calcula el balance para el período considerado de 5 años
balance<-function(inicio=1000000, p=0.1,nclientes=1000,precio_poliza=5500,years=5) {
  polizas<-precio_poliza*nclientes
  z<-numeric(years)
  zini<-1000000
  for (i in 1:years) {
    u<-runif(nclientes)
    reclamos<-u<=p
    nreclamos<- sum(reclamos)
    montos<-rpareto(nreclamos,alfa,beta)
    total.reclamos<-sum(montos)
    if (i==1) z[i]<-zini + polizas -total.reclamos
    else z[i]<-z[i-1] + polizas -total.reclamos
    if (z[i] < 0 ) {
      z[i]<-0 
      break()
    }
  }
  return(z)
}

## llamada a la funcion
balance()

# gráfico con varias realizaciones de Z vs. tiempo:
nrealiza<-10
varias<-matrix(0,nrow=nrealiza,ncol=6)
for (j in 1:nrealiza) {
  varias[j,2:6]<-balance()
  varias[j,1]<-1000000
}
maximo<-max(varias)
minimo<-min(varias)
plot(0:5,varias[1,],type="n",xlim=c(0,5),ylim=c(minimo,maximo),xlab="Tiempo",ylab="Ganancias")
for (j in 1:nrealiza) lines(0:5, varias[j,],col=j,lty=j,lwd=2)

# Para estimar la probabilidad de quiebra y las ganancias esperadas 
# corremos el algoritmo de balance muchas veces:
m<-1000
historia<-numeric(m)
quiebra<-0
for (i in 1:m) {
  cincoY<-balance()
  if (any (cincoY==0)) quiebra<-quiebra + 1
  historia[i] <- mean(cincoY)
}

# La probabilidad de quiebra es entonces:
pquiebra<-quiebra/m
pquiebra

#Error estandar de la probabilidad de quiebra:
sqrt((1-pquiebra)*pquiebra/m)

# Estimar las ganancias esperadas al finalizar el período de 5 años.
gananciaE<-mean(historia)
gananciaE

