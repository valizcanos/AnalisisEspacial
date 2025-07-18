#https://rpubs.com/JONASG/917633
library(ggplot2)
library(clhs)
library(ape)

set.seed(12345)
MO=rnorm(n = 150,mean = 3,sd = 0.5)
xy=expand.grid(x=seq(1,10),y=seq(1,15))

plot(xy,col=MO,pch=19, main = "Distribución espacial de materia Orgánica")

dfmo<-data.frame(MO, xy)
head(dfmo)

ggplot(dfmo, aes(x = x, y=y, fill = MO))+
  geom_tile( size = 3)

# Matriz de pesos

mdistancias <- as.matrix(dist(cbind(xy$x, xy$y)))#Matriz de distancias (distancia euclidea)
mdistanciasinv <- 1/mdistancias #inverso de la matriz de las distancias
diag(mdistanciasinv) <- 0 #pasar la diagona de infinito a cero
mdistanciasinv[1:5, 1:5]

# Índice de Moran
Moran.I(MO, mdistanciasinv) 
