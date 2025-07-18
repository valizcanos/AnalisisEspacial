library(sf)
library(mapview)
library(spdep)

#https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html

##########################

map <- st_read('D:/DOCS/REPOSITORIOS/AnalisisEspacial/Clase 4_AutocorrelacionE/boston_tracts.shp')

#map =read_sf('D:/DOCS/REPOSITORIOS/AnalisisEspacial/Clase 4_AutocorrelacionE/', layer='boston_tracts')

map$vble <- map$MEDV
mapview(map, zcol = "vble")

##########################

nb <- poly2nb(map, queen = TRUE) #Reina - vecinos
nbw <- nb2listw(nb, style = "W") #Pesos


# Global Moran's I
gmoran <- moran.test(map$vble, nbw, alternative = "greater")
gmoran


gmoran[["estimate"]][["Moran I statistic"]] # Moran's I
gmoran[["statistic"]] # z-score
gmoran[["p.value"]] # p-value

##########################
#The spatially lagged value 

moran.plot(map$vble, nbw)

# Lagged value (Li) = \sum_{j=1}^{n} w_{i,j} * x_{j} 
#x_{j} = Valor de la variable en la unidad vecina j
#w_{i,j} = Peso espacial entre i y j (normalizado o binario)
#Las líneas que forman los cuadrantes denotan los promedios de x_j y los lagged

moran.plot(x = map$vble, 
           listw = nbw, 
           labels = map$TOWN, 
           xlab = 'Ingresos', 
           ylab = 'Rezagos espaciales de la media de ingresos')

##########################
#Local Moran
lmoran <- localmoran(map$vble, nbw, alternative = "greater")
head(lmoran)

map$lmI <- lmoran[, "Ii"] # local Moran's I
map$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
map$lmp <- lmoran[, "Pr(z > E(Ii))"]

mapview(map, zcol = "TOWN")
mapview(map, zcol = "vble")
mapview(map, zcol = "lmI")
mapview(map, zcol = "lmZ")
mapview(map, zcol = "lmp")

##########################
#Clusters

lmoran <- localmoran(map$vble, nbw, alternative = "two.sided")
head(lmoran)

map$lmp <- lmoran[, 5] # p-values are in column 5
mp <- moran.plot(as.vector(scale(map$vble)), nbw)
head(mp)

map$quadrant <- NA
# high-high
map[(mp$x >= 0 & mp$wx >= 0) & (map$lmp <= 0.05), "quadrant"]<- 1
# low-low
map[(mp$x <= 0 & mp$wx <= 0) & (map$lmp <= 0.05), "quadrant"]<- 2
# high-low
map[(mp$x >= 0 & mp$wx <= 0) & (map$lmp <= 0.05), "quadrant"]<- 3
# low-high
map[(mp$x <= 0 & mp$wx >= 0) & (map$lmp <= 0.05), "quadrant"]<- 4
# non-significant
map[(map$lmp > 0.05), "quadrant"] <- 5

mapview(map, zcol = "quadrant")
