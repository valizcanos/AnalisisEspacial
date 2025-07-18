library(spData)
library(sf)
library(mapview)
library(terra)
library(spdep)
library(spData)


##########################

if (requireNamespace("spdep", quietly = TRUE)) {
  data(boston)
  hr0 <- lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + I(NOX^2) + I(RM^2) +
              AGE + log(DIS) + log(RAD) + TAX + PTRATIO + B + log(LSTAT), data = boston.c)
  summary(hr0)
  logLik(hr0)
  gp0 <- lm(log(CMEDV) ~ CRIM + ZN + INDUS + CHAS + I(NOX^2) + I(RM^2) +
              AGE + log(DIS) + log(RAD) + TAX + PTRATIO + B + log(LSTAT), data = boston.c)
  summary(gp0)
  logLik(gp0)
  spdep::lm.morantest(hr0, spdep::nb2listw(boston.soi))
}
if (requireNamespace("sf", quietly = TRUE)) {
  boston.tr <- sf::st_read(system.file("shapes/boston_tracts.gpkg",
                                       package="spData")[1])
  if (requireNamespace("spdep", quietly = TRUE)) {
    boston_nb <- spdep::poly2nb(boston.tr)
  }
}

st_write(boston.tr, "C:/Users/victor.lizcano/Downloads/AME/boston_tracts.shp")

map <- st_read(system.file("C:/Users/victor.lizcano/Downloads/AME/boston_tracts.shp",
                           package = "spData"), quiet = TRUE)


map =read_sf('C:/Users/victor.lizcano/Downloads/AME/', layer='boston_tracts')

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

##########################
#Local Moran
lmoran <- localmoran(map$vble, nbw, alternative = "greater")
head(lmoran)

map$lmI <- lmoran[, "Ii"] # local Moran's I
map$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
map$lmp <- lmoran[, "Pr(z > E(Ii))"]

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
