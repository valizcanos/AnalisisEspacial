#https://mgimond-github-io.translate.goog/simple_moransI_example/?_x_tr_sl=en&_x_tr_tl=es&_x_tr_hl=es&_x_tr_pto=tc

library(sf)
library(spdep)
library(tmap)
library(ggplot2)

s <- readRDS(url("https://github.com/mgimond/Data/raw/gh-pages/Exercises/nhme.rds"))
# s <- st_read( "NHME.shp") #Opción para cargar desde el pc
names(s)
s$Income

hist(s$Income, main=NULL)
boxplot(s$Income, horizontal = TRUE)


tm_shape(s) + tm_fill(col="Income", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)

# Mapa de ingresos con ggplot2
ggplot(s) +
  geom_sf(aes(fill = Income), color = NA) +  # 'color = NA' elimina bordes
  scale_fill_gradientn(colors = hcl.colors(8, "Greens"), 
                       breaks = quantile(s$Income, probs = seq(0, 1, length.out = 8))) +
  theme_minimal() +
  labs(title = "Distribución de Ingresos", fill = "Income")


#Definir vecinos
nb <- poly2nb(s, queen=TRUE)
nb[1]

#Asingar Pesos 
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

# Índice de Moran


I <- moran(s$Income, lw, length(nb), Szero(lw))[1]
I

moran.test(s$Income,lw, alternative="greater")

# Crear un dataframe con los resultados

moran_result <- moran.test(s$Income, lw, alternative = "greater")

results_df <- data.frame(
  Estadístico = c("Moran I", "Valor p"),
  Valor = c(moran_result$estimate[1], moran_result$p.value)
)

