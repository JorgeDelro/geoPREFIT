# SPATIAL ANALYSIS

# Packages
library(spdep)

## Shapefile with PREFIT cities
prov <- readOGR(dsn = "data", layer = "prov_prefit")
png("prov_PREFIT.png", width = 7, height = 5, units = 'in', res = 600)
plot(prov)
dev.off()

# Shapefile to get neighbour list
prov_manual <- readOGR(dsn = "data", layer = "Jorge_PREFIT")
png("prov_PREFIT_manual.png", width = 7, height = 5, units = 'in', res = 600)
plot(prov_manual)
dev.off()

## Neighbour list
prov.nb <- poly2nb(prov_manual)
## convertir a listw
prov.lw <- nb2listw(prov.nb)

## Orden of values: Vitoria - Almería - Mallorca - Cádiz - Castellón - Cuenca - Granada - Madrid - Las Palmas - Zaragoza
values_SLJ_test <- c(69.10078,72.58393,73.92500,62.88406,71.09690,74.96310,78.08070,75.18425,78.35769,77.84892)
values_HG_test <- c(7.311047,6.609541,8.088529,6.4,6.344231,7.408465,7.121930,6.722720,7.340385,6.910252)
values_P4X10M_test <- c(15.82122,17.15192,15.73542,17.62975,18.50204,17.40730,16.91176,16.45397,18.50204,16.57584)
values_Sol_test <- c(16.136240,12.212350,22.687893,11.559928,14.228745,15.128024,10.292042,12.416217,12.918301,9.251817)
# valorsPA_Level <- c(3.492188,3.485612,3.334728,3.392857,3.504098,3.521912,3.579505,3.915171,3.579151,3.543165)

## SLJ
moran.plot(values_SLJ_test,prov.lw)
moran.test(values_SLJ_test,prov.lw)
moran.test(values_SLJ_test,prov.lw, randomisation = F)
moran.mc(values_SLJ_test,prov.lw,10000)

## HG
moran.plot(values_HG_test,prov.lw)
moran.test(values_HG_test,prov.lw)
moran.test(values_HG_test,prov.lw, randomisation = F)
moran.mc(values_HG_test,prov.lw,10000)

## P4X10M
moran.plot(values_P4X10M_test,prov.lw)
moran.test(values_P4X10M_test,prov.lw)
moran.test(values_P4X10M_test,prov.lw, randomisation = F)
moran.mc(values_P4X10M_test,prov.lw,10000)

## Sol
moran.plot(values_Sol_test,prov.lw)
moran.test(values_Sol_test,prov.lw)
moran.test(values_Sol_test,prov.lw, randomisation = F)
moran.mc(values_Sol_test,prov.lw,10000)

## PA Levels
# moran.plot(valorsPA_Level,prov.lw)
# moran.test(valorsPA_Level,prov.lw)
# moran.test(valorsPA_Level,prov.lw, randomisation = F)
# moran.mc(valorsPA_Level,prov.lw,10000)

# sar.res <- spautolm(valorsPA_Level~1,listw = prov.lw)
# sar.res

# car.res <- spautolm(valorsPA_Level~1,listw = prov.lw,family = "CAR")
# car.res
