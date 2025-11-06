options(java.parameters = "-Xmx32g") 
library(rJava)
.jinit()

# Cargar librerías
library(loadeR)
library(transformeR)
library(visualizeR)

# library(climate4R.UDG)
# library(dplyr)
# library(climate4R.value)
# library(VALUE)
# library(downscaleR)
# library(climate4R.indices)
# library(geoprocessoR)
# library(rgdal)
# library(sp)
# library(RColorBrewer)
# library(yaml)


df <- read.csv("data_inventory.csv")

# subset(df, source == "ERA5-Land")

subset.pi <- subset(df, dataset == "ERA5-Land_Iberia_day")
lon.pi <- as.character(subset.pi$endpoint)

years <- 1986:2005

# Ajustamos latitud y longitud (Sur este de españa)
latitude <- c(39.8, 43)
longitude <- c(-2.3, 3.6)

tasmax <- loadGridData(dataset=lon.pi, var ="t2mx", years = years, latLim = latitude, lonLim = longitude)
tasmax <- gridArithmetics(tasmax, 273.15, operator="-")  # Convertir a °C

pr <- loadGridData(dataset=lon.pi, var ="tp", years = years, latLim = latitude, lonLim = longitude)
pr <- gridArithmetics(pr, 1000, operator="*")  # Convertir a mm/day

pr$Data[pr$Data < 1] <- 0


mask <- climatology(tasmax)
mask$Data[!is.na(mask$Data)] <- 1


# Función para obtener los meses de la ventana móvil
ventana.movil <- function(mes) {
  meses <- c((mes-1)%%12, mes%%12, (mes+1)%%12)  # Mes anterior, actual y siguiente
  meses[meses == 0] <- 12  # Corrige diciembre (cuando (mes-1)%%12 da 0)
  return(sort(meses))
}

pr.vm <- list()
tmax.vm <- list()

for(i in 1:12){
  pr.mes <- subsetGrid(pr, season=ventana.movil(i))
  tmax.mes <- subsetGrid(tasmax, season=ventana.movil(i))
  pr.mes <- aggregateGrid(grid = pr.mes, aggr.y = list(FUN = "sum", na.rm = FALSE))
  tmax.mes <- aggregateGrid(grid = tmax.mes, aggr.y = list(FUN = "mean", na.rm = FALSE))
  pr.clim <- climatology(pr.mes)
  tmax.clim <- climatology(tmax.mes)
  pr.vm[[i]] <- pr.clim
  tmax.vm[[i]] <- tmax.clim
}
tmax.ciclo.estacional <- do.call(bindGrid, c(tmax.vm, list(dimension = "time")))
pr.ciclo.estacional <- do.call(bindGrid, c(pr.vm, list(dimension = "time")))

source("functions/fun.hot.dry.season.R")
hottest.month <- fun.hot.dry.season(tmax.ciclo.estacional, pr.ciclo.estacional)

year.range <- paste0(min(years), "-", max(years))

# a <- hottest.month
# a$Data <- hottest.month$Data[2,,]
# attr(a$Data, "dimensions") <- c("lat","lon")

# png("prueba2.png")
# spatialPlot(a, set.min=0.5, set.max=12.5, at=seq(0.5,12.5,1), color.theme="jet.colors")
# dev.off()


ht <- list()
# Rellenar cada capa temporal con la misma máscara
for (i in 1:dim(hottest.month$Data)[which(attr(hottest.month$Data, "dimensions") == "time")]) {
  ht.m <- subsetDimension(hottest.month, dimension="time", indices=i)
  ht.m <- gridArithmetics(ht.m, mask, operator="*")
  ht[[i]] <- ht.m
}

hottest.month <- redim(do.call(bindGrid, c(ht, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)

# Procesamiento
source("functions/fun.transform.data.R")
pr.tf <- transform.data(pr, hottest.month)
tmax.tf <- transform.data(tasmax, hottest.month)

print("Transformed data: Done")

# Cuantiles históricos
source("functions/fun.quantiles.h-d.season.R")
pr.q50 <- fun.quantiles(pr.tf, var = "pr", quantile = 0.5)
tmax.q90 <- fun.quantiles(tmax.tf, var = "tmax", quantile = 0.9)

print("Quantiles: Done")

# Binarización y máscara
pr.monthly <- aggregateGrid(pr.tf, aggr.y = list(FUN = "sum", na.rm = TRUE))
# pr.monthly$Dates$start <- as.Date(c(1986:2005))

pr.mask <- list()
for (i in 1:dim(pr.monthly$Data)[which(attr(pr.monthly$Data, "dimensions") == "time")]){
  pr.year <- subsetGrid(pr.monthly, years = years[i])
  pr.masked <- gridArithmetics(pr.year, mask, operator = "*")
  pr.mask[[i]] <- pr.masked
}
pr.monthly.masked <- redim(do.call(bindGrid, c(pr.mask, list(dimension = "time", skip.temporal.check = TRUE))), drop = TRUE)

source("functions/fun.binarization.h-d.season.R")
pr.bin <- fun.binarization(dataset = pr.monthly.masked, hottest.month = hottest.month, quantile = pr.q50, var = "pr", milimetros)
# saveRDS(pr.bin, paste0(path, "pr.bin.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")

tmax.bin <- fun.binarization(dataset = tmax.tf, hottest.month = hottest.month, quantile = tmax.q90, var = "tmax", milimetros)
# saveRDS(tmax.bin, paste0(path, "tmax.bin.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")

print("Binarization: Done")

# Frecuencia
source("functions/fun.ce.frecuency.h-d.season.R")
frecuency <- fun.ce.frecuency(hottest.month = hottest.month, tmax.bin = tmax.bin, pr.bin = pr.bin)
# saveRDS(frecuency, paste0(path, "ce.frecuency.era5-land.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")

print("Frecuency: Done")

# Pintamos la frecuencia media
frecuency.mean <- aggregateGrid(frecuency, aggr.y = list(FUN = "sum", na.rm = TRUE))
frequency.mean <- climatology(frecuency.mean)
frequency.mean <- gridArithmetics(frequency.mean, mask, operator="*")

png("frequency_mean.png")
spatialPlot(frequency.mean, set.min=0, set.max=12, at=seq(0, 12, 1), color.theme="YlOrRd", main="Frecuencia media de eventos hot-dry (1986-2005)")
dev.off()

# # Duración
# source("/gpfs/users/fuentem/functions/fun.duration.h-d.season.R")
# racha <- 2
# duration <- fun.duration(frecuency, hottest.month, racha, years)
# saveRDS(duration, paste0(path, "ce.duration.era5-land.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")

# print("Duration: Done")

# # Severidad
# source("/gpfs/users/fuentem/functions/fun.severity.h-d.season.R")
# severity <- fun.severity(hottest.month, pr.obs = pr.tf.hist, tmax.obs = tmax.tf.hist, pr.daily = pr.tf.hist, tmax.daily = tmax.tf.hist)
# saveRDS(severity, paste0(path, "ce.severity.era5-land.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")

# print("Severity: Done")

# # Categorías
# source("/gpfs/users/fuentem/functions/fun.categories.h-d.season.R")
# categories <- fun.categories(severity, hottest.month)
# saveRDS(categories, paste0(path, "ce.categories.era5-land.HOT.DRY.station.ventana.movil.", year.range, ".", region, ".rds"), compress = "xz")



# print(paste0(path, "ce.categories.era5-land.HOT.DRY.station.ventana.movil.RAW.DATA.", year.range, ".", region, ".rds"))