# The data of tmax and pr have to be in estational cicles 
# 12 values per gridbox

fun.hot.dry.season <- function(tmax, pr){
    idx.lon <- list()
    idx.lat <- list()
    # aux <- climatology(tmax)
    for(i in 1:dim(tmax$Data)[which(attr(tmax$Data, "dimensions") == "lat")]){
        pr.lat <- subsetDimension(pr, dimension="lat", indices=i)
        tmax.lat <- subsetDimension(tmax, dimension="lat", indices=i)
        # aux.lat <- subsetDimension(aux, dimension="lat", indices=i)
        print(i)
        for(j in 1:dim(tmax$Data)[which(attr(tmax$Data, "dimensions") == "lon")]){
            pr.lon <- subsetDimension(pr.lat, dimension="lon", indices=j)
            tmax.lon <- subsetDimension(tmax.lat, dimension="lon", indices=j)
            aux.lon <- tmax.lon
            tryCatch({
                if (all(is.na(pr.lon$Data)) & all(is.na(tmax.lon$Data))){
                    aux.lon$Data <- array(NA, dim = c(3,1,1))
                    attr(aux.lon$Data, "dimensions") <- c("time", "lat","lon")
                }else{
                    mean.pr <- mean(pr.lon$Data)
                    sd.pr <- sd(pr.lon$Data)
                    mean.tmax <- mean(tmax.lon$Data)
                    sd.tmax <- sd(tmax.lon$Data)
                    pr.lon$Data <- (pr.lon$Data - mean.pr)/sd.pr
                    tmax.lon$Data <- (tmax.lon$Data - mean.tmax)/sd.tmax
                    # Calcular la diferencia absoluta
                    dif.tmax.pr <- tmax.lon$Data - pr.lon$Data
                    dif.tmax.pr[dif.tmax.pr <= 0] <- NA  # Ignorar valores donde tmax <= pr
                    # Encontrar el índice del máximo
                    month.max.dif <- which.max(dif.tmax.pr)
                    months.vm <- c(month.max.dif - 1, month.max.dif, month.max.dif + 1)
                    # Ajustar para que los meses estén en el rango 1-12
                    months.vm[months.vm == 0] <- 12  # Si el mes es enero, el anterior será diciembre
                    months.vm[months.vm == 13] <- 1  # Si el mes es diciembre, el siguiente será enero
                    aux.lon$Data <- array(sort(months.vm), dim = c(length(months.vm),1,1))
                    attr(aux.lon$Data, "dimensions") <- c("time", "lat","lon")
                }
            }, error = function(e) {
                warning(paste("Error en la iteración i=:", i, ", j=", j, e$message))
            })
            idx.lon[[j]] <- aux.lon
        }
        idx.lat[[i]] <- redim(do.call(bindGrid, c(idx.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
    }
    idx.hot.dry.month <- redim(do.call(bindGrid, c(idx.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

}