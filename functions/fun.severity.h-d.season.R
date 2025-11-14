fun.severity <- function(hottest.month, pr.obs, tmax.obs, pr.daily, tmax.daily) {

    # All the data must be introduced once it's transformed
    # Difference: for ERA5-LAND pr.obs == pr.daily
    # For models: pr.daily== data from historic period (transformed)
    #             pr.obs== data from future period (transformed)

    # pr.obs & tmax.obs: observed data ERA5.land
    pr.month <- aggregateGrid(pr.obs, aggr.y = list(FUN = "sum", na.rm = TRUE))
    tmax.max <- aggregateGrid(tmax.obs, aggr.y = list(FUN = "max", na.rm = TRUE))
    tmax.max$Data[is.infinite(tmax.max$Data)] <- NA

    source("functions/fun.quantiles.h-d.season.R")
    source("functions/fun.define.categories.h-d.season.R")
    # pr.daily = pr.transformed (only data in the sesason (3 months))
    pr50 <- fun.quantiles(pr.daily, var="pr", quantile=0.5)
    pr40 <- fun.quantiles(pr.daily, var="pr", quantile=0.4)
    pr30 <- fun.quantiles(pr.daily, var="pr", quantile=0.3)
    pr20 <- fun.quantiles(pr.daily, var="pr", quantile=0.2)
    pr10 <- fun.quantiles(pr.daily, var="pr", quantile=0.1)

    t75 <- fun.quantiles(tmax.daily, var="tmax", quantile=0.75)
    t80 <- fun.quantiles(tmax.daily, var="tmax", quantile=0.8)
    t85 <- fun.quantiles(tmax.daily, var="tmax", quantile=0.85)
    t90 <- fun.quantiles(tmax.daily, var="tmax", quantile=0.9)
    t95 <- fun.quantiles(tmax.daily, var="tmax", quantile=0.95)

    list.lat <- list()
    list.lon <- list()
    list.final <- list()
    start_date <- NULL
    end_date <- NULL

    for (i in 1:dim(tmax.max$Data)[which(attr(tmax.max$Data, "dimensions") == "lat")]) {
        print(paste("i= ", i))
        pr50.lat <- subsetDimension(pr50, dimension="lat", indices=i)
        pr40.lat <- subsetDimension(pr40, dimension="lat", indices=i)
        pr30.lat <- subsetDimension(pr30, dimension="lat", indices=i)
        pr20.lat <- subsetDimension(pr20, dimension="lat", indices=i)
        pr10.lat <- subsetDimension(pr10, dimension="lat", indices=i)

        t75.lat <- subsetDimension(t75, dimension="lat", indices=i)
        t80.lat <- subsetDimension(t80, dimension="lat", indices=i)
        t85.lat <- subsetDimension(t85, dimension="lat", indices=i)
        t90.lat <- subsetDimension(t90, dimension="lat", indices=i)
        t95.lat <- subsetDimension(t95, dimension="lat", indices=i)

        tmax.lat <- subsetDimension(tmax.max, dimension="lat", indices=i)
        pr.lat <- subsetDimension(pr.month, dimension="lat", indices=i)

        for(j in 1:dim(tmax.max$Data)[which(attr(tmax.max$Data, "dimensions") == "lon")]) {
            pr50.lon <- subsetDimension(pr50.lat, dimension="lon", indices=j)
            pr40.lon <- subsetDimension(pr40.lat, dimension="lon", indices=j)
            pr30.lon <- subsetDimension(pr30.lat, dimension="lon", indices=j)
            pr20.lon <- subsetDimension(pr20.lat, dimension="lon", indices=j)
            pr10.lon <- subsetDimension(pr10.lat, dimension="lon", indices=j)

            t75.lon <- subsetDimension(t75.lat, dimension="lon", indices=j)
            t80.lon <- subsetDimension(t80.lat, dimension="lon", indices=j)
            t85.lon <- subsetDimension(t85.lat, dimension="lon", indices=j)
            t90.lon <- subsetDimension(t90.lat, dimension="lon", indices=j)
            t95.lon <- subsetDimension(t95.lat, dimension="lon", indices=j)

            tmax.lon <- subsetDimension(tmax.lat, dimension="lon", indices=j)
            pr.lon <- subsetDimension(pr.lat, dimension="lon", indices=j)

                    # Determinar número de dimensiones y extraer el valor correctamente
            dims <- length(dim(hottest.month$Data))
            if (dims == 2) {
                month <- hottest.month$Data[i, j]
            } else if (dims == 3) {
                month <- hottest.month$Data[, i, j]  # Asumimos que la primera dimensión es "time"
            } else {
                stop("Número de dimensiones no soportado")
            }

            if (all(is.na(month))) {
                cat.lon.hot <- redim(tmax.lon, drop = TRUE)
            } else {
                pr50.lon.hot <- pr50.lon
                pr40.lon.hot <- pr40.lon
                pr30.lon.hot <- pr30.lon
                pr20.lon.hot <- pr20.lon
                pr10.lon.hot <- pr10.lon

                t75.lon.hot <- t75.lon
                t80.lon.hot <- t80.lon
                t85.lon.hot <- t85.lon
                t90.lon.hot <- t90.lon
                t95.lon.hot <- t95.lon

                tmax.lon.hot <- tmax.lon
                pr.lon.hot <- pr.lon

                cat.lon.hot <- tmax.lon
                
                categorias <- f.cat(pr.lon.hot, pr50.lon.hot, pr40.lon.hot, pr30.lon.hot, pr20.lon.hot, pr10.lon.hot, t75.lon.hot, t80.lon.hot, t85.lon.hot, t90.lon.hot, t95.lon.hot, tmax.lon.hot)
                cat.lon.hot$Data <- categorias
                attr(cat.lon.hot$Data, "dimensions") <- "time"
                # cat.lon <- bindGrid(tmax.lon.no.hot, cat.lon.hot, dimension="time")
            }
            
            # # Needed to assign the start and end dates to the final dataset
            # if (is.null(start_date) && is.null(end_date)) {
            #     start_date <- pr.lon$Dates$start
            #     end_date <- pr.lon$Dates$end
            # }
            
            # # Assign the start and end dates to the final dataset
            # cat.lon$Dates$start <- start_date
            # cat.lon$Dates$end <- end_date
            list.lon[[j]] <- cat.lon.hot
        }
        list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
    }
    list.final <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)

    return(list.final)
}