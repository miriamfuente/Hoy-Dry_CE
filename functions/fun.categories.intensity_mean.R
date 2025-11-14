# This function is used to define the severity of the compound events
fun.categories.intesity_mean <- function(cat, hottest.month){
    library(lubridate)
    months.no.hottest <- list()
    list.c0.lon <- list()
    list.c1.lon <- list()
    list.c2.lon <- list()
    list.c3.lon <- list()
    list.c4.lon <- list()
    list.c5.lon <- list()
 

    list.c0.lat <- list()
    list.c1.lat <- list()
    list.c2.lat <- list()
    list.c3.lat <- list()
    list.c4.lat <- list()
    list.c5.lat <- list()
   

    list.c0 <- list()
    list.c1 <- list()
    list.c2 <- list()
    list.c3 <- list()
    list.c4 <- list()
    list.c5 <- list()

    c0.int <- list()
    c1.int <- list()
    c2.int <- list()
    c3.int <- list()
    c4.int <- list()
    c5.int <- list()


    meses <- 1:12

    
    for (i in 1:dim(cat$Data)[which(attr(cat$Data, "dimensions") == "lat")]) {
        cat.lat <- subsetDimension(cat, dimension="lat", indices=i)
        for (j in 1:dim(cat$Data)[which(attr(cat$Data, "dimensions") == "lon")]) {
            cat.lon <- subsetDimension(cat.lat, dimension="lon", indices=j)
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
                c1.final <- redim(climatology(cat.lon), drop=TRUE)
                attr(c1.final$Data, "dimensions") <- c("lon")
                c0.final <- c1.final
                c2.final <- c1.final
                c3.final <- c1.final
                c4.final <- c1.final
                c5.final <- c1.final
                
            } else {
                years <- unique(year(cat.lon$Dates$start))

                for(t in seq_along(years)){
                    k <- years[t]
                    cat.lon.y <- subsetGrid(cat.lon, years = k)
                    cat.hottest <- cat.lon.y
                    c0 <- climatology(cat.hottest)
                    c1 <- climatology(cat.hottest)
                    c2 <- climatology(cat.hottest)
                    c3 <- climatology(cat.hottest)
                    c4 <- climatology(cat.hottest)
                    c5 <- climatology(cat.hottest)
                    freq <- table(factor(cat.hottest$Data, levels=0:5))
                    c0$Data <- as.numeric(freq["0"])
                    attr(c0$Data, "dimensions") <- c("lon")
                    c1$Data<- as.numeric(freq["1"])
                    attr(c1$Data, "dimensions") <- c("lon")
                    c2$Data <- as.numeric(freq["2"])
                    attr(c2$Data, "dimensions") <- c("lon")
                    c3$Data <- as.numeric(freq["3"])
                    attr(c3$Data, "dimensions") <- c("lon")
                    c4$Data <- as.numeric(freq["4"])
                    attr(c4$Data, "dimensions") <- c("lon")
                    c5$Data <- as.numeric(freq["5"])
                    attr(c5$Data, "dimensions") <- c("lon")
                    c0.int[[t]] <- c0
                    c1.int[[t]] <- c1
                    c2.int[[t]] <- c2
                    c3.int[[t]] <- c3
                    c4.int[[t]] <- c4
                    c5.int[[t]] <- c5
                }
                c0.final <- redim(climatology(redim(do.call(bindGrid, c(c0.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c0.final$Data, "dimensions") <- c("lon")
                c1.final <- redim(climatology(redim(do.call(bindGrid, c(c1.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c1.final$Data, "dimensions") <- c("lon")
                c2.final <- redim(climatology(redim(do.call(bindGrid, c(c2.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c2.final$Data, "dimensions") <- c("lon")
                c3.final <- redim(climatology(redim(do.call(bindGrid, c(c3.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c3.final$Data, "dimensions") <- c("lon")
                c4.final <- redim(climatology(redim(do.call(bindGrid, c(c4.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c4.final$Data, "dimensions") <- c("lon")
                c5.final <- redim(climatology(redim(do.call(bindGrid, c(c5.int, list(dimension = "time", skip.temporal.check = FALSE))), drop = TRUE)), drop = TRUE)
                attr(c5.final$Data, "dimensions") <- c("lon")


                # cat.hottest <-cat.lon
                # c0 <- climatology(cat.hottest)
                # c1 <- climatology(cat.hottest)
                # c2 <- climatology(cat.hottest)
                # c3 <- climatology(cat.hottest)
                # c4 <- climatology(cat.hottest)
                # c5 <- climatology(cat.hottest)
                # freq <- table(factor(cat.hottest$Data, levels=0:5))
                # c0$Data <- as.numeric(freq["0"])
                # attr(c0$Data, "dimensions") <- c("lon")
                # c1$Data<- as.numeric(freq["1"])
                # attr(c1$Data, "dimensions") <- c("lon")
                # c2$Data <- as.numeric(freq["2"])
                # attr(c2$Data, "dimensions") <- c("lon")
                # c3$Data <- as.numeric(freq["3"])
                # attr(c3$Data, "dimensions") <- c("lon")
                # c4$Data <- as.numeric(freq["4"])
                # attr(c4$Data, "dimensions") <- c("lon")
                # c5$Data <- as.numeric(freq["5"])
                # attr(c5$Data, "dimensions") <- c("lon")
                # c0.final <- c0
                # c1.final <- c1
                # c2.final <- c2
                # c3.final <- c3
                # c4.final <- c4
                # c5.final <- c5
            }
            list.c0.lon[[j]] <- c0.final
            list.c1.lon[[j]] <- c1.final
            list.c2.lon[[j]] <- c2.final
            list.c3.lon[[j]] <- c3.final
            list.c4.lon[[j]] <- c4.final
            list.c5.lon[[j]] <- c5.final
        }


        list.c0.lat[[i]] <- redim(do.call(bindGrid, c(list.c0.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c1.lat[[i]] <- redim(do.call(bindGrid, c(list.c1.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c2.lat[[i]] <- redim(do.call(bindGrid, c(list.c2.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c3.lat[[i]] <- redim(do.call(bindGrid, c(list.c3.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c4.lat[[i]] <- redim(do.call(bindGrid, c(list.c4.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c5.lat[[i]] <- redim(do.call(bindGrid, c(list.c5.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
    }
    list.c0 <- redim(do.call(bindGrid, c(list.c0.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c1 <- redim(do.call(bindGrid, c(list.c1.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c2 <- redim(do.call(bindGrid, c(list.c2.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c3 <- redim(do.call(bindGrid, c(list.c3.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c4 <- redim(do.call(bindGrid, c(list.c4.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c5 <- redim(do.call(bindGrid, c(list.c5.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)

    return(list(c0 = list.c0, c1 = list.c1, c2 = list.c2, c3 = list.c3, c4 = list.c4, c5 = list.c5))

}