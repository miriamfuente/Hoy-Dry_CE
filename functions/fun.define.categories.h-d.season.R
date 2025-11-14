# @author: Miriam Fuente González
# @date: 30/12/2024
# @description: This function assigns a category to each hottest.month gridbox for each year. 
# The categories are defined by the precipitation and temperature thresholds.
# @param pr.lon.hot: Grid with the precipitation data for the hottest month
# @param tmax.lon.hot: Grid with the maximum temperature data for the hottest month

f.cat <- function(pr.lon.hot, pr50.lon.hot, pr40.lon.hot, pr30.lon.hot, pr20.lon.hot, pr10.lon.hot, t75.lon.hot, t80.lon.hot, t85.lon.hot, t90.lon.hot, t95.lon.hot, tmax.lon.hot){
    # This function assigns a category to each hottest.month gridbox
    categorias <- rep(0, dim(pr.lon.hot$Data)[which(attr(pr.lon.hot$Data, "dimensions") == "time")])
    for (k in 1:dim(pr.lon.hot$Data)[which(attr(pr.lon.hot$Data, "dimensions") == "time")]){
        tryCatch({
            if(is.na(pr.lon.hot$Data[k]) && is.na(tmax.lon.hot$Data[k])){
                categorias[k] <- NA
                next
            } else if (is.na(pr.lon.hot$Data[k]) || is.na(tmax.lon.hot$Data[k])) {
                warning(paste("Advertencia: Un valor es NA en la iteración", k, 
                        "- pr.lon.hot =", pr.lon.hot$Data[k], 
                        ", tmax.lon.hot =", tmax.lon.hot$Data[k]))
                categorias[k] <- NA
                next  # Saltar esta iteración
            }
        
            if(pr.lon.hot$Data[k] > pr50.lon.hot$Data) {
                categorias[k] <- 0 
            }
            else if (pr.lon.hot$Data[k] <= pr50.lon.hot$Data && pr.lon.hot$Data[k] > pr40.lon.hot$Data) {
                if(tmax.lon.hot$Data[k] >= t75.lon.hot$Data) {
                    categorias[k] <- 1
                }else{
                    categorias[k] <- 0
                }
            }else if (pr.lon.hot$Data[k] <= pr40.lon.hot$Data && pr.lon.hot$Data[k] > pr30.lon.hot$Data) {
                if(tmax.lon.hot$Data[k] >= t75.lon.hot$Data && tmax.lon.hot$Data[k] < t80.lon.hot$Data) {
                    categorias[k] <- 1
                }else if (tmax.lon.hot$Data[k] >= t80.lon.hot$Data) {
                    categorias[k] <- 2
                }else{
                    categorias[k] <- 0
                }
            }else if (pr.lon.hot$Data[k] <= pr30.lon.hot$Data && pr.lon.hot$Data[k] > pr20.lon.hot$Data) {
                if(tmax.lon.hot$Data[k] >= t75.lon.hot$Data && tmax.lon.hot$Data[k] < t80.lon.hot$Data) {
                    categorias[k] <- 1
                }else if(tmax.lon.hot$Data[k] >= t80.lon.hot$Data && tmax.lon.hot$Data[k] < t85.lon.hot$Data) {
                    categorias[k] <- 2
                }else if (tmax.lon.hot$Data[k] >= t85.lon.hot$Data) {
                    categorias[k] <- 3
                }else{
                    categorias[k] <- 0
                }
            }else if (pr.lon.hot$Data[k] <= pr20.lon.hot$Data && pr.lon.hot$Data[k] > pr10.lon.hot$Data) {
                if(tmax.lon.hot$Data[k] >= t75.lon.hot$Data && tmax.lon.hot$Data[k] < t80.lon.hot$Data) {
                    categorias[k] <- 1
                }else if(tmax.lon.hot$Data[k] >= t80.lon.hot$Data && tmax.lon.hot$Data[k] < t85.lon.hot$Data) {
                    categorias[k] <- 2
                }else if(tmax.lon.hot$Data[k] >= t85.lon.hot$Data && tmax.lon.hot$Data[k] < t90.lon.hot$Data) {
                    categorias[k] <- 3
                }else if (tmax.lon.hot$Data[k] >= t90.lon.hot$Data) {
                    categorias[k] <- 4
                }else{
                    categorias[k] <- 0
                }
            }else if (pr.lon.hot$Data[k] <= pr10.lon.hot$Data) {
                if(tmax.lon.hot$Data[k] >= t75.lon.hot$Data && tmax.lon.hot$Data[k] < t80.lon.hot$Data) {
                    categorias[k] <- 1
                }else if(tmax.lon.hot$Data[k] >= t80.lon.hot$Data && tmax.lon.hot$Data[k] < t85.lon.hot$Data) {
                    categorias[k] <- 2
                }else if(tmax.lon.hot$Data[k] >= t85.lon.hot$Data && tmax.lon.hot$Data[k] < t90.lon.hot$Data) {
                    categorias[k] <- 3
                }else if(tmax.lon.hot$Data[k] >= t90.lon.hot$Data && tmax.lon.hot$Data[k] < t95.lon.hot$Data) {
                    categorias[k] <- 4
                }else if (tmax.lon.hot$Data[k] >= t95.lon.hot$Data) {
                    categorias[k] <- 5
                }else{
                    categorias[k] <- 0
                }
            }
        }, error = function(e) {
            warning(paste("Error en la iteración ", k,":", e$message))
        })
    }
    return(categorias)
}