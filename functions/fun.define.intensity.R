# @author: Miriam Fuente González
# @date: 30/12/2024
# @description: This function assigns a category to each hottest.month gridbox for each year. 
# The categories are defined by the precipitation and temperature thresholds.
# @param pr.lon.hot: Grid with the precipitation data for the hot-dry season
# @param tmax.lon.hot: Grid with the maximum temperature data for the hot-dry season

f.int <- function(pr.lon.hot, t90.lon.hot, t95.lon.hot, tmax.lon.hot){
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

            if(pr.lon.hot$Data[k] < 1e-10){ # Si la precipitación es 0 mm (DIA SECO) depende solo de la temperatura
                if(tmax.lon.hot$Data[k] < t90.lon.hot$Data){
                    categorias[k] <- 0
                }else if(tmax.lon.hot$Data[k] >= t90.lon.hot$Data && tmax.lon.hot$Data[k] < t95.lon.hot$Data){
                    categorias[k] <- 3
                }else if (tmax.lon.hot$Data[k] >= t95.lon.hot$Data){
                    categorias[k] <- 4
                }
            }else if (pr.lon.hot$Data[k] > 1e-10){      
                if(pr.lon.hot$Data[k] >= 1){
                    if(tmax.lon.hot$Data[k] < t90.lon.hot$Data){
                        categorias[k] <- 0
                    }else if(tmax.lon.hot$Data[k] >= t90.lon.hot$Data && tmax.lon.hot$Data[k] < t95.lon.hot$Data){
                        categorias[k] <- 1
                    }else if (tmax.lon.hot$Data[k] >= t95.lon.hot$Data){
                        categorias[k] <- 2
                    }
                }else if(pr.lon.hot$Data[k] > 0 && pr.lon.hot$Data[k] < 1){ # Si la precipitación es mayor que 0 mm y menor que 1 mm
                    categorias[k] <- 5
                }
                
            }
        }, error = function(e) {
            warning(paste("Error en la iteración ", k,":", e$message))
        })
    }
    return(categorias)
}