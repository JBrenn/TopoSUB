TopoSUB_droughtStat <- function(data)
{
  base <- data[[1]]
  
  base_doy <- lapply(X = base, FUN = function(x) {
    x$end_doy <- as.integer(format(as.Date(x$end), "%j"))  
  })
  
  # ttesting <- 
  #   lapply(X = data[-1], FUN = function(x){
  
  df <- rep(NA,7)
  
  for (j in 1:length(data[-1])) {
    
    out <- rep(NA,7)
    
    for (i in (1:length(base)))
    {
      # >=5  
      
      x <- data[-1][[j]]
      year <- names(data[-1])[j]
      
      # >=10  
      end_doy <- as.integer(format(as.Date(x[[i]]$end), "%j"))
      end_doy_T <- end_doy > 90 & end_doy < (365-90)
      choose_x <- end_doy_T & x[[i]]$D >= 10
      x_i <- x[[i]][choose_x,]
      
      base_doy_T <- base_doy[[i]] > 90 & base_doy[[i]] < (365-90)
      choose_y <- base_doy_T &  base[[i]]$D >= 10
      base_i <- base[[i]][choose_y,]
      
      if (sum(choose_x)<2 | sum(choose_y)<2) {
        tD_ <- tI_ <- tS_ <- c(rep(NA, 4), i, j, year)
      } else {
        tD <- t.test(x = x_i$D, y = base_i$D)
        tD_means <- round(tD$estimate,2); tD_p <- tD$p.value
        tD_ <- c(tD_p, tD_means, "D", i, j, year)
        
        tI <- t.test(x = x_i$I, y = base_i$I)
        tI_means <- round(tI$estimate,2); tI_p <- tI$p.value
        tI_ <- c(tI_p, tI_means, "I", i, j, year)
        
        tS <- t.test(x = x_i$S, y = base_i$S)
        tS_means <- round(tS$estimate,2); tS_p <- tS$p.value
        tS_ <- c(tS_p, tS_means, "S", i, j, year)
      }
      
      out <- rbind(out, tD_, tI_ ,tS_)
      
    }
    
    out <- out[-1,]
    df <- rbind(df, out)
  }
  
  return(df[,-1])
}

