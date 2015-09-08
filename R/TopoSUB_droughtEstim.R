TopoSUB_droughtEstim <- function(x)  {
  rl <- rle(x$VAR_QU10)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x$VAR_QU10
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  consDrought <- x$VAR_QU10 * cumsum(z)
  
  # local maxima
  localmax <- which(diff(sign(diff(consDrought)))==-2)+1
  
  # duration of "drought" D, start ans end day
  D <- consDrought[localmax]
  localmax <- ifelse(D<5, NA, localmax)
  localmax <- localmax[!is.na(localmax)]
  D <- consDrought[localmax]
  
  endDay <- as.Date(x$Date[localmax], origin=as.Date("1970-01-01"))
  startDay <- endDay - D + 1
  
  # Intensity I
  I <- c()
  for (i in 1:length(localmax))
  {
    qu10 <- x$QU10[(localmax[i]-D[i]+1):localmax[i]]
    var  <- x$VAR[(localmax[i]-D[i]+1):localmax[i]]
    I <- c(I,sum(qu10 - var) / D[i])
  }
  
  # Severity S
  S = I * D
  
  df <- as.data.frame(localmax)
  df$localmax <- localmax; df$start <- startDay; df$end <- endDay; df$D <- D; df$I = I; df$S = S
  
  return(df)
}