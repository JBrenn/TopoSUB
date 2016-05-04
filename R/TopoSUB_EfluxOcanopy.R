TopoSUB_EfluxOcanopy <- function(data, canopy_fraction=NULL)
{
  overcanopy <- list()
  
  for (i in names(data)) 
  {
    if (!is.null(canopy_fraction)) {
      overcanopy[[i]] <- canopy_fraction * (data[[i]]$g_veg + data[[i]]$veg) + (1-canopy_fraction) * data[[i]]$g_unveg
    } else {
      overcanopy[[i]] <- data[[i]]$cf * (data[[i]]$g_veg + data[[i]]$veg) + (1-data[[i]]$cf) * data[[i]]$g_unveg
    }
  }
  
  return(overcanopy)
}