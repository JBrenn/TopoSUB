# calculate climatic mean

# library(tidyr)
# library(dplyr)
# library(data.table)
# library(zoo)

TopoSUB_basinClimMean <- function(data, var, summfun, climK=30, doClimMean=TRUE)
{

data_select <- data[,c("Date12_DDMMYYYYhhmm_","IDpoint",var), with=FALSE]  
  
datasumm <- 
  data_select %>% 
    tidyr::separate(Date12_DDMMYYYYhhmm_,c("DD","MM","YYYY","hh","mm")) %>%
    dplyr::group_by(YYYY, IDpoint) %>%
    dplyr::summarise_each(funs(summfun), -DD, -MM, -hh, -mm) %>%
    data.table::setnames(old = c("YYYY", "IDpoint", var), new = c("YEAR","ID","VARofINT")) %>%
    tidyr::spread(key = ID, value = VARofINT)

datasumm_df <- as.data.frame(datasumm)

zoo_data <- zoo(datasumm_df[-dim(datasumm_df)[1],2:length(datasumm)], as.integer(datasumm_df[-dim(datasumm_df)[1],1]))

if (doClimMean)
  zoo_data <- rollmean(x = zoo_data, k = climK, fill=NA)

return(zoo_data)

}
