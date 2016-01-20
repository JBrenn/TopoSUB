TopoSUB_spreadVAR <- function(data, var, do.zoo)
{
  
  var2select <- which(names(data) == var)
  
  # get data 2 work on
  data <- data[, c(1,2,var2select), with=FALSE]
  
  data.table::setnames(data, names(data), c(names(data)[-3],"value"))
  
  # spread data
  # rows: Date
  # cols: IDpoints
  data_spread <- data.table::dcast.data.table(data, Date ~ IDpoint, value.var = "value")
  
  # create zoo object
  if (do.zoo)
  {
    Date <- data_spread[,Date]
    data_spread <- zoo(data_spread[,Date:=NULL], Date)
  }
  
}