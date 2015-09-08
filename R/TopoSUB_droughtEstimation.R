# libs
# library(dplyr)
# library(data.table)
# library(chron)

# source
# "drought_estim.R"

TopoSUB_droughtEstimation <- function(data_) {
  
  # month 
  data_[,Month := as.numeric(substr(Date12_DDMMYYYYhhmm_,4,5))]
  
  # year
  data_[,Year := as.numeric(substr(Date12_DDMMYYYYhhmm_,7,10))]
  start_year <- data_$Year[1]
  end_year   <- tail(data_$Year,1)-1
  
  # yearmon
  data_[,YearMon := substr(Date12_DDMMYYYYhhmm_,4,10)]
  
  # numeric dates in data table
  data_[,Date12_DDMMYYYYhhmm_ := as.numeric(as.Date(substr(Date12_DDMMYYYYhhmm_,1,10), "%d/%m/%Y"))]
  
  # baseline 1970-2010
  start_base <- as.numeric(as.Date(paste(start_year, "-01-01", sep=""), "%Y-%m-%d"))
  end_base   <- as.numeric(as.Date(paste(start_year+30-1, "-12-31", sep=""), "%Y-%m-%d"))
  
  quant <- function(x) quantile(x, probs=c(0.1))
  
  base_Q10 <-
  data_ %>%
    setnames(old = names(data_), new = c("Date","IDpoint","VAR", "MONTH", "YEAR", "YEARMON")) %>%
    filter(Date >= start_base & Date <= end_base) %>%
    group_by(IDpoint, MONTH) %>%
    summarise(QU10=quant(VAR))
  
  data_[, toJoin := paste(IDpoint, MONTH, sep="_")]
  base_Q10 <- base_Q10[, toJoin := paste(IDpoint, MONTH, sep="_")]
  
  data_ <-
    inner_join(data_, base_Q10[,c("QU10", "toJoin"), with=F], by = "toJoin") %>%
      mutate(VAR_QU10 = VAR < QU10)
  
  #1 filter date
  result <- list()
  
  for (clm_per in start_year:(end_year-30))
  {
    start_date <- as.numeric(as.Date(paste(clm_per, "-01-01", sep=""), "%Y-%m-%d"))
    end_date   <- as.numeric(as.Date(paste(clm_per+30-1, "-01-01", sep=""), "%Y-%m-%d"))
    
    clm_period <- 
      data_ %>%
      filter(Date >= start_date & Date <= end_date) %>%
      group_by(IDpoint)
    clm_period <- clm_period[order(Date)]
    
    clm_period_sp <- split(as.data.frame(clm_period), clm_period$IDpoint)
    #2 run drought_estim by IDpoint
    
    result[[paste(clm_per, clm_per+30-1, sep="-")]] <- lapply(X = clm_period_sp, FUN = TopoSUB_droughtEstim)
  }
  
  return(result)
 
}  
    