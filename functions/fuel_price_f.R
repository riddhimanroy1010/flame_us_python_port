#' fuel_price_f
#' Function: Returns prices of fuel used by the LDVs.
#' @export
fuel_price_f <- function(aeo_scen=NA, first_yr = NA, last_yr = NA){
  attribute_f("fuel_price_f")
  #Inputs
  fuel_price_aeo  <- get_input_f(input_name = 'fuel_price_aeo')
  fuel_price_dt <- subset(fuel_price_aeo,Region=="United States" & Aeo_case==aeo_scen,select=c(Fuel,Unit,Year,Value))
  #Add hydrogen price. assumption from energy.gov. $0.12/L
  fuel_price <- rbind(fuel_price_dt,data.frame(Fuel="Hydrogen",Unit="2018 $/L",Year=unique(fuel_price_dt$Year),Value=0.12))
  #Complete historical data 
  matrix_fuel_price_tmp <- acast(data=fuel_price, Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  last_hist_date <- min(fuel_price$Year)
  matrix_fuel_price <- matrix(0,ncol = length(first_yr:last_yr),nrow = length(unique(fuel_price$Fuel)),dimnames = list(unique(fuel_price$Fuel),first_yr:last_yr))
  matrix_fuel_price[rownames(matrix_fuel_price_tmp),colnames(matrix_fuel_price_tmp)] <- matrix_fuel_price_tmp
  #Assumption: Same than last year
  matrix_fuel_price[,as.character(first_yr:(last_hist_date-1))] <- matrix_fuel_price[,as.character(last_hist_date)]
  #Output
  fuel_price <- as.data.frame(matrix_fuel_price) %>% 
    cbind(Fuel=rownames(matrix_fuel_price),Unit=as.character(sapply(rownames(matrix_fuel_price),function(x)unique(subset(fuel_price,Fuel==x)$Unit))),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-c(Fuel,Unit),convert=TRUE)
  return(list(fuel_price=fuel_price))
}
