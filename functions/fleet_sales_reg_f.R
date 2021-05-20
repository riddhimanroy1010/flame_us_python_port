#' fleet_sales_reg_f
#' Function: DEPRECIATED. Returns regional sales of LDVs.
#' @export
fleet_sales_reg_f <-function(aeo_scen=NA,first_yr=NA,last_yr=NA) {
  attribute_f("fleet_sales_reg_f")
  #Input
hist_dts  <- get_input_f(input_name = 'fleet_sales_reg_hist_aeo_14-19')
  matrix_sales_hist <- acast(data=subset(hist_dts,Region!="United States"), paste(Region,"-",Size,"-",Technology) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #last_hist_yr is the last year of historical data
  last_hist_yr <- max(hist_dts$Year)
proj_sales_aeo  <- get_input_f(input_name = 'fleet_sales_reg_proj_aeo')
  sales_dts <- subset(proj_sales_aeo,Aeo_case==aeo_scen & Region!="United States" & Year%in%c((last_hist_yr+1):last_yr))
  matrix_sales_proj <- acast(sales_dts, paste(Region,"-",Size,"-",Technology) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Combine with historical data
  matrix_sales <- cbind(matrix_sales_hist,matrix_sales_proj[rownames(matrix_sales_hist),])
  return(list(matrix_sales=matrix_sales))
}
