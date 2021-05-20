#' fleet_initialize_f
#' Function: Creates a fleetClass object and stored the first data.
#' @export
fleet_initialize_f <- function(first_yr=NA,last_yr=NA){
  attribute_f("fleet_initialize_f")
  #Inputs
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  hist_stock_dt  <- get_input_f(input_name = 'fleet_stock_hist')
  hist_sales_dt  <- get_input_f(input_name = 'fleet_sales_hist')
  #Other parameters
  #i_year is the initilization year for the model. It has to be lower than firsT_yr. Preferably as old as the first stock and sales data to allow the model to correct the stocks
  i_year <- 1970
  first_proj_yr <- max(hist_stock_dt$Year)+1
  #Create historical matrix stock and sales
  matrix_stock_hist <- acast(data=subset(hist_stock_dt,Data_type=="stock"), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  matrix_sales_hist <- acast(data=subset(hist_sales_dt,Data_type=="sales"), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Initialize the fleet class
  fleet <- new("fleetClass")
  fleet$ldv_on_road_stock <- matrix(NA,nrow=nrow(matrix_stock_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_stock_hist),i_year:last_yr))
  fleet$ldv_on_road_stock[rownames(matrix_stock_hist),colnames(matrix_stock_hist)] <- matrix_stock_hist
  #
  fleet$ldv_on_road_stock_tot <- matrix(NA,nrow=1,ncol=length(i_year:last_yr),dimnames=list("Total",i_year:last_yr))
  fleet$ldv_on_road_stock_tot[,colnames(matrix_stock_hist)] <- colSums(matrix_stock_hist)
  #
  fleet$ldv_sales <- matrix(NA,nrow=nrow(matrix_sales_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_sales_hist),i_year:last_yr))
  fleet$ldv_sales[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist
  #
  fleet$technology_market_share <- matrix(NA,nrow=nrow(matrix_sales_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_sales_hist),i_year:last_yr))
  fleet$technology_market_share[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist %*% diag(x=1/colSums(matrix_sales_hist))
  return(fleet)
}
