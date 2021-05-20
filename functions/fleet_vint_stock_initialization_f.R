#' fleet_vint_stock_initialization_f
#' Function: Initializes a fleetClass object by calculating historical vintaged stocks by vehicle technology, size and model year and storing the data into the object.
#' @export
fleet_vint_stock_initialization_f <- function(fleet,survival_rate_mdl=NA){
  attribute_f("fleet_vint_stock_initialization_f")
  #Other parameters
  #i_year is the initilization year for the model. It has to be lower than firsT_yr. Preferably as old as the first stock and sales data to allow the model to correct the stocks
  i_year <- 1970
  first_proj_yr <- 2021
  last_age_tbc <- 30
  #Create useful matrix. Identity matrices for calculations
  car_identity_matrix <- diag(x = as.numeric(grepl("Car",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(car_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  lt_identity_matrix <- diag(x = as.numeric(grepl("Light truck",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(lt_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  #First, calculate the first year matrix stock
  matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(fleet$ldv_on_road_stock),dimnames = list(rownames(fleet$ldv_on_road_stock),0:last_age_tbc))
  #Update sales
  matrix_vint_stock[,as.character(0)] <- fleet$ldv_sales[rownames(matrix_vint_stock),as.character(i_year)]
  #vector_i_stock is the vector composed of stocks by vehicle type
  vector_i_stock <- fleet$ldv_on_road_stock[,as.character(i_year),drop=FALSE]
  #vector_remaining_stock is the vector composed of stocks by vehicle type minus sales. Attention: If negative, force 0
  vector_remaining_stock <- vector_i_stock - fleet$ldv_sales[rownames(vector_i_stock),as.character(i_year)]
  vector_remaining_stock[vector_remaining_stock<0] <- 0
  #car_survived_pop and lt_survived_pop represent the theoritical normalized survived population distribution based on the cumulative survivale rates
  car_survived_pop <- sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Car", survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))) /
    sum(sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Car", survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))))
  lt_survived_pop <- sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Light truck", survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))) /
    sum(sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Light truck", survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))))
  #The first year stock is based on the survive pop distirbution and the initial stock (minus new vehicles)
  matrix_vint_stock[,as.character(1:last_age_tbc)] <- car_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% (vector_remaining_stock[rownames(matrix_vint_stock),] %*% matrix(car_survived_pop,nrow=1)) + 
    lt_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% (vector_remaining_stock[rownames(matrix_vint_stock),] %*% matrix(lt_survived_pop,nrow=1))
  #Update fleet with first data
  fleet$vint_stock[[as.character(i_year)]] <- matrix_vint_stock
  fleet$vint_scrap[[as.character(i_year)]] <- NULL
  #Update the vintaged stock up to first_proj_yr
  for (year in (i_year+1):(first_proj_yr-1)){
    fleet <- do.call(fleet_vint_stock_update_with_sales_f,list(fleet=fleet,year=year))
  }
  return(fleet)
}
