#' fleet_vint_stock_update_with_stock_f
#' Function: Update the vintaged fleet stock by vehicle type of a given year from previous year vintaged stock and total stock to match. New sales are estimated.
#' @export
#Require: Previous year vintaged stock, current year sales by type, current year stock by type
fleet_vint_stock_update_with_stock_f <- function(fleet,year,survival_rate_mdl=NA){
  attribute_f("fleet_vint_stock_update_with_stock_f")
  #Inputs
  #Other parameters
  first_proj_yr <- 2021
  last_age_tbc <- 30
  #
  car_identity_matrix <- diag(x = as.numeric(grepl("Car",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(car_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  lt_identity_matrix <- diag(x = as.numeric(grepl("Light truck",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(lt_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  #Survival rates matrix. Not diagonal
  car_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Car", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(car_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  lt_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Light truck", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(lt_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  #Create matrix_vint_stock
  matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(fleet$ldv_sales),dimnames = list(rownames(fleet$ldv_on_road_stock),0:last_age_tbc))
  #Update stock based on previous year matrix stock and survival rates
  matrix_vint_stock[,as.character(1:last_age_tbc)] <- 
    car_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
    (fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% car_surv_rate_matrix) +
    lt_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
    (fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% lt_surv_rate_matrix)
  #Update new vehicle sales: Get sales number from difference between on-road old vehicles and obj stock. 
  #Check that objective stock is higher than vintaged stock from last year. Otherwise, no new sales
  if (fleet$ldv_on_road_stock_tot["Total",as.character(year)]-sum(matrix_vint_stock[,as.character(1:last_age_tbc)])>=0){
    matrix_vint_stock[,"0"] <- fleet$technology_market_share[rownames(matrix_vint_stock),as.character(year)] * (fleet$ldv_on_road_stock_tot["Total",as.character(year)]-sum(matrix_vint_stock[,as.character(1:last_age_tbc)]))
  } else {
    fleet$ldv_on_road_stock_tot["Total",as.character(year)] <- sum(matrix_vint_stock[,as.character(1:last_age_tbc)])
  }
  #Calculate the matrix_scrap. Contains number of scrapped vehicles at age for the given year
  matrix_scrap <- fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:last_age_tbc)] - cbind(matrix_vint_stock[,as.character(1:last_age_tbc)],0)
  dimnames(matrix_scrap) <- list(rownames(matrix_vint_stock),1:(last_age_tbc+1))
  #Update fleet
  fleet$vint_stock[[as.character(year)]] <- trunc(matrix_vint_stock)
  fleet$vint_scrap[[as.character(year)]] <- trunc(matrix_scrap)
  #Update the current year on-road stock and sales with actual values
  fleet$ldv_on_road_stock[rownames(matrix_vint_stock),as.character(year)] <- trunc(rowSums(matrix_vint_stock))
  fleet$ldv_sales[rownames(matrix_vint_stock),as.character(year)] <- trunc(matrix_vint_stock[,"0"])
  return(fleet)
}
