#' fleet_ldv_stock_update_constant_f
#' Function: Returns total prospective LDV stock assuming no changes and save the information in the fleetClass object.
#' @export
fleet_ldv_stock_update_constant_f <- function(fleet,last_yr=NA){
  attribute_f("fleet_ldv_stock_update_constant_f")
  #Calculate the difference from the first projection year: 2019
  first_proj_yr <- 2021
  #Update fleet total stock
  fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr-1)]
  return(fleet)
}
