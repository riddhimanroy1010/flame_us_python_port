#' fleet_ldv_stock_update_gcam_f
#' Function: Returns total LDV stock derived from GCAM and stores the data in the fleetClass object.
#' @export
fleet_ldv_stock_update_gcam_f <- function(fleet,
                                          last_yr=NA,
                                          ssp_scen=NA,
                                          ssp_mitigation_scen=NA,
                                          gcam_stock_adj=NA){
  attribute_f("fleet_ldv_stock_update_gcam_f")
  #Inputs
gcam_ldv_stock  <- get_input_f(input_name = 'gcam_ldv_on_road_stock')
  gcam_tot_ldv_stock <- subset(gcam_ldv_stock,grepl(paste0("SSP",ssp_scen),scenario) & grepl(ssp_mitigation_scen,scenario) & year%in%c(2015:last_yr)) %>%
    aggregate(formula=value~year,data=.,FUN=sum)
  matrix_tot_ldv_stock <- matrix(NA,ncol=1,nrow=length(2015:last_yr),dimnames = list(2015:last_yr,"Value"))
  matrix_tot_ldv_stock[as.character(gcam_tot_ldv_stock$year),"Value"] <- gcam_tot_ldv_stock$value
  matrix_tot_ldv_stock[,"Value"] <- approx(x=2015:last_yr,y=matrix_tot_ldv_stock,method="linear",xout=2015:last_yr)$y
  #Calculate the difference from the first projection year: 2019
  first_proj_yr <- 2021
  if (gcam_stock_adj=="abs"){
    matrix_tot_ldv_stock_adj <- matrix_tot_ldv_stock-matrix_tot_ldv_stock[as.character(first_proj_yr-1),"Value"]+fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr-1)]
  } else if(gcam_stock_adj=="rel"){
    matrix_tot_ldv_stock_adj <- matrix_tot_ldv_stock/matrix_tot_ldv_stock[as.character(first_proj_yr-1),"Value"]*fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr-1)]
  }
  #Update fleet total stock
  fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- matrix_tot_ldv_stock_adj[as.character(first_proj_yr:last_yr),]
  return(fleet)
}
