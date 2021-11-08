#This function calculates the GHG emissions of vehicles (in kg Co2 equivalent) for the fleet

fleet_ghg_per_km_f <- function(year = NA, stage = NA, technology = NA){
  
  fleet_gwp_res <- do.call(fun_res_f,list(fun_name="fleet_gwp_f"))
  fleet_vkt_res <- do.call(fun_res_f,list(fun_name="fleet_vkt_f"))
  
  if (stage = "Transport"){
    if (technology %in% c("PHEV20","PHEV40","BEV100","BEV300","HEV","FCV",))
    gwp_dt <- subset(fleet_gwp_res, Sector = stage, Year = year)
  }
  
  dt_col <- c("Year","Age","Size","Technology","Fuel","Unit","Value")
  output <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 5),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  output["Year"] = rep(year, 5)
  output["Technology"] =rep(technology, 5)
  
  
}