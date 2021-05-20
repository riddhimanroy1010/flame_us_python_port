#' fleet_driving_cost_per_vkt_f
#' Function: Calculated driving costs of the U.S. LDVs by vehicle technology, size and model year.
#' @export
fleet_driving_cost_per_vkt_f <- function(aeo_scen=NA,last_yr = NA){
  attribute_f("fleet_fuel_cost_per_vkt_f")
  #Functions' outputs
  fuel_price_f_res <- do.call(fun_res_f,list(fun_name="fuel_price_f"))
  fuel_price <- fuel_price_f_res[["fuel_price"]]
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  #Other parameters
  last_age_tbc <- 30
  #Output
  fleet_driving_cost <- NULL
  for (model_year in unique(fleet_fc_dt$Model_year)){
    #Get fuel consumption by fuel type
    matrix_fc <- fleet_fc_dt %>%
      subset(Model_year==model_year) %>%
      acast(., Size+Technology ~ Fuel , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Get vkt share by fuel type
    matrix_vkt_share <- fleet_uf_dt %>%
      subset(Model_year==model_year) %>%
      acast(., Size+Technology ~ Fuel , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Get price by fuel type for the following years
    matrix_fuel_price <- fuel_price %>%
      subset(Year >= model_year & Year <= model_year+last_age_tbc) %>%
      acast(., Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Calculate the driving costs for the given model year from first year onward. Unit: $/km
    matrix_driving_cost <- (matrix_fc * matrix_vkt_share[rownames(matrix_fc),colnames(matrix_fc)]) %*% matrix_fuel_price[colnames(matrix_fc),,drop=FALSE]/100
    #Convert matrix into long table
    tmp_driving_cost <- as.data.frame(matrix_driving_cost) %>% 
      cbind(Type=rownames(matrix_driving_cost),stringsAsFactors = FALSE) %>% 
      gather("Year","Value",-Type,convert=TRUE) %>%
      cbind(Model_year=model_year)
    tmp_driving_cost[,"Size"] <- substr(tmp_driving_cost$Type,0,as.numeric(regexpr(pattern="_",tmp_driving_cost$Type))-1)
    tmp_driving_cost[,"Technology"] <- substr(tmp_driving_cost$Type,as.numeric(regexpr(pattern="_",tmp_driving_cost$Type))+1,200)
    tmp_driving_cost[,"Type"] <- NULL
    tmp_driving_cost[,"Unit"] <-"$/km"
    #Combine
    fleet_driving_cost <- rbind(fleet_driving_cost,tmp_driving_cost)
  }
  fleet_driving_cost[,"Age"] <- fleet_driving_cost[,"Year"]-fleet_driving_cost[,"Model_year"]
  return(list(fleet_driving_cost=fleet_driving_cost))
  
}
