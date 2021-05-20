#' vehicle_initialize_f
#' Function: Creates a vehicleClass object with basic data
#' @export

vehicle_initialize_f = function(technology,size,first_yr=NA,last_yr=NA,BEV_bat_t=NA,PHEV_bat_t=NA,HEV_bat_t=NA){
  attribute_f("vehicle_initialize_f")
  #Initialize the vehicle class
  vehicle <- new("vehicleClass",technology=technology,size=size)
  #Update Battery tyoe
  bat_type <- switch(technology,
                     "HEV"=HEV_bat_t,
                     "BEV100"=BEV_bat_t,
                     "BEV300"=BEV_bat_t,
                     "PHEV20"=PHEV_bat_t,
                     "PHEV40"=PHEV_bat_t,
                     "N/A")
  vehicle$battery_type <- bat_type
  #Create fuel_consumption field and fill matrix with historical values
  vehicle <- do.call(vehicle_hist_fc_f,list(vehicle=vehicle))
  #Create material_composition and material_composition_component fields and fill them with historical values
  vehicle <- do.call(vehicle_hist_material_composition_f,list(vehicle=vehicle))
  #Create specifications field and fill matrix with values
  vehicle <- do.call(vehicle_specifications_f,list(vehicle=vehicle))
  #Utility factor
  first_hist_yr <- min(as.numeric(colnames(vehicle$fuel_consumption)))
  last_hist_yr <- max(subset(vehicle$material_component_composition)$Model_year)
  vehicle$utility_factor <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=ncol(vehicle$fuel_consumption),dimnames=list(vehicle$fuel_type,colnames(vehicle$fuel_consumption)))
  vehicle <- do.call(vehicle_utility_factor_f,list(vehicle=vehicle,model_year=last_hist_yr))
  #Consider utility factor constant before
  vehicle$utility_factor[,as.character(first_hist_yr:last_hist_yr)] <- vehicle$utility_factor[,as.character(last_hist_yr)]
  return(vehicle)
}
