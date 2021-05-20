#' vehicle_module_f
#' Function: Returns aggregated data (fuel consumption, utility factor, material composition) for all vehicles included in the LDV fleet by technology, size and model year
#' @export
vehicle_module_f = function(last_yr=NA){
  attribute_f("vehicle_module_f")
  #Inputs
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  for (technology in unique(vh_techno$Own)){
    for (size in c("Car","Light truck")){
      #Initialize the vehicle attributes
      vehicle <- do.call(vehicle_initialize_f,list(technology=technology,size=size))
      last_hist_yr <- max(as.numeric(colnames(vehicle$fuel_consumption)[!is.na(vehicle$fuel_consumption[1,])]))
      for (model_year in (last_hist_yr+1):last_yr){
        #Update fuel consumptions with efficiency improvements (without weight changes)
        vehicle <- do.call(vehicle_fc_changes_efficiency_f,list(vehicle=vehicle,model_year=model_year))
        #Update material composition and weight
        vehicle <- do.call(vehicle_weight_changes_f,list(vehicle=vehicle,model_year=model_year))
        #Update fuel consumptions with weight changes
        vehicle <- do.call(vehicle_fc_changes_weight_f,list(vehicle=vehicle,model_year=model_year))
        #Update utility factor
        vehicle <- do.call(vehicle_utility_factor_f,list(vehicle=vehicle,model_year=model_year))
      }
      #Save output of vehicle attributes
      fleet_fc_dt <- rbind(get0("fleet_fc_dt"),vehicle$get_data_frame("fuel_consumption"))
      #Format NAs into 0
      fleet_fc_dt[is.na(fleet_fc_dt)] <- 0
      fleet_uf_dt <- rbind(get0("fleet_uf_dt"),vehicle$get_data_frame("utility_factor"))
      fleet_mc_dt <- rbind(get0("fleet_mc_dt"),vehicle$get_data_frame("material_composition"))
      fleet_mc_cpt_dt <- rbind(get0("fleet_mc_cpt_dt"),vehicle$get_data_frame("material_component_composition"))
      fleet_veh_specs_dt <- rbind(get0("fleet_veh_specs_dt"),vehicle$get_data_frame("specifications"))
    }
  }
  return(list(fleet_fc_dt=fleet_fc_dt,fleet_uf_dt=fleet_uf_dt,fleet_mc_dt=fleet_mc_dt,fleet_mc_cpt_dt=fleet_mc_cpt_dt,fleet_veh_specs_dt=fleet_veh_specs_dt))  
}
