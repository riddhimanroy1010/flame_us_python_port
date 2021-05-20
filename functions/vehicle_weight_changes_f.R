#' vehicle_weight_changes_f
#' Function: Apply all functions that update vehicle weight.
#' @import reshape2
#' @import tidyr
#' @export
vehicle_weight_changes_f = function(vehicle,model_year,sms_cutoff=NA){
  attribute_f("vehicle_weight_changes_f")
  #mat_mc_component: Material composition of previous model_year
  mat_mc_component <- acast(data=subset(vehicle$material_component_composition, Model_year==(model_year-1)), Material ~ Subcomponent, value.var='Weight',fun.aggregate=sum, margins=FALSE)
  #Get material composition with material substitution of lightweighting
  mat_mc_component_lw <- do.call(vehicle_wgt_changes_lightweighting_f,list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component))
  #Get material composition with increase in feature content
  mat_mc_component_ft <- do.call(vehicle_wgt_changes_feature_f,list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_lw))
  #Get material composition after battery resizing. Update Battery range
  vehicle_weight_changes_battery_f_res <- do.call(vehicle_weight_changes_battery_f,list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_ft))
  mat_mc_component_bat <- vehicle_weight_changes_battery_f_res[["mat_f_mc_component"]]
  if ("Electricity"%in%vehicle$fuel_type){
    vehicle$specifications["range",as.character(model_year)] <- vehicle_weight_changes_battery_f_res[["final_range"]]
  }
  #Get material composition after powertrain resizing. Update peak power
  vehicle_weight_changes_powertrain_f_res <- do.call(vehicle_weight_changes_powertrain_f,list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_bat))
  mat_mc_component_pwt <- vehicle_weight_changes_powertrain_f_res[["mat_f_mc_component"]]
  vehicle$specifications["peak_power",as.character(model_year)] <- vehicle_weight_changes_powertrain_f_res[["final_peak_power"]]
  #Get material composition after secondary weight changes
  mat_mc_component_sec <- do.call(vehicle_weight_changes_secondary_f,list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_pwt))
  #Iterative weight changes
  while (sum(mat_mc_component_bat)-sum(mat_mc_component_sec)>sms_cutoff){
    #Get material composition after battery resizing. Update Battery range
    vehicle_weight_changes_battery_f_res <- do.call(vehicle_weight_changes_battery_f,
                                                    list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_sec,mat_i_mc_component=mat_mc_component_bat,weight_iteration="y"))
    mat_mc_component_bat <- vehicle_weight_changes_battery_f_res[["mat_f_mc_component"]]
    if ("Electricity"%in%vehicle$fuel_type){
      vehicle$specifications["range",as.character(model_year)] <- vehicle_weight_changes_battery_f_res[["final_range"]]
    }
    #Get material composition after powertrain resizing. Update peak power
    vehicle_weight_changes_powertrain_f_res <- do.call(vehicle_weight_changes_powertrain_f,
                                                       list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_bat,mat_i_mc_component=mat_mc_component_pwt,weight_iteration="y"))
    mat_mc_component_pwt <- vehicle_weight_changes_powertrain_f_res[["mat_f_mc_component"]]
    vehicle$specifications["peak_power",as.character(model_year)] <- vehicle_weight_changes_powertrain_f_res[["final_peak_power"]]
    #Get material composition after secondary weight changes
    mat_mc_component_sec <- do.call(vehicle_weight_changes_secondary_f,
                                    list(vehicle=vehicle,model_year=model_year,mat_mc_component=mat_mc_component_pwt,mat_i_mc_component=mat_mc_component_sec,weight_iteration="y"))
  }
  #Update material composition
  vehicle$material_composition[,as.character(model_year)] <- rowSums(mat_mc_component_sec)[rownames(vehicle$material_composition)]
  #Save output in vehicle
  tmp_dt <- as.data.frame(mat_mc_component_sec) %>%
    cbind(Material=rownames(mat_mc_component_sec),stringsAsFactors = FALSE) %>% 
    gather("Subcomponent","Weight",-Material,convert=TRUE) %>%
    cbind(Model_year=model_year)
  tmp_dt[,"Component"] <- get_matching_names(tmp_dt[,"Subcomponent"],matching_type="component",original_source="Own subcomponent",matched_source="Own component")
  vehicle$material_component_composition <- rbind(vehicle$material_component_composition,tmp_dt)
  return(vehicle)
}
