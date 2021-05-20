#' vehicle_weight_changes_battery_f
#' Function: Weight changes in EVs due to improvement in battery technology from one year to another
#' @export
vehicle_weight_changes_battery_f = function(vehicle,model_year,mat_mc_component,bat_resz_ratio=NA,mat_i_mc_component=0,weight_iteration="n"){
  attribute_f("vehicle_weight_changes_battery_f")
  #Inputs
  bat_fc_dt  <- get_input_f(input_name = 'greet_battery')
  #Output file
  mat_f_mc_component <- mat_mc_component
  #Adjust EV Battery for BEV and PHEV.
  if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
    #tmp_techno is the techno without range value
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
    cpt <- "EV Battery"
    #
    mat_frv <- do.call(vehicle_frv_f,list(vehicle,model_year))
    #Extract usable energy
    usable_en <- subset(bat_fc_dt,Subcomponent==cpt & sapply(1:nrow(bat_fc_dt),function(x)tmp_techno%in%unlist(strsplit(Technology, ",")[x])) & Data == "Usable Energy")[,"2015"]
    #i_range is the range vehicle before the weight changes
    i_range <- switch(weight_iteration,
                      "n"=vehicle$specifications["range",as.character(model_year-1)],
                      "y"=vehicle$specifications["range",as.character(model_year)])
    #i_bat_wgt is the weight of the battery before the weight changes
    i_bat_wgt <- switch(weight_iteration,
                        "n"=sum(subset(vehicle$material_component_composition, Model_year==model_year-1 & Component==cpt)$Weight),
                        "y"=sum(mat_i_mc_component[,cpt]))
    #i_elec_cons is the electrical consumption before the weight changes
    i_delta_cw <- sum(mat_i_mc_component)-sum(vehicle$material_composition[,as.character(model_year-1)])
    i_elec_cons <- switch(weight_iteration,
                          "n"=vehicle$fuel_consumption["Electricity",as.character(model_year-1)],
                          "y"=vehicle$fuel_consumption["Electricity",as.character(model_year)]+i_delta_cw*mat_frv["Electricity",]/100)
    #i_dens is the battery density before the weight changes
    i_dens <- i_range*i_elec_cons/(100*i_bat_wgt*usable_en)
    #f_dens is the current battery density
    f_dens <- vehicle$specifications["battery_density",as.character(model_year)]
    #Update the fuel consumption with the current weight changes
    f_delta_cw <- sum(mat_mc_component)-sum(vehicle$material_composition[,as.character(model_year-1)])
    #f_elec_cons is the current electrical consumption
    f_elec_cons <- vehicle$fuel_consumption["Electricity",as.character(model_year)]+f_delta_cw*mat_frv["Electricity",]/100
    #Calculate final battery weight
    f_bat_wgt <- i_bat_wgt*(1+bat_resz_ratio*(i_dens/f_dens*f_elec_cons/i_elec_cons-1))
    f_range <- 100/f_elec_cons*f_dens*f_bat_wgt*usable_en
    #Adjust EV Battery weight and material composition in mat_f_mc_component
    mat_f_mc_component[,cpt] <- mat_mc_component[,cpt]*f_bat_wgt/i_bat_wgt
  }
  return(list(mat_f_mc_component=mat_f_mc_component,final_range=get0("f_range")))
}
