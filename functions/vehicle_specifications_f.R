#' vehicle_specifications_f
#' Function: Returns vehicle specifications (e.g., horsepower, acceleration time)of a specified vehicle.
#' @export
vehicle_specifications_f = function(vehicle,fc_impro=NA,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_specifications_f")
  if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
  } else {
    tmp_techno <- vehicle$technology
  }
  #Input
  vehicle_specs_dt  <- get_input_f(input_name = 'vehicle_specifications')
  bat_fc_dt  <- get_input_f(input_name = 'greet_battery')
  #Format: Only consider appropriate rows
  rows <- which(sapply(1:nrow(vehicle_specs_dt),function(x)(tmp_techno%in%unlist(strsplit(vehicle_specs_dt$Technology[x],",")) | vehicle_specs_dt$Technology[x]=="Glo")
                       & (vehicle$size %in% unlist(strsplit(vehicle_specs_dt$Size[x],","))|vehicle_specs_dt$Size[x]=="Glo")))
  vehicle_specs <- vehicle_specs_dt[rows,]
  vehicle_specs_dyn_dt  <- get_input_f(input_name = 'vehicle_specifications_dyn')
  #Format: Only consider appropriate rows
  rows <- which(sapply(1:nrow(vehicle_specs_dyn_dt),function(x)(tmp_techno%in%unlist(strsplit(vehicle_specs_dyn_dt$Technology[x],",")) | vehicle_specs_dyn_dt$Technology[x]=="Glo")
                       & (vehicle$size %in% unlist(strsplit(vehicle_specs_dyn_dt$Size[x],","))|vehicle_specs_dyn_dt$Size[x]=="Glo")
                       & fc_impro %in% unlist(strsplit(vehicle_specs_dyn_dt$Improvement[x],","))))
  vehicle_specs_dyn <- vehicle_specs_dyn_dt[rows,]
  #Other parameters
  last_hist_yr <- max(as.numeric(colnames(vehicle$fuel_consumption)[!is.na(vehicle$fuel_consumption[1,])]))
  #List of specifications to consider
  if ("Electricity"%in%vehicle$fuel_type){
    specs_list <- c(vehicle_specs$Parameter,"range","battery_density","peak_power")
  } else {
    specs_list <- c(vehicle_specs$Parameter,"peak_power")
  }
  #Create specification matrix
  vehicle$specifications <- matrix(NA,nrow=length(specs_list),ncol=last_yr-first_yr+1,dimnames=list(specs_list,first_yr:last_yr))
  #Update the matrix with constant values
  vehicle$specifications[subset(vehicle_specs,Constant=="y")$Parameter,] <- subset(vehicle_specs,Constant=="y")$Value
  #Update the matrix with dynamic values
  for (par in unique(subset(vehicle_specs,Constant=="n")$Parameter)){
    if (is.na(subset(vehicle_specs,Parameter==par)$Value)){
      vehicle$specifications[par,] <- as.matrix(subset(vehicle_specs_dyn,Parameter==par)[,colnames(vehicle$specifications)])
    } else {
      vehicle$specifications[par,] <- subset(vehicle_specs,Parameter==par)$Value*as.matrix(subset(vehicle_specs_dyn,Parameter==par)[,colnames(vehicle$specifications)])
    }
  }
  first_cpt_composition_yr <- min(vehicle$material_component_composition$Model_year)
  #Update peak power from first material composition. Year=2016
  cpt_dt <- do.call(vehicle_peak_power_f,list(vehicle=vehicle,model_year=first_cpt_composition_yr))
  vehicle$specifications["peak_power",as.character(first_cpt_composition_yr:last_hist_yr)] <- sum(cpt_dt$peak_power)
  #Update battery density and vehicle range
  if ("Electricity"%in%vehicle$fuel_type){
    #Battery density
    vehicle$specifications["battery_density",as.character(first_cpt_composition_yr:last_yr)] <- do.call(battery_density_f,list(vehicle=vehicle,model_year=first_cpt_composition_yr:last_yr))
    #Initial vehicle range from battery weight
    #tmp_techno is the techno without range value
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
    wgt_bat <- sum(subset(vehicle$material_component_composition, Component=="EV Battery" & Model_year==first_cpt_composition_yr)$Weight)
    usable_en <- subset(bat_fc_dt,Subcomponent=="EV Battery" & sapply(1:nrow(bat_fc_dt),function(x)tmp_techno%in%unlist(strsplit(Technology, ",")[x])) & Data == "Usable Energy")[,"2015"]
    vehicle$specifications["range",as.character(first_cpt_composition_yr:last_hist_yr)] <- vehicle$specifications["battery_density",as.character(first_cpt_composition_yr:last_hist_yr)]*wgt_bat/vehicle$fuel_consumption["Electricity",as.character(first_cpt_composition_yr:last_hist_yr)]*100*usable_en
  }
  return(vehicle)
}
