#' vehicle_fc_changes_efficiency_f
#' Function: Calculate prospective fuel consumption values of LDVs from previous year fuel consumption value and changes in vehicle efficiency. Update the information in the vehicleClass object.
#' @export
vehicle_fc_changes_efficiency_f = function(vehicle, model_year,fc_impro=NA,fc_dr_city=NA){
  attribute_f("vehicle_fc_changes_efficiency_f")
  if (fc_impro=="n"){
    vehicle$fuel_consumption[,as.character(model_year)] <- vehicle$fuel_consumption[,as.character(model_year-1)]
  } else {
    if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
      tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
    } else {
      tmp_techno <- vehicle$technology
    }
    #Input
    coef_equation_dt  <- get_input_f(input_name = 'coef_fc_changes_equation')
    for (fuel in vehicle$fuel_type){
      #Format: Only consider appropriate rows
      rows <- which(sapply(1:nrow(coef_equation_dt),function(x)(tmp_techno%in%unlist(strsplit(coef_equation_dt$Technology[x],",")) | coef_equation_dt$Technology[x]=="Glo")
                           & (vehicle$size %in% unlist(strsplit(coef_equation_dt$Size[x],","))|coef_equation_dt$Size[x]=="Glo")
                           & (fuel %in% unlist(strsplit(coef_equation_dt$Fuel[x],","))|coef_equation_dt$Fuel[x]=="Glo")))
      coef_equation <- coef_equation_dt[rows,]
      #Create local variables for fixed coef_equation. Combined city and highway data
      for (par in unique(subset(coef_equation,Constant=="y")$Parameter)){
        if (nrow(subset(coef_equation, Parameter==par))==1){
          par_value <- subset(coef_equation, Parameter==par)[,"Value"]
        } else {
          par_value <- subset(coef_equation, Parameter==par & Data=="City")[,"Value"]*fc_dr_city+ subset(coef_equation,Parameter==par & Data=="Highway")[,"Value"]*(1-fc_dr_city)
        }
        assign(par,par_value)
      }
      #Create local variables for vehicle specifications
      coef_for_equation <- c("eta_i","eta_e","eta_t","alpha","mu","rol_resistance","drag_coef","front_area")
      for (par in intersect(rownames(vehicle$specifications),coef_for_equation)){
        assign(paste0("old_",par),vehicle$specifications[par,as.character(model_year-1)])
        assign(paste0("new_",par),vehicle$specifications[par,as.character(model_year)])
      }
      #I1 is integrale(dt) ; I2 is integrale(vdt) ; I3 is integrale(v^2dt) ; I4 is i,",ntegrale(v^3dt) ; I5 is integrale(avdt)
      #Coef_rolling: Coefficient of rolling
      if (tmp_techno %in% c("HEV","PHEV")){
        old_eta_c <- 1/(theta_elec/old_eta_e+(1-theta_elec)/old_eta_i)
        new_eta_c <- 1/(theta_elec/new_eta_e+(1-theta_elec)/new_eta_i)
      } else if ((!tmp_techno %in% c("HEV","PHEV","BEV"))){
        old_eta_c <- old_eta_i
        new_eta_c <- new_eta_i
      } else if (tmp_techno %in% c("BEV")){
        old_eta_c <- old_eta_e
        new_eta_c <- new_eta_e
      }
      #Assume no changes in curb weight. Only changes in efficiencies
      i_cw <- sum(vehicle$material_composition[,as.character(model_year-1)])
      const_ratio = 0.3325
      #old_f is the energy consumption of the vehicle with previous model year vehicle Parameters (in J)
      old_f <- i_cw/(old_eta_t*old_eta_c)*((1-theta*old_mu+rot_mass_factor)*I5+old_rol_resistance*grav_acc*I2) + 1/(old_eta_c)*(0.5*air_density*old_drag_coef*old_front_area*I4/old_eta_t + old_alpha*I1) + i_cw/(old_eta_c)*0.5*const_ratio*I2
      #new_f is the energy consumption of the model_year vehicle with the new vehicle Parameters (in J)
      new_f <- i_cw/(new_eta_t*new_eta_c)*((1-theta*new_mu+rot_mass_factor)*I5+new_rol_resistance*grav_acc*I2) + 1/(new_eta_c)*(0.5*air_density*new_drag_coef*new_front_area*I4/new_eta_t + new_alpha*I1) + i_cw/(new_eta_c)*0.5*const_ratio*I2
      #Update vehicle fuel consumption
      vehicle$fuel_consumption[fuel,as.character(model_year)] <- vehicle$fuel_consumption[fuel,as.character(model_year-1)]*new_f/old_f
    }
  }
  return(vehicle)
}
