#' vehicle_peak_power_f
#' Function: Returns vehicle peak power of a specific vehicle
#' @export
vehicle_peak_power_f = function(vehicle, model_year,HEV_bat_t=NA){
  #Assign default value
  attribute_f("vehicle_peak_power_f")
  #Inputs
  pwt_components_dt <- get_input_f(input_name = 'mackenzie_pwt_components')
  bat_fc_dt <- get_input_f(input_name = 'greet_battery')
  if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
  }else{
    tmp_techno <- vehicle$technology
  }
  pwt_components_dt <- pwt_components_dt[pwt_components_dt$Size==vehicle$size & sapply(1:nrow(pwt_components_dt),function(x)tmp_techno %in% unlist(strsplit(pwt_components_dt$Technology[x],","))),]
  #Create the component data frame
  cpt_l <- unlist(strsplit(pwt_components_dt$Component,","))
  cpt_dt <- data.frame(Component = cpt_l, stringsAsFactors = FALSE)
  #Loop for components
  for (cpt in cpt_dt$Component){
    #Extract component weight
    cpt_dt[cpt_dt$Component==cpt,"wgt"] <- sum(subset(vehicle$material_component_composition,Model_year==model_year & Subcomponent==cpt)$Weight)
    #Extract fixed mass if applicable
    cpt_dt[cpt_dt$Component==cpt,"fixed_mass"] <- ifelse(any(bat_fc_dt$Subcomponent==cpt
                                                         &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                         &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                                                         &bat_fc_dt$Data=="Fixed mass"),
                                                     bat_fc_dt[bat_fc_dt$Subcomponent==cpt
                                                               &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                               &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)      
                                                               &bat_fc_dt$Data=="Fixed mass","2015"],0)
    if ((cpt %in% c("Engine","Traction Motor") & tmp_techno!="HEV")|(cpt%in%c("Engine","EV Battery") & tmp_techno=="HEV")){
      #Extract density of the component (in kW or kWh / kg)
      cpt_dt[cpt_dt$Component==cpt,"density"] <- bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                                                                 &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                                 &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                                                                 &bat_fc_dt$Data=="Energy density"),"2015"]
      #Initial component's peak power based on density
      cpt_dt[cpt_dt$Component==cpt,"peak_power"]<- (cpt_dt[cpt_dt$Component==cpt,"wgt"]-cpt_dt[cpt_dt$Component==cpt,"fixed_mass"])*cpt_dt[cpt_dt$Component==cpt,"density"]
    } else {
      cpt_dt[cpt_dt$Component==cpt,"density"] <- 0
      cpt_dt[cpt_dt$Component==cpt,"peak_power"] <- 0
    }
    #Extract usable energy. 1 otherwise
    cpt_dt[cpt_dt$Component==cpt,"usable_e"] <-
      ifelse(any(bat_fc_dt$Subcomponent==cpt
                 &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                 &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                 &bat_fc_dt$Data=="Usable Energy"),
             bat_fc_dt[bat_fc_dt$Subcomponent==cpt
                       &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                       &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                       &bat_fc_dt$Data=="Usable Energy","2015"],1)
  }
  return(cpt_dt)
}
