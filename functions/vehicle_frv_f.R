#' vehicle_frv_f
#' Function: Returns Fuel Reduction Values of a specified vehicle technology and size
#' @export
vehicle_frv_f = function(vehicle, model_year,fc_impro=NA,fc_dr_city=NA,frv_pwt_r=NA){
  attribute_f("vehicle_frv_f")
  if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
  } else {
    tmp_techno <- vehicle$technology
  }
  #Input
  coef_equation_dt  <- get_input_f(input_name = 'coef_fc_changes_equation')
  mat_frv <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=1,dimnames=list(vehicle$fuel_type,as.character(model_year)))
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
      assign(par,vehicle$specifications[par,as.character(model_year)])
    }
    if (tmp_techno %in% c("HEV","PHEV")){
      eta_c <- 1/(theta_elec/eta_e+(1-theta_elec)/eta_i)
    } else if ((!tmp_techno %in% c("HEV","PHEV","BEV"))){
      eta_c<-eta_i
    } else if (tmp_techno %in% c("BEV")){
      eta_c<-eta_e
    }
    #
    i_cw <- sum(vehicle$material_composition[,as.character(model_year-1)])
    i_FC <- vehicle$fuel_consumption[fuel,as.character(model_year-1)]
    #
    Fw <- i_cw/(Hf*eta_t*eta_c)*((1-theta*mu+rot_mass_factor)*I5+rol_resistance*grav_acc*I2)
    #Calculate adjusted value
    Fw_adj <- Fw*on_road_deg
    #Fx is mass-independent load fuel consumption
    Fx <- 1/(Hf*eta_c)*(0.5*air_density*drag_coef*front_area*I4/eta_t + alpha*I1)
    Fx_adj<-Fx*on_road_deg
    #MIF
    mif <- (Fw/(Fw+Fx))*(i_FC/i_cw)*100
    #If electricity consumed, Ft is the sum of Fw and Fx. Otherwise, total fuel consumption
    if (fuel %in% c("Electricity","Hydrogen")){
      #Ft<-Fw+Fx=
      Ft <- i_FC/100*I2/10^3
      #FRV<-(Fw/Ft)*(i_FC/i_cw)*100
      FRV <- (Fw_adj/Ft)*(i_FC/i_cw)*100
    } else {
      #Ff<-1/(Hf*eta_c)*0.5*fmep*disp*gear_ratio*I2
      #const_ratio<-fmep*disp/1800*gear_ratio
      const_ratio=0.3325
      Ff <- i_cw/(Hf*eta_c)*0.5*const_ratio*I2
      #Calculated adjusted value
      Ff_adj <- Ff*on_road_deg
      #If Fuel Consumption is in Lper 100km, we divide per 100. I2 is in m.
      #Ft<-Fw+Fx+Ff
      Ft<-i_FC/100*I2/10^3
      #Unit FRV: L per km and kg. 
      #FRV_wo_pwt_res is the FRV without powertrain resizing
      FRV_wo_pwt_res <- (Fw_adj/Ft)*(i_FC/i_cw)*100
      #FRV_with_pwt_res is the FRV with powertrain resizing
      FRV_with_pwt_res <- (Fw_adj+Ff_adj)/Ft*(i_FC/i_cw)*100
      #FRV does not consider powertrain resizing if frv_pwt_r=0, consider full powertrain resizing if frv_pwt_r=1.
      FRV <- FRV_wo_pwt_res+frv_pwt_r*(FRV_with_pwt_res-FRV_wo_pwt_res)
    }
    mat_frv[fuel,as.character(model_year)] <- FRV
  }
  return(mat_frv)
}
