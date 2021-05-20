#' vehicle_hist_fc_f
#' Function: Returns historical fuel consumption of a specified vehicle technology and size, and save the information in the associated vehicleClass object.
#' @import reshape2
#' @export
vehicle_hist_fc_f = function(vehicle,first_yr=NA,last_yr=NA,fc_ev_mdl=NA,fc_conv_mdl=NA){
  attribute_f("vehicle_hist_fc_f")
  #Other parameters
  age_tbc = 30
  first_hist_yr = first_yr-age_tbc
  last_hist_yr = 2019
  #Create matrix of fuel consumption in the class fields for fuel_type1
  vehicle$fuel_consumption <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=last_yr-first_hist_yr+1,dimnames=list(vehicle$fuel_type,first_hist_yr:last_yr))
  if (vehicle$technology%in%c("ICEV-G","ICEV-D")){
    epa_fc  <- get_input_f(input_name = 'epa_fleet_fc_hist')
    tmp_mat_hist_fc <- acast(data=subset(epa_fc, Model_year>=first_hist_yr & Size==vehicle$size & Technology==vehicle$technology & Fuel_type==vehicle$fuel_type), Fuel_type ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Add degradation factors
    deg_fac <- switch(fc_conv_mdl,'def'=1,'low'=0.9,'high'=1.1)
    tmp_mat_hist_fc <- tmp_mat_hist_fc*deg_fac
  } else if (vehicle$technology%in%c("BEV100","BEV300","PHEV20","PHEV40")){
    fc_ev_hist_dt <- get_input_f(input_name = 'fc_ev_hist')
    #Get the historical values
    tmp_mat_hist_fc <- acast(data=subset(fc_ev_hist_dt, Year>=first_hist_yr & Size==vehicle$size & Technology==vehicle$technology & Model%in%c("Sales weighted",fc_ev_mdl)), Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Add battery charging efficiency and transmission losses
    tmp_mat_hist_fc["Electricity",] <- tmp_mat_hist_fc["Electricity",]/(0.90*0.95)
  } else {
    #Inputs
    fe_vision  <- get_input_f(input_name = 'vision_fe_hist')
    degra_fac  <- get_input_f(input_name = 'fc_degra_factor_vision')
    vh_techno <- get_input_f(input_name = 'model_matching_technology')
    fuel_conv  <- get_input_f(input_name = 'fuel_conversion')
    conv  <- get_input_f(input_name = 'conversion_units')
    #vision_techno contains the list of equivalent technologies in vision data
    vision_techno <- unlist(strsplit(vh_techno$Vision[vh_techno$Own==vehicle$technology],";"))
    #VISION data are unadjusted and combined. We need to consider the degradation factor provided by VISION.
    deg_fac <- subset(degra_fac,Technology==vehicle$technology & Size==vehicle$size & `Fuel type`==vehicle$fuel_type)[,"Degradation factor"]
    #fuel_conv_fact is a conversion factor to convert from L equivalent gasoline to L of fuel (or kWh)
    fuel_conv_fact <- subset(fuel_conv, Fuel=="Gasoline" & Data=="Conversion factor")$value/subset(fuel_conv, Fuel==vehicle$fuel_type & Data=="Conversion factor")$value
    #Get data from VISION
    tmp_mat_hist_fc <- as.matrix(subset(fe_vision,Technology%in%vision_techno & Size==vehicle$size & `Fuel type`==vehicle$fuel_type & Data=="Unadjusted Fuel Economy")[,grep(pattern="[[:digit:]]{1}",colnames(fe_vision),value=TRUE)[grep(pattern="[[:digit:]]{1}",colnames(fe_vision),value=TRUE)>=first_hist_yr]])
    rownames(tmp_mat_hist_fc) = vehicle$fuel_type
    #Convert data (MPGe) in fuel consumpton (L of kWh/100km)
    tmp_mat_hist_fc <- 1/(tmp_mat_hist_fc*deg_fac)*conv["L","1 gal"]*conv["mile","1 km"]*100*fuel_conv_fact
  }
  #Update the vehicle object
  vehicle$fuel_consumption[rownames(tmp_mat_hist_fc),colnames(tmp_mat_hist_fc)] <- tmp_mat_hist_fc
  #Solve NA discrepancy: If NAs between historical data and first year-age.Assumption: FC from the first available model year.
  if (any(is.na(vehicle$fuel_consumption[,colnames(vehicle$fuel_consumption)<first_yr]))){
    na_cols <- colnames(vehicle$fuel_consumption)[is.na(vehicle$fuel_consumption[1,]) & colnames(vehicle$fuel_consumption)<first_yr]
    vehicle$fuel_consumption[,na_cols] <- vehicle$fuel_consumption[,as.character(max(as.numeric(na_cols))+1)]
  }
  #If historical data stop before last_hist_yr, assume constant up to last_hist_yr
  if (any(is.na(vehicle$fuel_consumption[,as.character(last_hist_yr)]))){
    na_cols <- colnames(vehicle$fuel_consumption)[is.na(vehicle$fuel_consumption[1,]) & colnames(vehicle$fuel_consumption)<=last_hist_yr]
    vehicle$fuel_consumption[,na_cols] <- vehicle$fuel_consumption[,as.character(min(as.numeric(na_cols))-1)]
  }
  return(vehicle)
}
