#' battery_density_f
#' Function: Return the density of the battery for the specified vehicle
#' @export
battery_density_f = function(vehicle,model_year,bat_impro=NA){
  attribute_f("battery_density_f")
  #Inputs
  bat_fc_dt  <- get_input_f(input_name = 'greet_battery')
  #Check
  if (class(vehicle)[1]=="vehicleClass"){
    techno <- vehicle$technology
  } else {
    techno <- vehicle
  }
  if (grepl("BEV",techno) | grepl("PHEV",techno)){
    tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
    bat_type <- vehicle$battery_type
    #i_dens: Initial battery specific
    i_year <- 2015
    i_dens <- subset(bat_fc_dt,Subcomponent=="EV Battery" & `Battery type`%in%c(bat_type,NA) & Data=="Energy density" & grepl(tmp_techno,Technology))[,as.character(i_year)]
    #Consider battery specific energy improvement
    f_year <- 2030
    f_dens <- switch(bat_impro,
                        "n"=i_dens,
                        "y"=subset(bat_fc_dt,Subcomponent=="EV Battery" & `Battery type`%in%c(bat_type,NA) & Data=="Energy density" & grepl(tmp_techno,Technology))[,as.character(f_year)])
    mat_density <- matrix(NA,nrow=1,ncol=length(model_year),dimnames=list("kWh/kg",model_year))
    for (year in model_year){
      if (year <= i_year){
        dens <- i_dens
      } else if(year < f_year){
        dens <- i_dens+(year-i_year)*(f_dens-i_dens)/(f_year-i_year)
      } else {
        dens <- f_dens
      }
      mat_density[1,as.character(year)] <- dens
    }
  } else {
    mat_density <- NA
  }
  return(mat_density)
}
