#' fleet_i_ev_bat_f
#' Function: Return the initial electric range[miles], battery capacity [kWh], and battery weight[kg] for all technologies with ev batteries
#' @export
fleet_i_ev_bat_f<-function(FCV_bat_t = NA,
                        BEV_bat_t = NA,
                        PHEV_bat_t = NA,
                        HEV_bat_t = NA,
                        wgt_scen_GREET=NA,
                        ev_bat_size_mdl=NA){
  attribute_f("fleet_i_ev_bat_f")
  #Inputs
  vh_techno  <- get_input_f(input_name = 'model_matching_technology')
  wt_subcomp  <- get_input_f(input_name = 'c2g_rel_subcpt_wgt')
  ev_bat_size_dt  <- get_input_f(input_name = 'ev_bat_size')
  greet_battery_size <- get_input_f(input_name = 'greet_battery_size')
  bat_fc_dt <- get_input_f(input_name = 'greet_battery')
  conv <- get_input_f(input_name = 'conversion_units')
  #Output
  dt_col <- c("Size","Technology","Bat_type","Capacity","Weight")
  ev_bat_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop size
  for (size in c("Car","Light truck")){
    #Loop technology
    for (techno in unique(vh_techno$Own)){
      techno_greet <- subset(vh_techno,Own==techno)[,"GREET1"]
       component <- unique(subset(wt_subcomp,sapply(1:nrow(wt_subcomp),function(x)techno_greet%in%unlist(strsplit(Technology,",")[x])))[,"Component"])
      #If EV battery included in vehicle's component
      if ("EV Battery"%in%component){
        #bat_type is the cathode type of eletric battery
        if (grepl("BEV",techno)){
          bat_type=BEV_bat_t
        } else if (grepl("PHEV",techno)){
          bat_type=PHEV_bat_t
        } else if (techno=="HEV"){
          bat_type=HEV_bat_t
        } else if (techno=="FCV"){
          bat_type=FCV_bat_t
        }
        #
        if (grepl("BEV",techno) | grepl("PHEV",techno)){
          #bat_cap is the battery capacity in kWh
          bat_cap <- subset(ev_bat_size_dt,Year==2020 & Size==size & Technology==techno & Model==ev_bat_size_mdl)$Value
          tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
        } else {
          #Size_GREET:Model size to use in GREET
          if (size=="Light truck" & wgt_scen_GREET%in%c(1,4)|size=="Car" & wgt_scen_GREET%in%c(3,4)){ size_greet = "SUV"
          }else if (size=="Light truck" & wgt_scen_GREET%in%c(2,3)) { size_greet = "PUT"
          }else { size_greet="Car"}
          #bat_cap is the battery capacity in kWh
          bat_cap <- subset(greet_battery_size,Data=="Conventional" & Size==size_greet & Technology==techno)$value
          tmp_techno <- techno
        }
        #bat_wgt is the battery weight in lb
        bat_wgt <- bat_cap/
          subset(bat_fc_dt,Subcomponent=="EV Battery" & sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(Technology[x],","))) & `Battery type`==bat_type)[,"2015"]*
          conv["lb","1 kg"]
        #Fill output data
        ev_bat_dt[nrow(ev_bat_dt)+1,c("Size","Technology","Bat_type","Capacity","Weight")] <- c(size,techno,bat_type,bat_cap,bat_wgt)
      }
    }
  }
  #Format
  ev_bat_dt[,c("Capacity","Weight")] <- sapply(c("Capacity","Weight"),function(x) as.numeric(ev_bat_dt[,x]))
  return(list(fleet_i_ev_bat=ev_bat_dt))
}
