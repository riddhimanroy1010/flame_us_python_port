#' fleet_battery_flow_f
#' Function: Calculates material flow associated with batteries used by EVs.
#' @importFrom reshape2 acast
#' @import tidyr
#' @export
fleet_battery_flow_f <- function(bat_mat_comp_mdl=NA){
  attribute_f("fleet_battery_flow_f")
  #Inputs
  battery_material_composition  <- get_input_f(input_name = 'battery_material_composition')
  #Functions' Outputs
  #
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet_vint_stock_f_res[["fleet_vint_scrap"]]
  #
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_veh_specs_dt <- vehicle_module_f_res[["fleet_veh_specs_dt"]]
  fleet_mc_cpt_dt <- vehicle_module_f_res[["fleet_mc_cpt_dt"]]
  #Calculate battery production flow (in GWh)
  year_tbc <- unique(fleet_mc_cpt_dt$Model_year)
  #ev_techno_l <- unique(subset(fleet_mc_cpt_dt,Subcomponent=="EV Battery")$Technology) #SOLVE ISSUE WITH BATTERY DENSITY OF FCV AND HEV
  ev_techno_l <- c("BEV100","BEV300","PHEV20","PHEV40")
  #Production flow of eVs: Unit = number of vehicles
  mat_ev_prod <- subset(fleet_vint_stock,Age==0 & Year%in%year_tbc & Technology%in%ev_techno_l) %>%
    acast(data=., paste0(Size,"_",Technology) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Mass of battery by veh: Unit = kg
  mat_bat_wgt <- subset(fleet_mc_cpt_dt,Model_year%in%year_tbc & Technology%in%ev_techno_l & Subcomponent=="EV Battery") %>%
    acast(data=., paste0(Size,"_",Technology) ~ Model_year , value.var='Weight',fun.aggregate=sum, margins=FALSE)
  #Battery density: Unit = kWh/kg
  mat_bat_density <- subset(fleet_veh_specs_dt,Model_year%in%year_tbc & Technology%in%ev_techno_l & Attribute=="battery_density") %>%
    acast(data=., paste0(Size,"_",Technology) ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Calculate battery production flow in energy by technology: in GWh (converted from kWh to GWh)
  mat_bat_prod_en <- (mat_ev_prod * mat_bat_wgt[rownames(mat_ev_prod),colnames(mat_ev_prod)] * mat_bat_density[rownames(mat_ev_prod),colnames(mat_ev_prod)])*10^(-6)
  #Calculate battery production flow in mass
  mat_bat_prod_wgt <- t(colSums(mat_ev_prod * mat_bat_wgt[rownames(mat_ev_prod),colnames(mat_ev_prod)]))
  #Material composition
  mat_bat_mat_comp <- subset(battery_material_composition) %>%
    acast(data=., Metal ~ Unit , value.var=bat_mat_comp_mdl,fun.aggregate=sum, margins=FALSE)
  #Calculate nattery material flow: Unit = kg
  mat_bat_mat_flow <- mat_bat_mat_comp %*% mat_bat_prod_wgt
  #Format results
  #
  fleet_battery_flow_dt <- as.data.frame(mat_bat_prod_en,stringsAsFactors = FALSE) %>%
    cbind(Type=rownames(mat_bat_prod_en),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Type,convert=TRUE)
  fleet_battery_flow_dt[,"Size"] <- substr(fleet_battery_flow_dt$Type,0,as.numeric(regexpr(pattern="_",fleet_battery_flow_dt$Type))-1)
  fleet_battery_flow_dt[,"Technology"] <- substr(fleet_battery_flow_dt$Type,as.numeric(regexpr(pattern="_",fleet_battery_flow_dt$Type))+1,200)
  fleet_battery_flow_dt[,"Type"] <- NULL
  fleet_battery_flow_dt[,"Unit"] <- "GWh"
  #
  fleet_battery_material_flow_dt <- as.data.frame(mat_bat_mat_flow,stringsAsFactors = FALSE) %>%
    cbind(Material=rownames(mat_bat_mat_flow),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Material,convert=TRUE)
  fleet_battery_material_flow_dt[,"Unit"] <- "kg"
  return(list(fleet_battery_flow_dt=fleet_battery_flow_dt,fleet_battery_material_flow_dt=fleet_battery_material_flow_dt))
}
