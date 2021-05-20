#' fleet_demand_matrix_f
#' Function: Creates a matrix of demand by life cycle stage, process and phase. This matrix is multiplied by a matrix of GHG emission factors to obtain the U.S. LDV fleet GHG emissions.
#' @import tidyr
#' @importFrom reshape2 acast
#' @export
fleet_demand_matrix_f<-function(first_yr=NA,last_yr=NA){
  attribute_f("fleet_demand_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Functions' Outputs
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet_vint_stock_f_res[["fleet_vint_scrap"]]
  fleet_tot_new <-  fleet_vint_stock %>% subset(Age==0 & Year%in%first_yr:last_yr) %>% 
    aggregate(reformulate(c("Year"),response="Value"),data=.,FUN=sum)
  fleet_tot_scrap <-  fleet_vint_scrap %>% subset(Year%in%first_yr:last_yr) %>% 
    aggregate(reformulate(c("Year"),response="Value"),data=.,FUN=sum)
  #
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  bat_wgt_dt <- vehicle_module_f_res[["fleet_mc_cpt_dt"]] %>% 
    aggregate(formula=Weight~Component+Model_year+Technology+Size,data=.,FUN=sum) %>%
    subset(Component=="EV Battery")
  #
  fleet_fuel_u_f_res <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_f"))
  fleet_fuel_use_tot <- fleet_fuel_u_f_res[["fleet_fuel_use_tot"]]
  #
  fleet_mfa_f_res <- do.call(fun_res_f,list(fun_name="fleet_mfa_f"))
  fleet_mfa_dt <- fleet_mfa_f_res[["fleet_mfa_dt"]]
  #Create the demand matrix
  fleet_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr)) 
  #Fill demand matrix with primary material production
  prim_material_matrix <- fleet_mfa_dt %>% 
    subset(Name=="prim" & Year%in%first_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Primary Material Production",] <- prim_material_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,]
  #Fill demand matrix with secondary material production
  sec_material_matrix <- fleet_mfa_dt %>% 
    subset(Name%in%c("sec_int","sec_ext") & Year%in%first_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Secondary Material Production",] <- sec_material_matrix[subset(lca_process,Phase=="Secondary Material Production")$Process,]
  #Fill demand matrix with vehicle manufacturing
  fleet_demand_matrix[lca_process$Process=="Vehicle Assembly",] <- fleet_tot_new$Value[order(fleet_tot_new$Year)]
  #Fill demand matrix with manufacturing materials
  material_demand_matrix <- fleet_mfa_dt %>% 
    subset(Name=="dem" & Year%in%first_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Manufacturing" & lca_process$Process!="Vehicle Assembly",] <- material_demand_matrix[subset(lca_process, Phase=="Manufacturing" & Process!="Vehicle Assembly")$Process,]
  #Fill demand matrix with battery total weight. Be careful, in current model, battery weight starts not at beginning
  bat_wgt_matrix <- bat_wgt_dt %>% 
    subset(Model_year%in%first_yr:last_yr) %>%
    acast(Size + Technology ~ Model_year , value.var='Weight',fun.aggregate=sum, margins=FALSE)
  new_veh_matrix <- fleet_vint_stock %>%
    subset(Age==0 & Year%in%first_yr:last_yr & Technology%in%bat_wgt_dt$Technology) %>%
    acast(Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  tot_bat_wgt_matrix <- (new_veh_matrix[rownames(bat_wgt_matrix),colnames(bat_wgt_matrix)] * bat_wgt_matrix) %>% colSums()
  fleet_demand_matrix[lca_process$Process=="Battery production",colnames(bat_wgt_matrix)] <- tot_bat_wgt_matrix
  fleet_demand_matrix[lca_process$Process=="Battery Assembly",colnames(bat_wgt_matrix)] <- tot_bat_wgt_matrix
  #Fill demand matrix with fuel use
  fuel_use_matrix <- fleet_fuel_use_tot %>%
    subset(Year%in%first_yr:last_yr) %>%
    acast(Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Fuel Production",] <- fuel_use_matrix[subset(lca_process, Phase=="Fuel Production")$Process,]
  fleet_demand_matrix[lca_process$Phase=="Fuel Use",] <- fuel_use_matrix[subset(lca_process, Phase=="Fuel Use")$Process,]
  #Fill demand matrix with vehicle disposal
  fleet_demand_matrix[lca_process$Process=="Vehicle Disposal",] <- fleet_tot_scrap$Value[order(fleet_tot_scrap$Year)]
  return(list(fleet_demand_matrix=fleet_demand_matrix))
}
