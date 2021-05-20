#' vehicle_lca_demand_matrix_f
#' Function: Creates a matrix of demand by life cycle stage, process and phase for vehicles. This matrix is multiplied by a matrix of GHG emission factors to obtain the U.S. LDVs GHG emissions.
#' @export
vehicle_lca_demand_matrix_f<-function(techno,size,model_year,lca_veh_lifetime_mdl=NA,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_lca_demand_matrix_f")
  #Input files
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  conv  <- get_input_f(input_name = 'conversion_units')
  lca_process  <- get_input_f(input_name = 'lca_process')
  vkt_lifetime  <- get_input_f(input_name = 'vkt_lifetime')
  annual_mileage  <- get_input_f(input_name = 'annual_mileage_TEDB')
  nw_scrap_rt  <- get_input_f(input_name = 'new_scrap_rate')
  #Functions' Outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  fleet_mc_dt <- vehicle_module_f_res[["fleet_mc_dt"]]
  fleet_mc_cpt_dt <- vehicle_module_f_res[["fleet_mc_cpt_dt"]]
  vehicle_cpt_wgt_dt <- aggregate(formula=Weight~Component+Subcomponent,data=subset(fleet_mc_cpt_dt,Model_year==model_year & Technology==techno & Size==size),FUN=sum)
  #
  fleet_mfa_f_res <- do.call(fun_res_f,list(fun_name="fleet_mfa_f"))
  fleet_mfa_dt <- fleet_mfa_f_res[["fleet_mfa_dt"]]
  #Other parameters
  #lc_vkt is the VKMT over the lifetime of the technology
  lc_vkt <- subset(vkt_lifetime,Size == size & Model == lca_veh_lifetime_mdl)[,"Value"]
  #lifetime is the lifetime of the vehicle to achieve lc_vkt (in year). We add one the annual mileage because not an age, but years
  lifetime <- annual_mileage[min(which(cumsum(annual_mileage[,size]*conv["km","1 mile"])>lc_vkt)),"Vehicle age"]+1
  #vehicle_op_year_list
  vehicle_op_year_list <- model_year:(model_year+lifetime-1)
  #Create the demand matrix
  vehicle_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))

  # Filling matrix ----------------------------------------------------------
  
  #nw_scrap_matrix contains the rate of material demand compared to embodied material
  nw_scrap_matrix <- diag(x=1/(1-nw_scrap_rt$Rate),nrow=nrow(nw_scrap_rt),ncol=nrow(nw_scrap_rt))
  dimnames(nw_scrap_matrix) <- list(nw_scrap_rt$Material,nw_scrap_rt$Material)
  #emb_material_matrix
  emb_material_matrix <- fleet_mc_dt %>% 
    subset(Technology==techno & Size==size & Model_year==model_year) %>%
    acast(Material ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #dem_material_matrix
  dem_material_matrix <-  nw_scrap_matrix %*% emb_material_matrix[colnames(nw_scrap_matrix),as.character(model_year),drop=FALSE]
  #prim_material_share_matrix
  prim_material_share_matrix <-  fleet_mfa_dt %>%
    subset(Name=="prim_share" & Year==model_year) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  ##Fill demand matrix with primary material production
  vehicle_demand_matrix[lca_process$Phase=="Primary Material Production",as.character(model_year)] <- dem_material_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,as.character(model_year),drop=FALSE] * prim_material_share_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,as.character(model_year),drop=FALSE]
  #sec_material_share_matrix
  sec_material_share_matrix <-  fleet_mfa_dt %>%
    subset(Name=="sec_share" & Year==model_year) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Fill demand matrix with secondary material production
  vehicle_demand_matrix[lca_process$Phase=="Secondary Material Production",as.character(model_year)] <- dem_material_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,as.character(model_year),drop=FALSE] * sec_material_share_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,as.character(model_year),drop=FALSE]
  #Fill demand matrix with vehicle manufacturing. 1 vehicle
  vehicle_demand_matrix[lca_process$Process=="Vehicle Assembly",as.character(model_year)] <- 1
  #Fill demand matrix with manufacturing materials
  vehicle_demand_matrix[lca_process$Phase=="Manufacturing" & lca_process$Process!="Vehicle Assembly",as.character(model_year)] <- dem_material_matrix[subset(lca_process, Phase=="Manufacturing" & Process!="Vehicle Assembly")$Process,as.character(model_year),drop=FALSE]
  #Fill demand matrix with battery total weight. Be careful, in current model, battery weight starts not at beginning
  if ("EV Battery"%in%vehicle_cpt_wgt_dt$Component){
    bat_wgt_matrix <- subset(vehicle_cpt_wgt_dt,Component=="EV Battery")$Weight
    vehicle_demand_matrix[lca_process$Process=="Battery production",as.character(model_year)] <- bat_wgt_matrix
    vehicle_demand_matrix[lca_process$Process=="Battery Assembly",as.character(model_year)] <- bat_wgt_matrix
  }
  #Fill fuel use
  #List of fuels used by the technology
  fuel_l <- unlist(strsplit(vh_techno$`Fuel type`[vh_techno$Own == techno][1], ";"))
  #annual_vkt_dt contains the annual kilometer traveled
  dt_col <- c("Age","Annual VKT")
  annual_vkt_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = lifetime),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  annual_vkt_dt[,"Age"] <- 0:(lifetime-1)
  annual_vkt_dt[annual_vkt_dt$Age%in%0:(lifetime-2),"Annual VKT"] <- annual_mileage[annual_mileage$`Vehicle age`%in%0:(lifetime-2),size]*conv["km","1 mile"]
  annual_vkt_dt[annual_vkt_dt$Age == lifetime-1,"Annual VKT"] <- lc_vkt-sum(annual_vkt_dt[annual_vkt_dt$Age%in%0:(lifetime-2),"Annual VKT"])
  for (fuel in fuel_l){
    fc <- subset(fleet_fc_dt,Model_year==model_year & Technology==techno & Size==size & Fuel==fuel)$Value
    uf <- subset(fleet_uf_dt,Model_year==model_year & Technology==techno & Size==size & Fuel==fuel)$Value
    #Fill
    vehicle_demand_matrix[lca_process$Phase=="Fuel Production" & lca_process$Process==fuel,as.character(vehicle_op_year_list)] <- fc/100*uf*annual_vkt_dt$`Annual VKT`
    vehicle_demand_matrix[lca_process$Phase=="Fuel Use" & lca_process$Process==fuel,as.character(vehicle_op_year_list)] <- fc/100*uf*annual_vkt_dt$`Annual VKT`
  }
  #Fill demand matrix with vehicle disposal
  vehicle_demand_matrix[lca_process$Process=="Vehicle Disposal",as.character(model_year+lifetime-1)] <- 1
  return(list(vehicle_demand_matrix=vehicle_demand_matrix))
}
