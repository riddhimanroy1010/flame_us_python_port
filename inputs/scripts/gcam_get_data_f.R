library(tidyr)
change_scenario_name <- function(scen_name){
  out_scenario_name <- gsub("GCAM_","",scen_name)
  out_scenario_name <- gsub("_ag_trade","",out_scenario_name)
  out_scenario_name <- gsub("2p6","_26",out_scenario_name)
  return(out_scenario_name)
}

get_on_road_stock <- function(gcam_data_list){
  #Extract
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  out_by_tech <- as.data.frame(gcam_data_list[['outputs by tech']])
  load_factor_ldv_by_tech <- gcam_data_list[['transport load factors']] %>%
    subset(.,sector%in%c("trn_pass_road_LDV_4W"))%>%
    as.data.frame()
  #
  ldv_pkt <- subset(inputs_by_tech,sector%in%c("trn_pass_road_LDV") & mode%in%c("4W"))
  #
  out_ldv_pkt_by_tech <- subset(out_by_tech,sector%in%c("trn_pass_road_LDV_4W"))
  #Other parameters
  gcam_size_l <- c("Compact Car","Large Car","Light Truck and SUV","Midsize Car")
  #vkt_veh_dt contains the vehicle.km per year of the vehicle
  vkt_veh_dt <- read.csv("inputs/data/gcam_vkt_veh.csv",stringsAsFactors = FALSE) %>%
    subset(.,region=="USA" & sector=="trn_pass_road_LDV_4W")
  on_road_stock_ldv <- subset(out_ldv_pkt_by_tech,select=-c(Units,output))
  for (r in 1:nrow(on_road_stock_ldv)){
    gcam_size <- on_road_stock_ldv[r,"subsector"]
    gcam_techno <- on_road_stock_ldv[r,"technology"]
    gcam_year <- on_road_stock_ldv[r,"year"]
    #Get load factor (passenger/veh)
    load_factor_value <- subset(load_factor_ldv_by_tech,subsector==gcam_size & technology==gcam_techno & year==gcam_year)$value
    #Get vkt per veh
    vkt_per_veh_value <- subset(vkt_veh_dt,subsector==gcam_size)$vkt_veh
    #Vehicle stock: 
    on_road_stock_ldv[r,"value"] <- out_ldv_pkt_by_tech[r,"value"]*10^6/load_factor_value/vkt_per_veh_value
  }
  on_road_stock_ldv[,"Units"] <- "vehicle"
  on_road_stock_ldv[,"scenario"] <- change_scenario_name(unique(on_road_stock_ldv[,"scenario"]))
  return(on_road_stock_ldv)
}
#
get_ldv_energy_use <- function(gcam_data_list){
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  in_ldv_energy_use <- subset(inputs_by_tech,sector%in%c("trn_pass_road_LDV_4W") & Units=='EJ')
  in_ldv_energy_use[,"scenario"] <- change_scenario_name(unique(in_ldv_energy_use[,"scenario"]))
  return(in_ldv_energy_use)
}
#
get_ldv_energy_consumption <- function(gcam_data_list){
  #
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  in_ldv_energy_use <- subset(inputs_by_tech,sector%in%c("trn_pass_road_LDV_4W") & Units=='EJ')
  on_road_energy_consumption_ldv <- subset(in_ldv_energy_use,select=-Units)
  load_factor_ldv_by_tech <- gcam_data_list[['transport load factors']] %>%
    subset(.,sector%in%c("trn_pass_road_LDV_4W"))%>%
    as.data.frame()
  #
  out_by_tech <- as.data.frame(gcam_data_list[['outputs by tech']])
  out_ldv_pkt_by_tech <- subset(out_by_tech,sector%in%c("trn_pass_road_LDV_4W"))
  #Calculate the energy consumption by vehicle (MJ/km)
  for (r in 1:nrow(on_road_energy_consumption_ldv)){
    gcam_size <- on_road_energy_consumption_ldv[r,"mode"]
    gcam_techno <- on_road_energy_consumption_ldv[r,"technology"]
    gcam_year <- on_road_energy_consumption_ldv[r,"year"]
    #Get load factor (passenger/veh)
    load_factor_value <- subset(load_factor_ldv_by_tech,subsector==gcam_size & technology==gcam_techno & year==gcam_year)$value
    #Get vkt per veh
    pkt_value <- subset(out_ldv_pkt_by_tech,subsector==gcam_size & technology==gcam_techno & year==gcam_year)$value*10^6
    #on-road energy consumption in MJ/100km: 
    on_road_energy_consumption_ldv[r,"value"] <- in_ldv_energy_use[r,"value"]*10^12/(pkt_value/load_factor_value)*100
  }
  on_road_energy_consumption_ldv[,"Units"] <- "MJ/100km"
  on_road_energy_consumption_ldv[,"scenario"] <- change_scenario_name(unique(on_road_energy_consumption_ldv[,"scenario"]))
  return(on_road_energy_consumption_ldv)
}

#Calculate the emission factor for refined products, assuming biomass inputs have no emissions
get_emission_intensity_refining <- function(gcam_data_list){
  #inputs
  gcam_ef <- read.csv("inputs/data/gcam_emission_factors.csv",stringsAsFactors = FALSE)
  #
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  out_by_tech <- as.data.frame(gcam_data_list[['outputs by tech']])
  carbon_seq_dt <- as.data.frame(gcam_data_list[['CO2 sequestration by sector']])
  kgco2_per_kgc <- 11/3
  
  #Create matrix of technology outputs for refining
  mat_out_refining <- acast(data=subset(out_by_tech, sector=="refining" & output=="refining" & Units=="EJ"), subsector ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  
  #Create matrix of inptus for refining (except electricity)
  mat_in_refining <- acast(data=subset(inputs_by_tech, sector=="refining" & Units=="EJ" & input!="elect_td_ind"), input ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  
  #Create matrix of emission factors
  mat_in_refining_ef <- diag(x=as.numeric(sapply(rownames(mat_in_refining),function(x)gcam_ef[sapply(1:nrow(gcam_ef),function(y)x %in% unlist(strsplit(gcam_ef[y,"Final_input"],";"))),"Value_co2"])))
  dimnames(mat_in_refining_ef) <- list(rownames(mat_in_refining),rownames(mat_in_refining))
  
  #Calculate matrix of emissions per input
  mat_in_refining_e <- mat_in_refining_ef%*%mat_in_refining*10^9
  
  #Calculate carbon sequestration
  mat_css_refining <- matrix(0,nrow=1,ncol=ncol(mat_in_refining_e),dimnames=list("refining",colnames(mat_in_refining_e)))
  if ("refining"%in%unique(carbon_seq_dt$sector)){
    tmp_mat_css_refining <- acast(data=subset(carbon_seq_dt, sector=="refining"), sector ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
    mat_css_refining[,colnames(tmp_mat_css_refining)] <- tmp_mat_css_refining
  }
  #Convert MTC in kg CO2
  mat_css_refining <- mat_css_refining*kgco2_per_kgc*10^9
  
  #Calculate matrix of emission factors for the refining sector (in kg CO2/EJ of refined liquids) by substracting CSS
  mat_refining_ef <- (colSums(mat_in_refining_e)-mat_css_refining)/colSums(mat_out_refining)
  rownames(mat_refining_ef) <- "kg CO2/EJ"
  
  #Compile output in dataframe
  refining_ef_dt <- as.data.frame(mat_refining_ef) %>% 
    cbind(Unit=rownames(mat_refining_ef),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Unit,convert=TRUE) %>%
    cbind(Scenario=change_scenario_name(unique(inputs_by_tech[,"scenario"])))
  
  refining_out_dt <- as.data.frame(mat_out_refining) %>% 
    cbind(Fuel=rownames(mat_out_refining),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Fuel,convert=TRUE) %>%
    cbind(Unit="EJ",Scenario=change_scenario_name(unique(inputs_by_tech[,"scenario"])))
  return(list(mat_refining_ef=mat_refining_ef,
              refining_ef_dt=refining_ef_dt,
              refining_out_dt=refining_out_dt))
}

#
get_mix_electricity <- function(gcam_data_list){
  #Inputs
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  out_by_tech <- as.data.frame(gcam_data_list[['outputs by tech']])
  #Create matrix of inputs for electricity
  mat_in_electricity <- acast(data=subset(inputs_by_tech,sector=="electricity"), input ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  #Compile output in dataframe
  electricity_mix_dt <- as.data.frame(mat_in_electricity) %>% 
    cbind(Technology=rownames(mat_in_electricity),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Technology,convert=TRUE) %>%
    cbind(Scenario=change_scenario_name(unique(inputs_by_tech[,"scenario"])))
  electricity_mix_dt$Unit <- "EJ"
  return(electricity_mix_dt)
}

#
get_ef_electricity <- function(gcam_data_list){
  #Inputs
  gcam_ef <- read.csv("inputs/data/gcam_emission_factors.csv",stringsAsFactors = FALSE)
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  out_by_tech <- as.data.frame(gcam_data_list[['outputs by tech']])
  carbon_seq_dt <- as.data.frame(gcam_data_list[['CO2 sequestration by sector']])
  kgco2_per_kgc <- 11/3
  #Obtain EF for refined liquids
  mat_refining_ef <- get_emission_intensity_refining(gcam_data_list)[["mat_refining_ef"]]
  #Create matrix of inputs for electricity
  mat_in_electricity <- acast(data=subset(inputs_by_tech,sector=="electricity" & mode%in%c("refined liquids","coal","gas")), input ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  #
  mat_res_in_electricity <- matrix(0,nrow=3,ncol=ncol(mat_in_electricity),dimnames=list(c("refined liquids industrial","regional coal","wholesale gas"),colnames(mat_in_electricity)))
  for (yr in colnames(mat_in_electricity)){
    #Obtain efficiency of inputs per source
    mat_in_in_electricity <- acast(data=subset(inputs_by_tech,sector%in%rownames(mat_in_electricity) & input%in%c("refined liquids industrial","regional coal","wholesale gas") & year==yr), input ~ yr , value.var='value',fun.aggregate=sum, margins=FALSE)
    #
    mat_res_in_electricity[,as.character(yr)] <- acast(data=subset(inputs_by_tech,sector%in%rownames(mat_in_electricity) & input%in%rownames(mat_res_in_electricity) & year==yr), input ~ yr , value.var='value',fun.aggregate=sum, margins=FALSE)[rownames(mat_res_in_electricity),]
  }
  
  #Create matrix of output for electricity
  mat_out_electricity <- acast(data=subset(out_by_tech,sector=="electricity"), subsector ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  
  #Create matrix of emission factors
  mat_in_electricity_ef <- matrix(0,nrow=nrow(mat_res_in_electricity),ncol=ncol(mat_res_in_electricity),dimnames = dimnames(mat_res_in_electricity))
  mat_in_electricity_ef[,] <- as.numeric(sapply(rownames(mat_res_in_electricity),function(x)gcam_ef[sapply(1:nrow(gcam_ef),function(y)x %in% unlist(strsplit(gcam_ef[y,"Final_input"],";"))),"Value_co2"]))*10^9
  mat_in_electricity_ef["refined liquids industrial",] <- mat_refining_ef[,colnames(mat_in_electricity_ef)]
  
  #Calculate matrix of emissions per input
  mat_in_electricity_e <- mat_res_in_electricity*mat_in_electricity_ef[rownames(mat_res_in_electricity),colnames(mat_in_electricity)]
  
  #Calculate carbon sequestration
  mat_css_electricity <- matrix(0,nrow=1,ncol=ncol(mat_in_electricity_e),dimnames=list("electricity",colnames(mat_in_electricity_e)))
  if (any(grepl("elec_",carbon_seq_dt$sector))){
    tmp_mat_css_electricity <- acast(data=subset(carbon_seq_dt, grepl("elec_",sector)), sector ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
    mat_css_electricity[,colnames(tmp_mat_css_electricity)] <- colSums(tmp_mat_css_electricity)
  }
  #Convert MTC in kg CO2
  mat_css_electricity <- mat_css_electricity*kgco2_per_kgc*10^9

  #Calculate matrix of emission factors for the refining sector (in kg CO2/EJ of refined liquids) by substracting CSS
  mat_electricity_ef <- (colSums(mat_in_electricity_e)-mat_css_electricity)/colSums(mat_out_electricity)
  rownames(mat_electricity_ef) <- "kg CO2/EJ"
  
  #Compile output in dataframe
  electricity_ef_dt <- as.data.frame(mat_electricity_ef) %>% 
    cbind(Unit=rownames(mat_electricity_ef),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Unit,convert=TRUE) %>%
    cbind(Scenario=change_scenario_name(unique(inputs_by_tech[,"scenario"])))
  electricity_ef_dt$Value <- electricity_ef_dt$Value/(10^15/3600)
  electricity_ef_dt$Unit <- "kg CO2/kWh"
  return(list(mat_electricity_ef=mat_electricity_ef,
              electricity_ef_dt=electricity_ef_dt))
}

#
get_emission_intensity_ldv <- function(gcam_data_list){
  #Inputs
  gcam_ef <- read.csv("inputs/data/gcam_emission_factors.csv",stringsAsFactors = FALSE)
  inputs_by_tech <- as.data.frame(gcam_data_list[['inputs by technology']])
  in_ldv_energy_use <- subset(inputs_by_tech,sector%in%c("trn_pass_road_LDV_4W") & Units=='EJ')
  #Other function outputs
  mat_refining_ef <- get_emission_intensity_refining(gcam_data_list)[["mat_refining_ef"]]
  mat_electricity_ef <- get_ef_electricity(gcam_data_list)[["mat_electricity_ef"]]
  #Create matrix
  mat_in_ldv <- acast(data=subset(inputs_by_tech,sector%in%c("trn_pass_road_LDV_4W") & Units=='EJ'), input ~ year , value.var='value',fun.aggregate=sum, margins=FALSE)
  
  #Create matrix EF
  mat_in_ldv_ef <- matrix(0,nrow=nrow(mat_in_ldv),ncol=ncol(mat_in_ldv),dimnames = dimnames(mat_in_ldv))
  mat_in_ldv_ef["delivered gas",] <- gcam_ef[sapply(1:nrow(gcam_ef),function(y)"delivered gas" %in% unlist(strsplit(gcam_ef[y,"Final_input"],";"))),"Value_co2"]*10^9
  mat_in_ldv_ef["refined liquids enduse",] <- mat_refining_ef[,colnames(mat_in_ldv_ef)]
  mat_in_ldv_ef["elect_td_trn",colnames(mat_electricity_ef)] <- mat_electricity_ef[,]
  mat_in_ldv_ef["H2 enduse",] <- 0
  
  #Calculate matrix of emissions
  mat_in_ldv_e <- mat_in_ldv*mat_in_ldv_ef[rownames(mat_in_ldv),colnames(mat_in_ldv)]
  
  #Compile output in dataframe
  ldv_e_dt <- as.data.frame(mat_in_ldv_e) %>% 
    cbind(Input=rownames(mat_in_ldv_e),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Input,convert=TRUE) %>%
    cbind(Unit="kg CO2",Scenario=change_scenario_name(unique(inputs_by_tech[,"scenario"])))
  
  return(ldv_e_dt)
}