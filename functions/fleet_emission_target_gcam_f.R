#' fleet_emission_target_gcam_f
#' Function: Calculates GHG emission budgets for the US LDV fleet derived from the GCAM model that are consistent with given climate change targets
#' @export
fleet_emission_target_gcam_f<-function(ssp_scen=NA,
                                       ssp_mitigation_scen=NA,
                                       last_yr=NA){
  #Assign default value
  attribute_f(fun_name = "fleet_emission_target_gcam_f")
  #Inputs
  ssp_emissions  <- get_input_f(input_name = 'gcam_ldv_emissions')
  #Other parameters
  first_data_yr <- 2015
  emission_target_dt <- ssp_emissions %>%
    subset(.,grepl(paste0("SSP",ssp_scen),Scenario) & grepl(ssp_mitigation_scen,Scenario) & Year%in%c(first_data_yr:last_yr)) %>%
    aggregate(formula=Value~Year+Unit,data=.,FUN=sum)
  #Create matrix with final emissions
  emission_target_matrix <- matrix(NA,ncol=1,nrow=length(first_data_yr:last_yr),dimnames = list(first_data_yr:last_yr,"Value"))
  emission_target_matrix[as.character(emission_target_dt$Year),"Value"] <- emission_target_dt$Value
  #Perform the linear regression.
  emission_target_matrix[,"Value"] <- approx(x=first_data_yr:last_yr,y=emission_target_matrix,method="linear",xout=first_data_yr:last_yr)$y
  #Output
  fleet_emission_target <- as.data.frame(emission_target_matrix) %>%
    cbind(Year=as.numeric(rownames(emission_target_matrix)),Unit="kg CO2",stringsAsFactors = FALSE) %>%
    subset(.,Year%in%c(first_data_yr:last_yr))
  return(list(fleet_emission_target=fleet_emission_target))
}
