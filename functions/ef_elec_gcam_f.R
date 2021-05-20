#' ef_elec_gcam_f
#' Function: Returns GHG emission factors of electricity production derived from the GCAM model
#' @export
ef_elec_gcam_f <- function(ssp_scen=NA, ssp_mitigation_scen=NA, last_yr=NA){
  #Assign default value
  attribute_f(fun_name = "ef_elec_gcam_f")
  #Inputs
  ssp_emissions  <- get_input_f(input_name = 'gcam_ef_elec')
  #Other parameters
  emission_factor_dt <- subset(ssp_emissions,Scenario==paste0("SSP",ssp_scen,"_",ssp_mitigation_scen) & Year<=last_yr)
  first_data_yr <- min(emission_factor_dt$Year)
  #Create matrix with final emissions
  emission_factor_matrix <- matrix(NA,ncol=length(first_data_yr:last_yr),nrow=1,dimnames = list("Value",first_data_yr:last_yr))
  emission_factor_matrix["Value",as.character(emission_factor_dt$Year)] <- emission_factor_dt$Value
  #Perform the linear regression #Assumption: Light-duty vehicles are responsible for 65.7% of these emissions.
  emission_factor_matrix["Value",] <- approx(x=first_data_yr:last_yr,y=emission_factor_matrix,method="linear",xout=first_data_yr:last_yr)$y
  return(list(matrix_ef_elec=emission_factor_matrix))
}
