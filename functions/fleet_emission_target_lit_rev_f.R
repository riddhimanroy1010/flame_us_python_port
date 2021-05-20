#' fleet_emission_target_lit_rev_f
#' Function: Calculates GHG emission budgets for the US LDV fleet derived from a literature review that are consistent with given climate change targets
#' @export
fleet_emission_target_lit_rev_f<-function(last_yr=NA){
  #Assign default value
  attribute_f(fun_name = "fleet_emission_target_lit_rev_f")
  #Inputs
  ldv_emissions  <- get_input_f(input_name = 'lit_rev_ldv_co2_budget')
  source="Miotti (2016)"
  first_data_yr=2015
  emission_target_dt <- ldv_emissions %>%
    subset(.,Source==source & Year%in%c(first_data_yr:last_yr)) %>%
    aggregate(formula=Value~Year+Unit,data=.,FUN=sum)
  #Convert in kg CO2
  emission_target_dt$Value <- emission_target_dt$Value*10^9
  emission_target_dt$Unit <- "kg CO2"
  return(list(fleet_emission_target=emission_target_dt))
}
