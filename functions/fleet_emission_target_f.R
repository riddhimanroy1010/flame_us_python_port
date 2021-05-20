#' fleet_emission_target_f
#' Function: Calculates GHG emission budgets for the US LDV fleet that are consistent with given climate change targets
#' @export
fleet_emission_target_f<-function(ghg_budget_mdl=NA){
  #Assign default value
  attribute_f(fun_name = "fleet_emission_target_f")
  if (ghg_budget_mdl=="gcam"){
    res <- do.call(fun_res_f,list(fun_name="fleet_emission_target_gcam_f",fast_mode="n"))
  } else if(ghg_budget_mdl=="lit_rev"){
    res <- do.call(fun_res_f,list(fun_name="fleet_emission_target_lit_rev_f",fast_mode="n"))
  }
  fleet_emission_target <- res[["fleet_emission_target"]]
  return(list(fleet_emission_target=fleet_emission_target))
}
