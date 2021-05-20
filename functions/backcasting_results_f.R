#' backcasting_results_f
#' Function: Runs the backasting procedure to balance climate change target with climate change impacts of the LDV fleet. Then returns the result of some functions with the backcasted value of the attribute.
#' @export
backcasting_results_f <- function(){
  attribute_f("backcasting_results_f")
  #Function
  backcasting_procedure_f_res <- do.call(fun_res_f,list(fun_name="backcasting_procedure_f"))
  target_results <- backcasting_procedure_f_res[["target_results"]]
  #Other parameters: function_tbc are the functions' results to show
  function_tbc <- c("fleet_vint_stock_f","vehicle_module_f","fleet_fuel_u_f","fleet_battery_flow_f","fleet_climate_change_metric_f")
  if(target_results$Target_achieved=="n"){
    return(setNames(rep(list(NULL),times=length(function_tbc)),function_tbc))
  } else if(target_results$Target_achieved%in%c("y","y by def")){
    #Get results from considered functions
    return(setNames(lapply(function_tbc,function(x)do.call(fun_res_f,list(fun_name=x,fast_mode=fast_mode))),function_tbc))
  }
}
