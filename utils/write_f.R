#' write_scenario_f
#'
#' Run and write outputs of functions for a given scenario with default attribute values.
#' @param function_tbc List of functions to simulate
#' @param scen_tbc Scenario to consider
#' @export
write_scenario_f <- function(outputs_path,function_tbc,scen_tbc){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  #Get the list of depend functions. Useful to erase data.
  list_depend_fct <- get_dependent_functions(attribute_list=rownames(scen_attributes))
  for (i in 1:length(function_tbc)){
    #Output files
    results_l <- list()
    fct <- function_tbc[i]
    #Delete all saved results
    del_fun_res("all")
    #Loop on scenarios
    for (scen_number in seq_len(ncol(scen_attributes))){
      #Reset all attribute values to default values
      reset_attribute_values()
      #Update the attribute values with the scenario's values
      update_attribute_values(scen_attributes[,scen_number,drop=FALSE])
      #Delete all saved results
      del_fun_res(list_depend_fct)
      #Get function results
      results <- append(do.call(get(fct), list()), setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,scen_number])),"Scenario"))
      #Store results in a new list element
      results_l[[length(results_l)+1]] <- results
    }
    save(list="results_l",file=paste0(outputs_path,"/results/scen_",fct,"_",scen_tbc,".RData"))
  }
  #Reset all attribute values to default values
  reset_attribute_values()
  #Delete all saved results
  del_fun_res("all")
}

#' write_simulation_f
#'
#' Run and write outputs of functions for a given scenario and a given simulation number.
#' @param function_tbc List of functions to simulate
#' @param scen_tbc Scenario to consider
#' @param sim_tbc Simulation to consider
#' @param sim_type Type of simulation discrete or continuous
#' @export
write_simulation_f <- function(outputs_path,function_tbc,scen_tbc,sim_type,sim_tbc){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get simulation attributes with values
  if (sim_type=="discrete"){
    sim_attributes <- get_discrete_simulation_attributes(outputs_path,sim_tbc)
  } else if(sim_type=="continuous"){
    sim_attributes <- get_continuous_simulation_attributes(outputs_path,sim_tbc)
  }
  list_depend_fct_simulation <- get_dependent_functions(attribute_list=rownames(sim_attributes))
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  list_depend_fct_scenario <- get_dependent_functions(attribute_list=rownames(scen_attributes))
  #Output files
  results_l <- list()
  #Loop on scenarios
  for (scen_number in seq_len(ncol(scen_attributes))){
    #Reset all attribute values to default values
    reset_attribute_values()
    #Update the attribute values with the scenario's values
    update_attribute_values(scen_attributes[,scen_number,drop=FALSE])
    #Clear environment with function related to scenario attributes
    del_fun_res(list_depend_fct_scenario)
    #Loop on sensitivty analysis attributes
    for (sim_number in seq_len(ncol(sim_attributes))){
      #Update the attribute values with the simulation's values
      update_attribute_values(sim_attributes[,sim_number,drop=FALSE])
      #Clear the functions' results
      del_fun_res(list_depend_fct_simulation)
      #Get function results
      results <- append(do.call(get(function_tbc), list()),
                        append(setNames(list(sapply(rownames(sim_attributes),function(x)sim_attributes[x,sim_number])),"Simulation"),
                               setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,scen_number])),"Scenario")))
      #Store results in a new list element
      results_l[[length(results_l) + 1]] <- results
    }
  }
  #Delete all saved results
  del_fun_res("all")
  #Write results
  save(list="results_l",file=paste0(outputs_path,"/results/sim_",function_tbc,"_",scen_tbc,"_",sim_tbc,"_",sim_type,".RData"))
  reset_attribute_values()
}

#' write_sens_analysis_f
#'
#' Run and write function outputs of single-factor sensitivty anaysis
#' @param function_tbc Function to simulate (length=1)
#' @param scen_tbc Scenario to consider
#' @param sens_tbc Sensitivity analysis to consider
#' @param get_default_attr_val Shall it consider the default values of the assessed attributes?
#' @export
write_sens_analysis_f<-function(outputs_path,function_tbc,scen_tbc,sens_tbc,get_default_attr_val="n"){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  list_depend_fct_scenario <- get_dependent_functions(attribute_list=rownames(scen_attributes))
  #Get the list of attributes to include in the sensitivity analysis
  sens_analysis_attribute_list <- get_sens_analysis_attributes(outputs_path,sens_tbc,function_tbc)
  #Output files
  results_l <- list()
  del_fun_res("all")
  #Loop on scenarios
  for (scen_number in seq_len(ncol(scen_attributes))){
    #Clear environment
    del_fun_res(list_depend_fct_scenario)
    #Loop on sensitivty analysis attributes
    for (sens_attribute in sens_analysis_attribute_list){
      #Reset all attribute values to default values
      reset_attribute_values()
      #Update the attribute values with the scenario's values
      update_attribute_values(scen_attributes[,scen_number,drop=FALSE])
      #Get all attribute values
      sens_attribute_value_list <- get_all_attribute_value(sens_attribute,get_default_attr_val=get_default_attr_val)
      #Get list of functions that are dependant on the attribute
      list_depend_fct_attribute <- get_dependent_functions(attribute_list=sens_attribute)
      if (length(sens_attribute_value_list)>=1){
        for (sens_attribute_value in sens_attribute_value_list){
          #Clear the environment that contains functions results. Only clear the appropriate function results
          del_fun_res(list_depend_fct_attribute)
          #Update with the attribute value
          update_attribute_values(setNames(list(sens_attribute_value),sens_attribute))
          #Get function results
          results <- append(do.call(get(function_tbc), list()),
                            append(setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,scen_number])),"Scenario"),
                                   list(Sensitivity=setNames(sens_attribute_value,sens_attribute))))
          #Store results in a new list element
          results_l[[length(results_l)+1]] <- results
        }
      }
      #Delete all saved results
      del_fun_res(list_depend_fct_attribute)
    }
    
  }
  #Write results
  save(list="results_l",file=paste0(outputs_path,"/results/sens_a_",function_tbc,"_",sens_tbc,"_",scen_tbc,".RData"))
  reset_attribute_values()
  del_fun_res("all")
}


#' write_multi_factor_sens_a_f
#'
#' Run and write function outputs of multi-factor sensitivty anaysis
#' @param function_to_write Function to simulate (length=1)
#' @param scen_tbc Scenario to consider
#' @param mf_sens_tbc Multi-factor sensitivity analysis to consider
#' @param sens_tbc Sensitivity analysis to consider for attributes
#' @export
write_multi_factor_sens_a_f <- function(outputs_path,function_to_write,scen_tbc,sens_tbc,mf_sens_tbc){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get sensitivity analysis parameters
  mf_s_a_attributes <- get_mf_sens_analysis_attributes(outputs_path,mf_sens_tbc)
  #Input: Get bounding cases per scenario
  tornado_f_res <- do.call(tornado_f,list(function_tbc=mf_s_a_attributes[["function_to_eval"]],
                                          sub_function_tbc=mf_s_a_attributes[["subfunction_to_eval"]],
                                          dts_name=mf_s_a_attributes[["dts_name"]],
                                          scen_tbc=scen_tbc,
                                          sens_tbc=sens_tbc,
                                          subset_conditions_l=mf_s_a_attributes[["subset_conditions_l"]],
                                          variable_value_name=mf_s_a_attributes[["variable_value_name"]],
                                          variable_to_aggregate=mf_s_a_attributes[["variable_to_aggregate"]],
                                          cumulative=mf_s_a_attributes[["cumulative"]]))
  matrix_attribute_values <- tornado_f_res[["matrix_attribute_values"]]
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  list_depend_fct <- get_dependent_functions(attribute_list=c(rownames(scen_attributes),setdiff(rownames(matrix_attribute_values),c("Scenario name","Scenario bounding case"))))
  #Output
  results_l <- list()
  for (i in 1:ncol(matrix_attribute_values)){
    #Delete previous results
    del_fun_res(list_depend_fct)
    #Reset all attribute values to default values
    reset_attribute_values()
    #Update attribute values with scenario
    update_attribute_values(scen_attributes[,matrix_attribute_values["Scenario name",i],drop=FALSE])
    #Update attribute values with bounding case
    update_attribute_values(matrix_attribute_values[!rownames(matrix_attribute_values)%in%c("Scenario name","Scenario bounding case"),i,drop=FALSE])
    #Get function results
    results <- append(do.call(get(function_to_write), list()),
                      append(setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,matrix_attribute_values["Scenario name",i]])),"Scenario"),
                             list("Multi-factor sensitivity"=matrix_attribute_values[!rownames(matrix_attribute_values)%in%c("Scenario name"),i,drop=TRUE])))
    #Store results in a new list element
    results_l[[length(results_l)+1]] <- results
  }
  sens_tbc_name <- paste(sens_tbc,collapse = "-")
  #Write results
  save(list="results_l",file=paste0(outputs_path,"/results/mf_sens_a_",function_to_write,"_",mf_sens_tbc,"_",sens_tbc_name,"_",scen_tbc,".RData"))
  #Delete all saved results
  del_fun_res("all")
  #Update attribute values to use by default values
  reset_attribute_values()
}

