#' read_def_outputs_f
#'
#' Read outputs of functions for a given scenario with default attribute values.
#' @param function_tbc Function
#' @param sub_function_tbc Subfunction if specified.
#' @param scen_tbc Scenario to consider
#' @export
read_scenario_f<-function(outputs_path,function_tbc,sub_function_tbc="n",scen_tbc){
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  load(file=paste0(outputs_path,"/results/scen_",function_tbc,"_",scen_tbc,".RData"))
  #Output
  def_output_l <- list()
  #Case with multiple function output
  if (sub_function_tbc=="n"){
    dts_names_l <- setdiff(names(results_l[[1]]),"Scenario")
  } else {
    dts_names_l <- names(results_l[[1]][[sub_function_tbc]])
  }
  for (dts_name in dts_names_l){
    #Clean the environment
    dts1 <- NULL
    for (i in 1:length(results_l)){
      if (sub_function_tbc=="n"){
        temp_dt <- results_l[[i]][[dts_name]]
      } else {
        temp_dt <- results_l[[i]][[sub_function_tbc]][[dts_name]]
      }
      #Update temp_dt with the scenario name
      temp_dt[,"Scenario"] <- colnames(scen_attributes)[sapply(seq_len(ncol(scen_attributes)),function(x)all(scen_attributes[,x]==results_l[[i]][["Scenario"]][rownames(scen_attributes)]))]
      #Merge results
      dts1 <- rbind(dts1,temp_dt)
    }
    def_output_l[[length(def_output_l) + 1]] <- dts1
    #Rename new element with par and par_value
    names(def_output_l)[length(def_output_l)] <- dts_name
  }
  return(def_output_l)
}

#' read_simulation_f
#'
#' Function that read the output "dts_names" of simulation "sim_tbc" of function "function_tbc"
#' @param function_tbc Function
#' @param sub_function_tbc Subfunction if specified
#' @param dts_name Name of the dataframe
#' @param scen_tbc Scenario to consider
#' @param sim_tbc Simulation
#' @param sim_type Type of simulation
#' @export
read_simulation_f<-function(outputs_path,
                            function_tbc,
                            sub_function_tbc="n",
                            dts_name,
                            scen_tbc,
                            sim_tbc,
                            sim_type="discrete"){
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  #Get simulation attributes with values
  if (sim_type=="discrete"){
    sim_attributes <- get_discrete_simulation_attributes(outputs_path,sim_tbc)
  } else if(sim_type=="continuous"){
    sim_attributes <- get_continuous_simulation_attributes(outputs_path,sim_tbc)
  }
  attr_to_keep <- get_simulation_attribute_to_keep(outputs_path,sim_tbc,sim_type)
  #Rdata to load
  load(paste0(outputs_path,"/results/sim_",function_tbc,"_",scen_tbc,"_",sim_tbc,"_",sim_type,".RData"))
  #Output
  output_l <- list()
  dts <- NULL
  for (i in 1:length(results_l)){
    if (sub_function_tbc=="n"){
      temp_dt <- results_l[[i]][[dts_name]]
    } else {
      temp_dt <- results_l[[i]][[sub_function_tbc]][[dts_name]]
    }
    if (!is.null(temp_dt)){
      #Update temp_dt
      temp_dt[,"Scenario"] <- colnames(scen_attributes)[sapply(seq_len(ncol(scen_attributes)),function(x)all(scen_attributes[,x]==results_l[[i]][["Scenario"]][rownames(scen_attributes)]))]
      temp_dt[,"Simulation"] <- colnames(sim_attributes)[sapply(seq_len(ncol(sim_attributes)),function(x)all(sim_attributes[,x]==results_l[[i]][["Simulation"]][rownames(sim_attributes)]))]
      if (length(attr_to_keep)>0){
        temp_dt <- cbind(temp_dt,data.frame(as.list(results_l[[i]][["Simulation"]][attr_to_keep]),stringsAsFactors = FALSE))
      }
    }
    #Merge results
    dts <- rbind(dts,temp_dt)
  }
  return(dts)
}

#' read_sens_analysis_f
#'
#' Function that read the outputs "dts_names" of sensitivty analysis "sens_tbc" or on complete sensitivity analysis.
#' @param function_tbc Function
#' @param sub_function_tbc Subfunction if specified
#' @param dts_name Name of the dataframe
#' @param scen_tbc Scenario to consider
#' @param sens_tbc Sensitivity
#' @param def_results Shall it consider default values?
#' @export
read_sens_analysis_f<-function(outputs_path,
                               function_tbc,
                               sub_function_tbc="n",
                               dts_name,
                               sens_tbc,
                               scen_tbc,
                               def_results="y"){
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  #Get the list of attributes to include in the sensitivity analysis
  sens_analysis_attribute_list <- get_sens_analysis_attributes(outputs_path,sens_tbc,function_tbc)
  #Read output files
  load(file=paste0(outputs_path,"/results/sens_a_",function_tbc,"_",sens_tbc,"_",scen_tbc,".RData"))
  #Clean the environment
  #Get default table
  if (def_results=="y"){
    results <- do.call(read_scenario_f,list(outputs_path=outputs_path,function_tbc=function_tbc,sub_function_tbc=sub_function_tbc,scen_tbc=scen_tbc))
    dts <- results[[dts_name]]
    dts[,"Sens_attribute"] <- "all"
    dts[,"Sens_attribute_value"] <- "Default"
    dts[,"Sens_attribute_description"] <- "Default"
  } else{
    dts <- NULL
  }
  for (i in 1:length(results_l)){
    if (sub_function_tbc=="n"){
      temp_dt <- results_l[[i]][[dts_name]]
    } else {
      temp_dt <- results_l[[i]][[sub_function_tbc]][[dts_name]]
    }
    if (!is.null(temp_dt)){
      #Extract parameter names from name
      attribute_name <- names(results_l[[i]][["Sensitivity"]])
      #Extract parameter value from name
      attribute_value <- results_l[[i]][["Sensitivity"]][[attribute_name]]
      #Update temp_dt
      temp_dt[,"Scenario"] <- colnames(scen_attributes)[sapply(seq_len(ncol(scen_attributes)),function(x)all(scen_attributes[,x]==results_l[[i]][["Scenario"]][rownames(scen_attributes)]))]
      temp_dt[,"Sens_attribute"] <- attribute_name
      temp_dt[,"Sens_attribute_name"] <- get_attribute_name(attribute_name)
      temp_dt[,"Sens_attribute_value"] <- attribute_value
      #Merge results
      dts <- rbind(dts,temp_dt)
    }
  }
  return(dts)
}


#' read_multi_factor_sens_a_f
#'
#' Function that read the outputs "dts_names" of sensitivty analysis "sens_tbc" or on complete sensitivity analysis.
#' @param function_tbc Function
#' @param sub_function_tbc Subfunction if specified
#' @param dts_name Name of the dataframe
#' @param scen_tbc Scenario to consider
#' @param sens_tbc Sensitivity
#' @param mf_sens_tbc Multi-factor sensitivity
#' @param def_results Shall it consider default values?
#' @export
read_multi_factor_sens_a_f <- function(outputs_path,
                                       function_tbc,
                                       sub_function_tbc = "n",
                                       dts_name,
                                       mf_sens_tbc,
                                       sens_tbc,
                                       scen_tbc,
                                       def_results = "y"){
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(outputs_path,scen_tbc)
  #Get name of the sensitivity analysis
  sens_tbc_name <- paste(sens_tbc,collapse = "-")
  #Write results
  load(file=paste0(outputs_path,"/results/mf_sens_a_",function_to_write,"_",mf_sens_tbc,"_",sens_tbc_name,"_",scen_tbc,".RData"))
  #Clean the environment
  #Get default table
  if (def_results=="y"){
    results <- do.call(read_scenario_f,list(outputs_path=outputs_path,function_tbc=function_tbc,sub_function_tbc=sub_function_tbc,scen_tbc=scen_tbc))
    dts <- results[[dts_name]]
    dts[,"Sens_analysis"] <- "all"
  } else{
    dts <- NULL
  }
  for (i in 1:length(results_l)){
    if (sub_function_tbc=="n"){
      temp_dt <- results_l[[i]][[dts_name]]
    } else {
      temp_dt <- results_l[[i]][[sub_function_tbc]][[dts_name]]
    }
    if (!is.null(temp_dt)){
      #Update temp_dt
      temp_dt[,"Scenario"] <- colnames(scen_attributes)[sapply(seq_len(ncol(scen_attributes)),function(x)all(scen_attributes[,x]==results_l[[i]][["Scenario"]][rownames(scen_attributes)]))]
      temp_dt[,"Sens_analysis"] <- results_l[[i]][["Multi-factor sensitivity"]][["Scenario bounding case"]]
      #Merge results
      dts <- rbind(dts,temp_dt)
    }
  }
  return(dts)
}

