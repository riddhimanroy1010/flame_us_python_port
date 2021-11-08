#' fleet_lci_f
#' Function: el
#' @import modelframework
#' @import tidyr
#' @export
fleet_lci_f <- function(){
  attribute_f("fleet_lci_f")
  #Inputs
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Functions' Outputs
  #
  fleet_demand_matrix_f_res <- do.call(fun_res_f,list(fun_name="fleet_demand_matrix_f"))
  fleet_demand_matrix <- fleet_demand_matrix_f_res[["fleet_demand_matrix"]]
  fleet_lci <- NULL
  for (ghg in c("CO2","CH4","N2O")){
    fleet_env_matrix_f_res <- do.call(fleet_env_matrix_f,list(ghg=ghg))
    fleet_env_matrix <- fleet_env_matrix_f_res[["fleet_env_matrix"]]
    #Calculate the Life Cycle Inventory matrix
    fleet_lci_matrix <- fleet_demand_matrix * fleet_env_matrix
    #Create long table
    tmp_dt <- as.data.frame(fleet_lci_matrix) %>% 
      cbind(lca_process[,c("Sector","Phase","Process")],stringsAsFactors = FALSE) %>% 
      gather("Year","Value",-c(Sector,Phase,Process),convert=TRUE) %>% 
      cbind(GHG=ghg,Unit="kg",stringsAsFactors = FALSE)
    #Remove zeros
    tmp_dt <- subset(tmp_dt,Value!=0)
    fleet_lci <- rbind(fleet_lci,tmp_dt)
  }
  return(list(fleet_lci=fleet_lci))
}
