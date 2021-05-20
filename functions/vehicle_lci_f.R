#' vehicle_lci_f
#' Function: Calculates vehicle-level life cycle GHG emissions of U.S. LDVs for all vehicle technologies and sizes by process, phase and GHGs.
#' @import modelframework
#' @export
vehicle_lci_f <- function(vh_lca_yrs=NA){
  attribute_f("vehicle_lci_f")
  #Inputs
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Other parameters
  years_tbc <- as.numeric(unlist(strsplit(vh_lca_yrs,split=" ")))
  vehicle_lci <- NULL
  for (year in years_tbc){
    for (size in c("Car","Light truck")){
      for (techno in unique(vh_techno$Own)){
        #Get demand matrix
        vehicle_lca_demand_matrix_f_res <- do.call(vehicle_lca_demand_matrix_f,list(techno=techno,size=size,model_year=year))
        vehicle_demand_matrix <- vehicle_lca_demand_matrix_f_res[["vehicle_demand_matrix"]]
        for (ghg in c("CO2","CH4","N2O")){
          #Get environmental matrix
          fleet_env_matrix_f_res <- do.call(fleet_env_matrix_f,list(ghg=ghg))
          fleet_env_matrix <- fleet_env_matrix_f_res[["fleet_env_matrix"]]
          #Calculate the Life Cycle Impacts matrix
          vehicle_lci_matrix <- vehicle_demand_matrix * fleet_env_matrix
          #Create long table
          tmp_dt <- as.data.frame(vehicle_lci_matrix) %>% 
            cbind(lca_process[,c("Sector","Phase","Process")],stringsAsFactors = FALSE) %>% 
            gather("Year","Value",-c(Sector,Phase,Process),convert=TRUE) %>% 
            cbind(Size=size,Technology=techno,Model_year=year,GHG=ghg,Unit="kg",stringsAsFactors = FALSE) %>%
            subset(subset=Value!=0,select=c(Size,Technology,Model_year,Year,GHG,Sector,Phase,Process,Unit,Value))
          #Merge it with final output
          vehicle_lci <- rbind(vehicle_lci,tmp_dt)
        }
      }
    }
  }
  return(list(vehicle_lci=vehicle_lci))
}
