#' fleet_env_matrix_f
#' Function: Creates a matrix of GHG emission factors for the U.S. LDV fleet to calculate fleet life cycle GHG emissions
#' @export
fleet_env_matrix_f <- function(first_yr=NA,last_yr=NA,ghg=NA){
  attribute_f("fleet_env_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Functions' outputs
  #
  lca_ef_elec_f_res <- do.call(lca_ef_elec_f,list(ghg=ghg))
  lca_ef_elec <- subset(lca_ef_elec_f_res[["lca_ef_elec"]],Year%in%first_yr:last_yr)
  #
  lca_ef_greet_f_res <- do.call(lca_ef_greet_f,list(ghg=ghg))
  lca_ef_greet <- lca_ef_greet_f_res[["lca_ef_greet"]]
  #
  lca_ef_lit_f_res <- do.call(lca_ef_lit_f,list(ghg=ghg))
  lca_ef_lit <- lca_ef_lit_f_res[["lca_ef_lit"]]
  #Output
  fleet_env_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))
  #Fill environmental matrix with GREET emission factors
  for (i in which(lca_process$Source=="GREET")){
    fleet_env_matrix[i,] <- as.numeric(subset(lca_ef_greet,Phase==lca_process[i,"Phase"] & Process==lca_process[i,"Process"])$Value)
  }
  #Fill environmental matrix with literature emission factors except battery
  for (i in which(lca_process$Source=="lit")){
    fleet_env_matrix[i,] <- as.numeric(subset(lca_ef_lit,Phase==lca_process[i,"Phase"] & Process==lca_process[i,"Process"] & Technology%in%c("BEV100",NA))$Value)
  }
  #Fill environmental matrix with ecoInvent emissions factors for electricity production
  fleet_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- lca_ef_elec$Value[order(lca_ef_elec$Year)]
  return(list(fleet_env_matrix=fleet_env_matrix))
}
