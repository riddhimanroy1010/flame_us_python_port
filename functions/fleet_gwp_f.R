#' fleet_gwp_f
#' Function: Calculate life cycle Global Warming Potential of the U.S. LDV fleet by life cycle processes, stages and GHGs.
#' @import tidyr
#' @export
fleet_gwp_f <- function(gwp_th=NA,gwp_cc_fb=NA){
  attribute_f("fleet_gwp_f")
  #Get LC Inventory by GHG
  fleet_lci_f_res <- do.call(fun_res_f,list(fun_name="fleet_lci_f"))
  fleet_lci <- fleet_lci_f_res[["fleet_lci"]]
  #Input
  ipcc_gwp = get_input_f(input_name = "ipcc_gwp")
  #Create output file
  fleet_gwp <- fleet_lci
  #Characterize the GHGs
  fleet_gwp$Value <- sapply(1:nrow(fleet_gwp),function(x)ifelse(fleet_gwp[x,"GHG"]=="CO2",fleet_gwp[x,"Value"],fleet_gwp[x,"Value"]*subset(ipcc_gwp,Time_horizon==gwp_th & cc_fb==gwp_cc_fb & GHG==fleet_gwp[x,"GHG"])$Value))
  fleet_gwp$Unit <- "kg CO2 eq."
  return(list(fleet_gwp=fleet_gwp))
}
