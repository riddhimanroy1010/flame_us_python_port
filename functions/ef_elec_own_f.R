#' ef_elec_own_f
#' Function: Returns GHG emission factors of elecricity production calculated from source specific emission factors and source mixes.
#' @export
ef_elec_own_f <- function(ghg=NA,elec_methane_leak_rate=NA){
  attribute_f("ef_elec_own_f")
  #function's output
  EF_elec_GREET  <- get_input_f(input_name = 'EF_elec_GREET')
  elec_source_ev_f_res <- do.call(fun_res_f,list(fun_name="elec_source_ev_f"))
  matrix_mix <- elec_source_ev_f_res[["matrix_mix"]]
  #Input
  #EF_us_elec_prod  <- get_input_f(input_name = 'ef_us_elec')
  elec_techno_match <- get_input_f(input_name = "model_matching_electricity")
  unit_name <- paste("kg",ghg,"/ kWh")
  #Extract emission factors from GREET.Note: No methane leakage rates.
  #Create the matrix with the emission factors: Consider the mean of the EFs.
  matrix_ef_elec_source <- matrix(sapply(unique(elec_techno_match$Own),function(x)sum(subset(EF_elec_GREET,Technology==subset(elec_techno_match,Own==x)[,"GREET"])[,ghg])/10^3),ncol=length(unique(elec_techno_match$Own)),nrow=1,dimnames=list(unit_name,unique(elec_techno_match$Own)))
  #Note methane leakage is modelled. Add methane leakage
  if (ghg=="CH4"){
    conv <- get_input_f(input_name = 'conversion_units')
    fuel_specs <- get_input_f(input_name = 'fuel_specs')
    ch4_leakage_rate <- switch(elec_methane_leak_rate,"def"=0.023,"low"=0.009,"high"=0.04)
    ng_power_plant_efficiency <- 0.501 #Derived from GREET2020 based on a mix of 84.2% CC turbine, 8.8% boiler and 6.1% SC turbine
    add_ch4_emissions <- ch4_leakage_rate*1/as.numeric(fuel_specs["Natural gas","LHV"])*conv["BTU","1 kWh"]*as.numeric(fuel_specs["Natural gas","Density"])/ng_power_plant_efficiency/10^3
    #Add emission rate to matrix
    matrix_ef_elec_source[1,"Natural Gas"] <- matrix_ef_elec_source[1,"Natural Gas"]+add_ch4_emissions
  }
  matrix_ef_elec <- matrix_ef_elec_source %*% matrix_mix[colnames(matrix_ef_elec_source),]
  return(list(matrix_ef_elec=matrix_ef_elec))
}
