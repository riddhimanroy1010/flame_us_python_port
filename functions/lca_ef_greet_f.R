#' lca_ef_greet_f
#' Function: Returns GHG emission factors taken from the GREET model
#' @export
lca_ef_greet_f <- function (ghg=NA,ef_prim_mat_mdl=NA,ef_fuel_prod_mdl=NA){
  attribute_f("lca_ef_greet_f")
  #Input files
  lca_process <- get_input_f(input_name = 'lca_process')
  conv <- get_input_f(input_name = 'conversion_units')
  fuel_specs <- get_input_f(input_name = 'fuel_specs')
  EF_assembly_GREET <- get_input_f(input_name = 'EF_assembly_greet')
  EF_fuel_GREET <- get_input_f(input_name = 'EF_fuel_greet')
  EF_mat_trans_GREET<- get_input_f(input_name = 'EF_mat_trans_greet')
  EF_mat_prod_GREET <- get_input_f(input_name = 'EF_mat_prod_greet')
  #Adjustment factors for fuel production emissions
  ef_prim_mat_factor <- switch(ef_prim_mat_mdl,"def"=1,"low"=0.9,"high"=1.1)
  ef_fuel_prod_factor <- switch(ef_fuel_prod_mdl,"def"=1,"low"=0.9,"high"=1.1)
  
  #Output file
  lca_process <- lca_process[lca_process$Source=="GREET",]
  ef_greet <- lca_process[lca_process$Source=="GREET",c("Unit","Phase","Process","Source")]
  ef_greet[,"Value"] <- 0
  #Fill the emission factors from GREET
  #Fill env mat for Primary Material production
  for (mat in which(ef_greet$Phase=="Primary Material Production"&ef_greet$Source=="GREET")){
    ef_greet[mat,"Value"] <- EF_mat_prod_GREET[which(EF_mat_prod_GREET$`LCI`==ghg),lca_process$GREET[mat]]/10^3*conv["lb","1 kg"]*ef_prim_mat_factor
  }
  #Fill env mat for Secondary Material production
  for (mat in which(ef_greet$Phase=="Secondary Material Production"&ef_greet$Source=="GREET")){
    ef_greet[mat,"Value"] <- EF_mat_prod_GREET[which(EF_mat_prod_GREET$`LCI`==ghg),lca_process$GREET[mat]]/10^3*conv["lb","1 kg"]
  }
  #Fill env mat for Vehicle assembly
  ef_greet[which(ef_greet$Process=="Vehicle Assembly"),"Value"] <-
    sum(as.numeric(EF_assembly_GREET[which(EF_assembly_GREET$LCI==ghg),strsplit(lca_process$GREET[which(lca_process$Process=="Vehicle Assembly")],",")[[1]]]))/10^3
  #Fill env for manufacturing
  for (mat in which(ef_greet$Phase=="Manufacturing"&!grepl("Assembly",ef_greet$Process))){
    ef_greet[mat,"Value"]<- mean(as.numeric(EF_mat_trans_GREET[which(EF_mat_trans_GREET$LCI==ghg),strsplit(lca_process$GREET[mat],",")[[1]]]))/10^3*conv["lb","1 kg"]
  }
  #Fill env for Fuel production
  for (fuel in which(ef_greet$Phase=="Fuel Production" & ef_greet$Source=="GREET")){
      if (ef_greet$Process[fuel]!="Electricity"){
      ef_greet[fuel,"Value"] <- subset(EF_fuel_GREET, Data=="WTP" & LCI==ghg)[,lca_process$GREET[fuel]]/10^3*conv["mmBTU","1 BTU"]*as.numeric(fuel_specs[ef_greet$Process[fuel],"LHV"])*conv["gal","1 L"]*ef_fuel_prod_factor
    } else if (ef_greet$Process[fuel]=="Electricity") {
      ef_greet[fuel,"Value"] <- subset(EF_fuel_GREET, Data=="WTP" & LCI==ghg)[,lca_process$GREET[fuel]]/10^3*conv["mmBTU","1 kWh"]
    }
  }
  #Fill env for Fuel use
  for (fuel in which(ef_greet$Phase=="Fuel Use"&ef_greet$Source=="GREET")){
      if (!is.na(EF_fuel_GREET[which(EF_fuel_GREET$Data=="PTW"&EF_fuel_GREET$LCI==ghg),lca_process$GREET[fuel]])){
        ef_greet[fuel,"Value"] <- subset(EF_fuel_GREET, Data=="PTW" & LCI==ghg)[,lca_process$GREET[fuel]]/10^3*EF_fuel_GREET[which(EF_fuel_GREET$Data=="Fuel Economy"),lca_process$GREET[fuel]]*conv["gal","1 L"]
      }else {
        ef_greet[fuel,"Value"]<-0
    }
  }
  #Fill env for End of life
  ef_greet[ef_greet$Process=="Vehicle Disposal","Value"] <- sum(as.numeric(subset(EF_assembly_GREET,LCI==ghg)[,strsplit(subset(lca_process,Process=="Vehicle Disposal")$GREET,",")[[1]]]))/10^3
  return(list(lca_ef_greet=ef_greet))
}
