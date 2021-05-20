#' vehicle_climate_change_metric_f
#' Function: Calculate the resulting climate change metric of the fleet life cycle GHG emission inventory
#' @import tidyr
#' @import reshape2
#' @export
vehicle_climate_change_metric_f <- function(){
  attribute_f("vehicle_climate_change_metric_f")
  #Get vehicle emission profil results
  vehicle_lci_f_res <- do.call(fun_res_f,list(fun_name="vehicle_lci_f"))
  vehicle_lci <- vehicle_lci_f_res[["vehicle_lci"]]
  #Output list
  results_l <- list()
  results_l[["emi_dt"]] <- vehicle_lci
  for (ghg in unique(vehicle_lci$GHG)){
    #Create matrix of annual emissions by process (unit = kg CO2)
    emission_matrix <- acast(data=subset(vehicle_lci,GHG==ghg), Model_year + Size + Technology + Sector + Phase + Process ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Get resulting climate changes metric matrices
    climate_change_metric_f_res <- do.call(climate_change_metric_f,list(emission_matrix=emission_matrix,ghg=ghg))
    #Save the matrices into dataframes
    for (mat_name in setdiff(names(climate_change_metric_f_res),"units")){
      mat <- climate_change_metric_f_res[[mat_name]]
      #Transform matrices into data frames
      dt <- as.data.frame(mat) %>%
        cbind(Temp=rownames(mat),stringsAsFactors = FALSE) %>% 
        gather("Year","Value",-Temp,convert=TRUE)
      dt[,"Model_year"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[1])
      dt[,"Size"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[2])
      dt[,"Technology"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[3])
      dt[,"Sector"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[4])
      dt[,"Phase"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[5])
      dt[,"Process"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[6])
      dt[,"Unit"] <- climate_change_metric_f_res[["units"]][[mat_name]]
      dt[,"GHG"] <- ghg
      dt <- dt[,c("GHG","Sector","Phase","Process","Model_year","Year","Size","Technology","Value","Unit")]
      if (exists(paste0(mat_name,"_dt"))){
        assign(paste0(mat_name,"_dt"),rbind(get(paste0(mat_name,"_dt")),dt))
      } else {
        assign(paste0(mat_name,"_dt"),dt)
      }
      results_l[[paste0(mat_name,"_dt")]] <- get(paste0(mat_name,"_dt"))
    }
  }
  return(results_l)
}
