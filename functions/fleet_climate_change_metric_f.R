#' fleet_climate_change_metric_f
#' Function: Calculate climate change metrics of the U.S. LDV fleet
#' @import tidyr
#' @import reshape2
#' @export
fleet_climate_change_metric_f <- function(){
  attribute_f("fleet_climate_change_metric_f")
  #Get LC Inventory by GHG
  fleet_lci_f_res <- do.call(fun_res_f,list(fun_name="fleet_lci_f"))
  fleet_lci <- fleet_lci_f_res[["fleet_lci"]]
  #Output list
  results_l <- list()
  results_l[["emi_dt"]] <- fleet_lci
  for (ghg in unique(fleet_lci$GHG)){
    #Create matrix of annual emissions by process (unit = kg CO2)
    emission_matrix <- acast(data=subset(fleet_lci,GHG==ghg), Sector + Phase + Process ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Get resulting climate changes metric matrices
    climate_change_metric_f_res <- do.call(climate_change_metric_f,list(emission_matrix=emission_matrix,ghg=ghg))
    #Save the matrices into dataframes
    for (mat_name in setdiff(names(climate_change_metric_f_res),"units")){
      mat <- climate_change_metric_f_res[[mat_name]]
      #Transform matrices into data frames
      dt <- as.data.frame(mat) %>%
        cbind(Temp=rownames(mat),stringsAsFactors = FALSE) %>% 
        gather("Year","Value",-Temp,convert=TRUE)
      dt[,"Sector"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[1])
      dt[,"Phase"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[2])
      dt[,"Process"] <- sapply(strsplit(dt$Temp,"_"),function(x)x[3])
      dt[,"Unit"] <- climate_change_metric_f_res[["units"]][[mat_name]]
      dt[,"GHG"] <- ghg
      dt <- dt[,c("GHG","Sector","Phase","Process","Year","Value","Unit")]
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
