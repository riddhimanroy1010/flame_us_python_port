#' backcasting_procedure_f
#' Function: Runs a backcasting procedure to balance climate change targets with climate change impacts of the LDV fleet. Perform the backcasting on an unique attribute.
#' @export
backcasting_procedure_f <- function(s_and_f_ghg=NA,
                          s_and_f_metric=NA,
                          s_and_f_target_year=NA,
                          s_and_f_target_sector=NA,
                          s_and_f_attribute_name=NA,
                          s_and_f_max_attribute_value=NA,
                          s_and_f_cut_off=NA){
  attribute_f("backcasting_procedure_f")
  #Get the list of depend functions. Useful to erase data.
  list_depend_fct <- get_dependent_functions(attribute_list=s_and_f_attribute_name)
  #Delete all previously saved data that are sensitive to s_and_f_attribute_name
  del_fun_res(list_depend_fct)
  i_year <- 2015
  #Output
  dt_col <- c("Attribute","Attribute_value","Target","Score")
  seek_and_find_results <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  dt_col <- c("Attribute","Target_achieved","Attribute_value","Target","Score")
  target_results <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

  # Calculate the emission profile target -------------------------------------------
  
  fleet_emission_target_f_res <- do.call(fun_res_f,list(fun_name="fleet_emission_target_f",fast_mode="n"))
  fleet_emission_target <- fleet_emission_target_f_res[["fleet_emission_target"]]
  
  # Calculate the emission profile in default case --------------------------

  fleet_climate_change_metric_f_ref <- do.call(fun_res_f,list(fun_name="fleet_climate_change_metric_f",fast_mode="n"))
  gwp_dt_sector <- fleet_climate_change_metric_f_ref[["gwp_dt"]]
  ##Specify the sectors to consider and aggregate emissions by sector
  if (s_and_f_target_sector=="all"){
    sector_tbc <- unique(gwp_dt_sector$Sector)
  } else {
    sector_tbc <- unlist(strsplit(s_and_f_target_sector,split=","))
  }
  ##Specify the GHG emissions and aggregate emission by GHG
  if (s_and_f_ghg=="all"){
    ghg_tbc <- unique(gwp_dt_sector$GHG)
  } else {
    ghg_tbc <- unlist(strsplit(s_and_f_ghg,split=","))
  }
  gwp_dt <- aggregate(data=subset(gwp_dt_sector,Sector%in%sector_tbc & GHG%in%ghg_tbc),Value ~ Year,FUN=sum)
  
  #Calculate the target depending on the metric
  #first get the name of the dataframe to consider from the climate change metric function
  metric_dt_name <- switch(s_and_f_metric,"cum_gwp"="gwp","cum_rf"="crf","cum_gtc"="agt")
  #Convert emission profile into appropriate metric
  mat_co2 <- acast(data=fleet_emission_target, Unit ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  mat_co2 <- mat_co2/mat_co2[1,as.character(i_year)]*subset(gwp_dt,Year==i_year)$Value
  climate_change_metric_f_res <- do.call(climate_change_metric_f,list(emission_matrix=mat_co2,ghg="CO2"))
  #Calculate the metric target. Assume cumulative value
  target <- sum(climate_change_metric_f_res[[metric_dt_name]][1,as.character(i_year:s_and_f_target_year)])
  
  ##Get values of the metric in the default case
  metric_value_def <- sum(subset(fleet_climate_change_metric_f_ref[[paste0(metric_dt_name,"_dt")]],Sector%in%sector_tbc & GHG%in%ghg_tbc & Year%in%c(i_year:s_and_f_target_year))$Value)
  
  ##Get the current value of the attribute to change. Then update the list of attribtues with value
  attribute_value_def <- as.numeric(get_attribute_value(s_and_f_attribute_name))
  ##Save the values in data.frame
  seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(s_and_f_attribute_name,attribute_value_def,target,metric_value_def)
  
  ##We seek an attribute value that achieves more reductions (so more negative changes) than the target
  ##If metric_value_def is lower than target, target is achieved for default value
  if (metric_value_def <= target){
    #Save no result
    target_results[nrow(target_results)+1,] <- c(s_and_f_attribute_name,"y by def",attribute_value_def,target,metric_value_def)
  ##Otherwise, calculate case with high value
  } else {
    
    # Calculate the emission changes in extreme case --------------------------
    
    ##Update value to maximum then calculate value
    update_attribute_values(setNames(as.list(s_and_f_max_attribute_value),s_and_f_attribute_name))
    ##Clean the results of dependent functions in the saved environment and local
    del_fun_res(list_depend_fct)
    ##Obtain metric value
    fleet_climate_change_metric_f_ref <- do.call(fun_res_f,list(fun_name="fleet_climate_change_metric_f",fast_mode="n"))
    metric_value_max <- sum(subset(fleet_climate_change_metric_f_ref[[paste0(metric_dt_name,"_dt")]],Sector%in%sector_tbc & GHG%in%ghg_tbc & Year%in%c(i_year:s_and_f_target_year))$Value)
    ##Save the values in data.frame
    seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(s_and_f_attribute_name,s_and_f_max_attribute_value,target,metric_value_max)
    
    ##If the metric with maximum attribute value is lower than the target, proceed by dichotomy to find the value
    if (metric_value_max <= target){
      lower_bound <- attribute_value_def
      higher_bound <- s_and_f_max_attribute_value
      attribute_value <- (lower_bound+higher_bound)/2
      ##As long as attribute value bounds are higher than cut-off interval
      while ((higher_bound-lower_bound)>s_and_f_cut_off){
        ##Calculate the emission changes for the new attribute value
        update_attribute_values(setNames(as.list(attribute_value),s_and_f_attribute_name))
        del_fun_res(list_depend_fct)
        fleet_climate_change_metric_f_ref <- do.call(fun_res_f,list(fun_name="fleet_climate_change_metric_f",fast_mode="n"))
        metric_value <- sum(subset(fleet_climate_change_metric_f_ref[[paste0(metric_dt_name,"_dt")]],Sector%in%sector_tbc & GHG%in%ghg_tbc & Year%in%c(i_year:s_and_f_target_year))$Value)
        seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(s_and_f_attribute_name,attribute_value,target,metric_value)
        #Update lower and higher bounds
        if (metric_value <= target){
          lower_bound <- lower_bound
          higher_bound <- attribute_value
        } else{
          lower_bound <- attribute_value
          higher_bound <- higher_bound
        }
        attribute_value <- (lower_bound+higher_bound)/2
      }
      #Save final result
      target_results[nrow(target_results)+1,] <- c(s_and_f_attribute_name,"y",attribute_value,target,metric_value)
    ##If metric_value_max is higher than target, target never achieved
    } else {
      #Save no result
      target_results[nrow(target_results)+1,] <- c(s_and_f_attribute_name,"n",NA,target,metric_value_max)
    }
  }
  ##Update value back to default value
  update_attribute_values(setNames(as.list(attribute_value_def),s_and_f_attribute_name))
  
  # Format final output -----------------------------------------------------
  
  dt_col <- c("Attribute_value","Target","Score")
  seek_and_find_results[,dt_col] <- sapply(dt_col,function(x) as.numeric(seek_and_find_results[,x]))
  target_results[,dt_col] <- sapply(dt_col,function(x) as.numeric(target_results[,x]))
  return(list(seek_and_find_results=seek_and_find_results,target_results=target_results))
}
