#' arrows_ribbon_f
#'
#' Function that returns a dataframe to plot the ribbons and arrows between lines
#' @param dtf Input dataframe
#' @param y_axis variable on the Y axis
#' @param x_axis variable on the x axis
#' @param var_color variable the represent the different lines (color)
#' @param ref_var let the user defines if the arrows ribbons for each observation start with the highest observation ("reference") or following the decreasing order ("order")
#' @export
arrows_ribbon_f<-function(dtf,
                          y_axis,
                          x_axis,
                          var_color,
                          ref_var="order",
                          order_var="decreasing"){
  #Parameters
  #x_axis_l contains the list of the x_axis value
  x_axis_l<-unique(dtf[,x_axis])
  #var_color_order is the order of the lines to consider. We assume the order to the last x_axis_l value order.
  if (order_var=="decreasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=TRUE)]
  } else if (order_var=="simulation"){
    var_color_order<-unique(dtf[,var_color])
  } else if (order_var=="increasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=FALSE)]
  }
  #var_color_ref is the var_color of reference. Lines and ribbons start from there
  var_color_ref<-var_color_order[1]
  for (c in 2:length(var_color_order)){
    if (ref_var=="order"){
      dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_order[c-1],y_axis]
    } else if (ref_var=="reference"){
      dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_ref,y_axis]
    }
    dtf[dtf[,var_color]==var_color_order[c],"y_end"]<-dtf[dtf[,var_color]==var_color_order[c],y_axis]
    dtf[dtf[,var_color]==var_color_order[c],"delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"y_end"]-dtf[dtf[,var_color]==var_color_order[c],"y_start"]
    dtf[dtf[,var_color]==var_color_order[c],"rel_delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"delta_y"]/dtf[dtf[,var_color]==var_color_order[c],"y_start"]
  }
  dtf[,var_color]<-factor(dtf[,var_color],levels=var_color_order)

  return(dtf)
}

#' tornado_f
#'
#' Returns a data frame of the sensitivty analysis results for function_tbc with the , the best case scenario arguments values and worst case scenario arguments values.
#' @param variable_value_name variable representing the values (usually value)
#' @param subset_conditions_l List of conditions (in text) for the subset of the data.frame.
#' @param variable_to_aggregate Variables or Columns to aggregate. Ex. c(Age,Size,Technology)
#' @param cumulative Use cumulative values or not?
#' @export
tornado_f <- function(function_tbc,
                      sub_function_tbc = "n",
                      dts_name,
                      scen_tbc,
                      sens_tbc,
                      subset_conditions_l,
                      variable_value_name = "Value",
                      variable_to_aggregate,
                      cumulative = "n"){
  #Input
  scen_attributes <- get_scenario_attributes(scen_tbc)
  all_sens_analysis_attribute_list <- unique(unlist(lapply(sens_tbc,function(x)get_sens_analysis_attributes(x,function_tbc))))
  #Output: Tornado dtf
  dt_col <- c("Scenario","Sens_analysis","Sens_attribute","Sens_attribute_description","min","def","max")
  tornado_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Output: Extreme values per
  matrix_attribute_values <- matrix(NA,ncol=ncol(scen_attributes)*2,nrow=length(all_sens_analysis_attribute_list)+2,dimnames=list(c("Scenario name","Scenario bounding case",all_sens_analysis_attribute_list),1:(ncol(scen_attributes)*2)))
  matrix_attribute_values["Scenario bounding case",] <- unlist(lapply(c("min","max"),function(x)rep(x,times=ncol(scen_attributes))))
  matrix_attribute_values["Scenario name",] <- rep(colnames(scen_attributes),times=2)
  for (sens_a in sens_tbc){
    sens_a_dt <- do.call(read_sens_analysis_f,list(function_tbc=function_tbc,sub_function_tbc=sub_function_tbc,dts_name=dts_name,sens_tbc=sens_a,scen_tbc=scen_tbc,def_results="y"))
    #Calculative cumulative values if specified
    if (cumulative=="y"){
      #Calculate cumulative results
      sens_a_dt <- cum_long_dtf_f(dtf=sens_a_dt,cum_variable = variable_value_name,cum_param="Year")
    }
    #Subset the data.frame
    if (!any(is.na(subset_conditions_l))){
      sens_a_dt <- sens_a_dt[Reduce('&',lapply(subset_conditions_l,function(x)with(sens_a_dt,eval(parse(text=x))))),]
    }
    #Aggregate variables
    if (!any(is.na(variable_to_aggregate))){
      agg.formula <- reformulate(termlabels = setdiff(colnames(sens_a_dt),c(variable_to_aggregate,variable_value_name)),response = variable_value_name)
      sens_a_dt <- aggregate(formula=agg.formula,data = sens_a_dt,FUN=sum)
    }

    # #if case_ref is "n", means that no delta is considered, only the absolute value
    # if (case_ref=="n"){
    #   delta_dtf <- dtf
    #   #scen_list: List of scenarios to consider in calculations. All scenarios if no reference case
    #   scen_list <- unique(sens_a_dt$Scenario)
    # } else {
    #   #Calculate difference between case_ref and case_tbc for each sensitivity analysis
    #   delta_dtf <- delta_long_dtf_f(dtf=dtf, x_axis=x_axis, y_axis=y_axis, ref_var=var_tbc, ref_case=case_ref)
    #   #Replace old value columns by delta value columns
    #   delta_dtf <- delta_dtf[,colnames(delta_dtf)!=y_axis]
    #   colnames(delta_dtf)[colnames(delta_dtf)==paste0("delta_",y_axis)] <- y_axis
    #   #scen_list: List of scenarios to consider in calculations. All scenarios except reference case
    #   scen_list <- setdiff(unique(delta_dtf[,var_tbc]),case_ref)
    # }
    #sens_a_attribute_l: List of attributes in the sensivitiy analyses
    sens_a_attribute_l <- get_sens_analysis_attributes(sens_a,function_tbc)
    for (sens_a_attr in sens_a_attribute_l){
      #attr_description is the description of the attribute. If NA in the table, consider sens_a_attr
      attr_description <- ifelse(is.na(get_attribute_description(sens_a_attr)),sens_a_attr,get_attribute_description(sens_a_attr))
      for (scen in colnames(scen_attributes)){
        #Get default attribute value
        if (sens_a_attr %in% rownames(scen_attributes)){
          def_attr_val <- scen_attributes[sens_a_attr,scen]
        } else {
          def_attr_val <- get_default_value(sens_a_attr)
        }
        #Create temporary one row dataframe
        temp_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
        #Get default value. Check if only one value
        if (nrow(subset(sens_a_dt,Scenario==scen & Sens_attribute=="all" & Sens_attribute_value=="Default"))==1){
          def_val <- subset(sens_a_dt,Scenario==scen & Sens_attribute=="all" & Sens_attribute_value=="Default")[,variable_value_name]
        } else {
          return(warning("Multiple values: Insufficient conditions in the subset"))
        }
        #Subset the data with the corresponding scenario and attribute value
        subset_dt <- subset(sens_a_dt,Sens_attribute==sens_a_attr & Scenario==scen)
        #min_row is the row of min variable_value_name value (either minimum delta compared to case_ref or minimum value)
        min_row <- which.min(subset_dt[,variable_value_name])
        min_val <- subset_dt[min_row,variable_value_name]
        max_row <- which.max(subset_dt[,variable_value_name])
        max_val <- subset_dt[max_row,variable_value_name]
        if (min_val <= def_val & max_val >= def_val){
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="min" & matrix_attribute_values["Scenario name",]==scen] <- subset_dt[min_row,"Sens_attribute_value"]
          temp_dt[1,"min"] <- min_val
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="max" & matrix_attribute_values["Scenario name",]==scen] <- subset_dt[max_row,"Sens_attribute_value"]
          temp_dt[1,"max"] <- max_val
        } else if (min_val > def_val){
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="min" & matrix_attribute_values["Scenario name",]==scen] <- def_attr_val
          temp_dt[1,"min"] <- def_val
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="max" & matrix_attribute_values["Scenario name",]==scen] <- subset_dt[max_row,"Sens_attribute_value"]
          temp_dt[1,"max"] <- max_val
        } else if (max_val < def_val){
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="min" & matrix_attribute_values["Scenario name",]==scen] <- subset_dt[min_row,"Sens_attribute_value"]
          temp_dt[1,"min"] <- min_val
          matrix_attribute_values[sens_a_attr,matrix_attribute_values["Scenario bounding case",]=="max" & matrix_attribute_values["Scenario name",]==scen] <- def_attr_val
          temp_dt[1,"max"] <- def_val
        }
        temp_dt[1,c("Scenario","Sens_analysis","Sens_attribute","Sens_attribute_description","def")] <- c(scen,sens_a,sens_a_attr,attr_description,def_val)
        tornado_dt <- rbind(tornado_dt,temp_dt)
      }
    }
  }
  #Format numeric
  dt_col <- c("min","def","max")
  tornado_dt[,dt_col] <- sapply(dt_col,function(x) as.numeric(tornado_dt[,x]))
  #Calculate delta in tornado_dtf
  tornado_dt[,"delta"] <- tornado_dt[,"min"]-tornado_dt[,"max"]
  return(list(tornado_dt=tornado_dt,
              matrix_attribute_values=matrix_attribute_values))
}

#' get_tornado_plot_dt_f
#' Obtain the dataframe to plot a tornado with the parameters from the specified multi-factor sensitivity analysis
#'
#' @export
get_tornado_plot_dt_f <- function(mf_sens_tbc,
                                  scen_tbc,
                                  sens_tbc){
  #Load attribute values
  load_attribute_value()
  #Get sensitivity analysis parameters
  mf_s_a_attributes <- get_mf_sens_analysis_attributes(mf_sens_tbc)
  #Input: Get bounding cases per scenario
  tornado_f_res <- do.call(tornado_f,list(function_tbc=mf_s_a_attributes[["function_to_eval"]],
                                          sub_function_tbc=mf_s_a_attributes[["subfunction_to_eval"]],
                                          dts_name=mf_s_a_attributes[["dts_name"]],
                                          scen_tbc=scen_tbc,
                                          sens_tbc=sens_tbc,
                                          subset_conditions_l=mf_s_a_attributes[["subset_conditions_l"]],
                                          variable_value_name=mf_s_a_attributes[["variable_value_name"]],
                                          variable_to_aggregate=mf_s_a_attributes[["variable_to_aggregate"]],
                                          cumulative=mf_s_a_attributes[["cumulative"]]))
  dt <- tornado_f_res[["tornado_dt"]]
  return(dt)
}

#' get_tornado_dataframe
#'
#' @export
get_tornado_dataframe <- function(dts,
                                  subset_conditions_l,
                                  variable_value_name = "Value",
                                  variable_to_aggregate){
  tornado_dt <- dts
  if (length(subset_conditions_l)>0){
    tornado_dt <- dts[Reduce('&',lapply(subset_conditions_l,function(x)with(dts,eval(parse(text=x))))),]
  }
  #Aggregate variables
  if (length(variable_to_aggregate)>0){
    agg.formula <- reformulate(termlabels = setdiff(colnames(tornado_dt),c(variable_to_aggregate,variable_value_name)),response = variable_value_name)
    tornado_dt <- aggregate(formula=agg.formula,data = tornado_dt,FUN=sum)
  }

  #Parameters
  #x_axis_l contains the list of the x_axis value
  x_axis_l<-unique(dtf[,x_axis])
  #var_color_order is the order of the lines to consider. We assume the order to the last x_axis_l value order.
  if (order_var=="decreasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=TRUE)]
  } else if (order_var=="simulation"){
    var_color_order<-unique(dtf[,var_color])
  } else if (order_var=="increasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=FALSE)]
  }
  #var_color_ref is the var_color of reference. Lines and ribbons start from there
  var_color_ref<-var_color_order[1]
  for (c in 2:length(var_color_order)){
    if (ref_var=="order"){
      dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_order[c-1],y_axis]
    } else if (ref_var=="reference"){
      dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_ref,y_axis]
    }
    dtf[dtf[,var_color]==var_color_order[c],"y_end"]<-dtf[dtf[,var_color]==var_color_order[c],y_axis]
    dtf[dtf[,var_color]==var_color_order[c],"delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"y_end"]-dtf[dtf[,var_color]==var_color_order[c],"y_start"]
    dtf[dtf[,var_color]==var_color_order[c],"rel_delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"delta_y"]/dtf[dtf[,var_color]==var_color_order[c],"y_start"]
  }
  dtf[,var_color]<-factor(dtf[,var_color],levels=var_color_order)

  return(dtf)
}
