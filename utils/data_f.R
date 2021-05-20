#' cum_long_dtf_f
#'
#' Create a data frame with the cumulative values for long tables
#' @param dtf Input dataframe
#' @param cum_variable Variable (column) we want to cumulate
#' @param cum_param Variable (column) leading the accumulation (generally time, such as years)
#' @param out_var Variable to eliminate in the accumulation
#' @export
cum_long_dtf_f<-function(dtf,
                         cum_variable="Value",
                         cum_param="Year",
                         out_var=NULL){
  cum_dtf <- dtf
  param_col <- which(!colnames(dtf) %in% c(cum_variable,cum_param,out_var))
  set_param <- data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE)
  for (r in 1:nrow(set_param)){
    #rows contains the rows in the data frame to calculate the cumulative function
    rows <- which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=","))
    #Order rows from the smaller to higher values of cum_param
    rows <- rows[order(dtf[rows,cum_param])]
    #Calculate cumulative values for cum_variable and substitute the values in the dataframe
    cum_dtf[rows,cum_variable] <- cumsum(dtf[rows,cum_variable])
  }
  return(cum_dtf)
}

#' delta_long_dtf_f
#'
#' Create a data frame with the delta values for long tables
#' @param dtf Input dataframe
#' @param x_axis variable representing the x axis (usually Year)
#' @param y_axis variable representing the values (usually value)
#' @param ref_var variable containing the scenarios to compare with a reference case
#' @param ref_case reference case in the variable ref_var
#' @param out_var variable that we don't want to include (useful for sensitivity analysis)
#' @export
delta_long_dtf_f<-function(dtf,
                           x_axis,
                           y_axis,
                           ref_var,
                           ref_case,
                           out_var=NULL){
  delta_dtf <- dtf
  #param_col contains the column numbers of the variables to consider
  param_col <- which(!colnames(dtf) %in% c(x_axis,y_axis,out_var))
  #set_param contains the unique set of parameters to be considered
  set_param <- data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param) <- colnames(dtf)[param_col]
  #Create the data frame with the reference case associated with each set of parameters
  set_param_ref <- set_param
  set_param_ref[,ref_var] <- ref_case
  for (r in 1:nrow(set_param)){
    #rows contains the rows in the data frame to calculate the cumulative function
    rows <- which(sapply(1:nrow(delta_dtf),function(x)paste(delta_dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=","))
    ref_rows <- which(sapply(1:nrow(delta_dtf),function(x)paste(delta_dtf[x,param_col],collapse=",")) %in% paste(set_param_ref[r,],collapse=","))
    x_axis_l<-unique(delta_dtf[rows,x_axis])
    for (x_val in x_axis_l){
      ref_value <- delta_dtf[intersect(ref_rows,which(delta_dtf[,x_axis]==x_val)),y_axis]
      delta_dtf[intersect(rows,which(delta_dtf[,x_axis]==x_val)),paste0("delta_",y_axis)] <- delta_dtf[intersect(rows,which(delta_dtf[,x_axis]==x_val)),y_axis]-ref_value
    }
  }
  return(delta_dtf)
}

#' pay_back_time_f
#'
#' Compute the pay_back time
#' @param dtf Input dataframe
#' @param x_axis variable representing the x axis (usually Year)
#' @param y_axis variable representing the values (usually value)
#' @param i_x_val initial value of the x_axis to consider
#' @param f_x_val initial value of the x_axis to consider
#' @param ref_var variable containing the scenarios to compare with a reference case
#' @param ref_case reference case in the variable ref_var

#' @export
pay_back_time_f<-function(dtf,
                          y_axis,
                          x_axis,
                          i_x_val=2016,
                          f_x_val="last",
                          ref_var,
                          ref_case){
  #param_col contains the column numbers of the variables to consider
  param_col<-which(!colnames(dtf) %in% c(y_axis,x_axis))
  #set_param contains the unique set of parameters to be considered
  set_param<-data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param)<-colnames(dtf)[param_col]
  #Delete the rows that contain the reference case
  set_param<-data.frame(set_param[set_param[,ref_var]!=ref_case,],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param)<-colnames(dtf)[param_col]
  #Create the data frame with the reference case associated with each set of parameters
  set_param_ref<-set_param
  set_param_ref[,ref_var]<-ref_case
  #Output file
  pay_back_dt<-set_param
  for (r in 1:nrow(set_param)){
    if (i_x_val=="lw_first_yr"){
      first_yr<-as.numeric(set_param[r,i_x_val])
    } else {
      first_yr<-as.numeric(i_x_val)
    }
    #rows contains the rows in the data frame to calculate the cumulative function
    rows<-intersect(which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=",")),
                    which(dtf[,x_axis]>=(first_yr-1)))
    ref_rows<-intersect(which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param_ref[r,],collapse=",")),
                        which(dtf[,x_axis]>=(first_yr-1)))
    #Order rows from the smaller to higher values of cum_param
    rows<-rows[order(dtf[rows,x_axis])]
    ref_rows<-ref_rows[order(dtf[ref_rows,x_axis])]
    #Create temp_dt: Difference between case and reference case
    temp_dt<-data.frame(dtf[rows,y_axis]-dtf[ref_rows,y_axis])
    rownames(temp_dt)<-dtf[rows,x_axis]
    if (any(temp_dt<0)){
      #row_min is row which the difference is 0 or negative
      row_min<-min(which(temp_dt<0))
      #Estimate pay_back time
      if (row_min==1){
        pay_back=0
      } else {
        pay_back<-as.numeric(rownames(temp_dt)[row_min-1])-temp_dt[row_min-1,]/(temp_dt[row_min,]-temp_dt[row_min-1,])-first_yr+1
      }
    } else {
      pay_back<-NA
    }
    pay_back_dt[r,"Payback"]<-pay_back
    if (f_x_val=="last"){
      pay_back_dt[r,"Final changes"]<-temp_dt[nrow(temp_dt),1]
    }

  }
  return(pay_back_dt)
}

#' get_changes_baseline
#'
#' Return the changes at year f_year compared to the baseline at year i_year
#' @param dts Input dataframe
#' @param i_year Initial year
#' @param f_year Final year
#' @param cumulative_changes Shall it consider the cumulative changes?
#' @param absolute_changes Shall it consider the absolute changes?
#' @export
get_changes_baseline <- function(dts,
                                 i_year,
                                 f_year,
                                 cumulative_changes,
                                 absolute_changes){
  ##If cumulative target, we assume the initial year as the baseline and calculate the annual reductions up to f_year
  ##Otherwise, consider annual reduction at f_year
  if (cumulative_changes=="y"){
    changes_value <- switch(absolute_changes,
                            "y"=sum(subset(dts,Year%in%i_year:f_year)$Value - subset(dts,Year==i_year)$Value),
                            "n"=sum(subset(dts,Year%in%i_year:f_year)$Value) / (subset(dts,Year==i_year)$Value*(f_year-i_year+1))-1)
  } else {
    changes_value <- switch(absolute_changes,
                            "y"=subset(dts,Year==f_year)$Value - subset(dts,Year==i_year)$Value,
                            "n"=subset(dts,Year==f_year)$Value / subset(dts,Year==i_year)$Value-1)

  }
  return(changes_value)
}
