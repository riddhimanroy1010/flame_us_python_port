#' fleet_vkt_reg_f
#' Function: DEPRECIATED. Calculates distance traveled by the U.S. LDV fleet by technology, size, age, year and region in kilometers.
#' @import tidyr
#' @importFrom reshape2 acast
#' @export
fleet_vkt_reg_f<-function (first_yr = NA,last_yr = NA,vkt_model=NA){
  attribute_f("fleet_vkt_reg_f")
  #Input
  vh_techno <- get_input_f("model_matching_technology")
  conv  <- get_input_f(input_name = 'conversion_units')
  annual_mileage  <- get_input_f(input_name = 'annual_mileage_TEDB')
  #Function outputs
  fleet_vint_stock_reg_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_reg_f"))
  fleet_vint_stock <- fleet_vint_stock_reg_f_res[["fleet_vint_stock"]]
  #vkt_growth_f is a function that returns the growth of VKMT of a given year
  if (vkt_model == "Growth"){
    #Create VKMT growth function: Based on VISION's model
    vkt_growth_f<-function(y){
      growth_rate = 0.008
      years_to_zero_growth = 200
      discount_rate = -growth_rate/(years_to_zero_growth*0.8)
      ref_y = 2015
      t = ifelse(ref_y>y,0,y-ref_y)
      return(exp(growth_rate*(t)+discount_rate*(t)^2))
    }
  } else if (vkt_model == "Constant"){
    #No growth in VKMT
    vkt_growth_f<-function(y){
      return(1)
    }
  } else if (vkt_model == "linear_def"){
    #Growth in VKMT following linear trends based on historical calculation
    vkt_growth_f<-function(y){
      growth_rate = 0.004
      ref_y = 2015
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  } else if (vkt_model == "linear_high"){
    #Growth in VKMT following linear trends based on historical calculation
    vkt_growth_f<-function(y){
      growth_rate = 0.008
      ref_y = 2015
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  }
  #Output files: fleet_vint_vkt contains the distance traveled by technology, size, age and year in kilometers
  fleet_vint_vkt <- NULL
  for (size in c("Car", "Light truck")) {
    vec_vkt_growth <- matrix(vkt_growth_f(first_yr:last_yr),ncol = 1,nrow = length(first_yr:last_yr),dimnames = list(first_yr:last_yr,"Growth Rate"))
    vec_annual_kt <- matrix(annual_mileage[order(annual_mileage$`Vehicle age`),size]*conv["km","1 mile"],ncol = max(fleet_vint_stock$Age)+1,nrow = 1,dimnames = list("Annual KT per vehicle",unique(fleet_vint_stock$Age)))
    for (techno in unique(vh_techno$Own)) {
      for (reg in unique(fleet_vint_stock$Region)){
        #Create matrix of vintaged stock. Rows: Year. Cols: Age
        stock <- subset(fleet_vint_stock,Size==size & Technology == techno & Region==reg & Year%in%(first_yr:last_yr))
        matrix_stock <- acast(stock, Year ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
        #Create matrix of annual vkt by age. Rows: Year. Cols: Age
        matrix_vkt <- matrix_stock * ((vec_vkt_growth %*% vec_annual_kt)[rownames(matrix_stock),colnames(matrix_stock)])
        #Convert matrix in long table
        tmp_vkt_dt <- as.data.frame(matrix_vkt) %>% 
          cbind(Year=as.numeric(rownames(matrix_vkt)),stringsAsFactors = FALSE) %>% 
          gather("Age","Value",-Year,convert=TRUE) %>% 
          cbind(Technology=techno,Size=size,Region=reg,stringsAsFactors = FALSE)
        #Combine
        fleet_vint_vkt <- rbind(fleet_vint_vkt,tmp_vkt_dt)
      }
    }
  }
  fleet_vint_vkt[,"Unit"] <- "km"
  results<-list(fleet_vint_vkt=fleet_vint_vkt)
  return(results)
}
