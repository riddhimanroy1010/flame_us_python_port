#' fleet_vkt_f
#' Function: Calculates distance traveled by the U.S. LDV fleet by technology, size, age and year in kilometers.
#' @export
fleet_vkt_f <- function(first_yr = NA, last_yr = NA, vkt_model=NA, vkt_growth_rate=NA){
  attribute_f("fleet_vkt_f")
  #Input
  vh_techno <- get_input_f("model_matching_technology")
  conv  <- get_input_f(input_name = 'conversion_units')
  annual_mileage  <- get_input_f(input_name = 'annual_mileage_TEDB')
  #Function outputs
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_rebound_vkt_f_res <- do.call(fun_res_f,list(fun_name="fleet_rebound_vkt_f"))
  fleet_rebound_vkt <- fleet_rebound_vkt_f_res[["fleet_rebound_vkt"]]
  #vkt_growth_f is a function that returns the growth of VKMT of a given year
  if (vkt_model == "Growth"){
    #Create VKMT growth function: Based on VISION's model
    vkt_growth_f<-function(y){
      growth_rate = 0.0081
      years_to_zero_growth = 200
      discount_rate = -growth_rate/(years_to_zero_growth*0.8)
      ref_y = 2019
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
      ref_y = 2019
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  } else if (vkt_model == "linear_high"){
    #Growth in VKMT following linear trends based on historical calculation
    vkt_growth_f<-function(y){
      growth_rate = 0.008
      ref_y = 2019
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  } else if (vkt_model == "linear_low"){
    #Growth in VKMT following linear trends based on historical calculation
    vkt_growth_f<-function(y){
      growth_rate = -0.008
      ref_y = 2019
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  } else if (vkt_model == "user"){
    #Growth in VKMT defines by user
    vkt_growth_f<-function(y){
      growth_rate = as.numeric(vkt_growth_rate)
      ref_y = 2019
      t = ifelse(ref_y>y,0,y-ref_y)
      return((1+growth_rate)^t)
    }
  
  }
  #
  vec_vkt_growth <- matrix(vkt_growth_f(first_yr:last_yr),ncol = 1,nrow = length(first_yr:last_yr),dimnames = list(first_yr:last_yr,"Growth Rate"))
  #Output files: fleet_vint_vkt contains the distance traveled by technology, size, age and year in kilometers
  dt_col <- c("Size","Technology","Age","Year","Value")
  fleet_vint_vkt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (size in c("Car", "Light truck")) {
    for (techno in unique(vh_techno$Own)) {
      #Create matrix of vintaged stock. Rows: Year. Cols: Age
      stock <- subset(fleet_vint_stock,Size==size & Technology == techno & Year%in%(first_yr:last_yr))
      matrix_stock <- acast(stock, Year ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
      vec_annual_kt <- matrix(annual_mileage[order(annual_mileage$`Vehicle age`),size]*conv["km","1 mile"],ncol = length(unique(stock$Age)),nrow = 1,dimnames = list("Annual KT per vehicle",unique(stock$Age)))
      #matrix_rebound_vkt contains the direct rebound effect values associatied with driving costs
      matrix_rebound_vkt <- subset(fleet_rebound_vkt,Size==size & Technology == techno & Year%in%(first_yr:last_yr)) %>%
        acast(., Year ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
      #Create matrix of annual vkt by age. Rows: Year. Cols: Age
      matrix_vkt <- matrix_stock * ((vec_vkt_growth %*% vec_annual_kt)[rownames(matrix_stock),colnames(matrix_stock)] * matrix_rebound_vkt[rownames(matrix_stock),colnames(matrix_stock)])
      #Convert matrix in long table
      tmp_vkt_dt <- as.data.frame(matrix_vkt) %>% 
        cbind(Year=as.numeric(rownames(matrix_vkt)),stringsAsFactors = FALSE) %>% 
        gather("Age","Value",-Year,convert=TRUE) %>% 
        cbind(Technology=techno,Size=size,stringsAsFactors = FALSE)
      #Combine
      fleet_vint_vkt <- rbind(fleet_vint_vkt,tmp_vkt_dt)
    }
  }
  fleet_vint_vkt[,"Unit"] <- "km"
  #Output files: fleet_vkt contains the total distance traveled by technology, size and year in km
  agg.formula <- reformulate(termlabels = setdiff(colnames(fleet_vint_vkt),c("Age","Value")),response = "Value")
  fleet_vkt <- aggregate(data = fleet_vint_vkt, Value ~ Size + Technology + Year,FUN=sum)
  #Output file: fleet_vkt_new contains the total distance traveled by technology, size and year for new vehicles in km
  fleet_vkt_new <- subset(fleet_vint_vkt, Age==0)
  results<-list(fleet_vint_vkt=fleet_vint_vkt,
                fleet_vkt=fleet_vkt,
                fleet_vkt_new=fleet_vkt_new)
  return(results)
}
