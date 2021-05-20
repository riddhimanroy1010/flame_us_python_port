#' fleet_rebound_vkt_f
#' Function: Calculates rebound effect factors of travel demand by vehicle technology, size and model year.
#' @export
##>Function: Calculate the direct rebound effect of vkt by driving cost by technology
fleet_rebound_vkt_f <- function(first_yr = NA, last_yr = NA, rebound_effect_mdl = NA, rebound_effect_value= NA){
  attribute_f("fleet_rebound_vkt_f")
  fleet_driving_cost_per_vkt_f_res <- do.call(fun_res_f,list(fun_name="fleet_driving_cost_per_vkt_f"))
  fleet_driving_cost <- fleet_driving_cost_per_vkt_f_res[["fleet_driving_cost"]]
  fleet_driving_cost[,"Age"] <- fleet_driving_cost[,"Year"]-fleet_driving_cost[,"Model_year"]
  #Other parameters
  last_age_tbc <- 30
  re_value = switch(rebound_effect_mdl,
                    "n"=0,
                    "def"=-0.1,
                    "high"=-0.3,
                    "own"=as.numeric(rebound_effect_value))
  base_technology = "ICEV-G"
  base_year = first_yr
  #Create the matrix base
  matrix_driving_cost_base <- fleet_driving_cost %>%
    subset(Year==base_year & Age<=last_age_tbc) %>%
    acast(., Size+Technology ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Create useful matrix. Identity matrices for calculations
  car_identity_matrix <- diag(x = as.numeric(grepl("Car",rownames(matrix_driving_cost_base))), nrow=nrow(matrix_driving_cost_base), ncol=nrow(matrix_driving_cost_base))
  dimnames(car_identity_matrix) <- list(rownames(matrix_driving_cost_base),rownames(matrix_driving_cost_base))
  lt_identity_matrix <- diag(x = as.numeric(grepl("Light truck",rownames(matrix_driving_cost_base))), nrow=nrow(matrix_driving_cost_base), ncol=nrow(matrix_driving_cost_base))
  dimnames(lt_identity_matrix) <- list(rownames(matrix_driving_cost_base),rownames(matrix_driving_cost_base))
  #Create price base matrix
  matrix_price_base_car <- diag(x=1/matrix_driving_cost_base[paste0("Car_",base_technology),])
  dimnames(matrix_price_base_car) <- list(0:last_age_tbc,0:last_age_tbc)
  matrix_price_base_lt <- diag(x=1/matrix_driving_cost_base[paste0("Light truck_",base_technology),])
  dimnames(matrix_price_base_lt) <- list(0:last_age_tbc,0:last_age_tbc)
  #Output
  fleet_rebound_vkt <- NULL
  for (year in first_yr:last_yr){
    #Get fuel consumption by fuel type
    matrix_driving_cost <- fleet_driving_cost %>%
      subset(Year==year & Age<=last_age_tbc) %>%
      acast(., Size+Technology ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Calculate the rebound effect
    matrix_rebound <- re_value*
      (((car_identity_matrix[rownames(matrix_driving_cost),rownames(matrix_driving_cost)] %*% matrix_driving_cost) %*% matrix_price_base_car + 
         (lt_identity_matrix[rownames(matrix_driving_cost),rownames(matrix_driving_cost)] %*% matrix_driving_cost) %*% matrix_price_base_lt) - 1) + 1
    #Convert matrix into long table
    tmp_rebound_effect <- as.data.frame(matrix_rebound) %>% 
      cbind(Type=rownames(matrix_rebound),stringsAsFactors = FALSE) %>% 
      gather("Age","Value",-Type,convert=TRUE) %>%
      cbind(Year=year)
    tmp_rebound_effect[,"Size"] <- substr(tmp_rebound_effect$Type,0,as.numeric(regexpr(pattern="_",tmp_rebound_effect$Type))-1)
    tmp_rebound_effect[,"Technology"] <- substr(tmp_rebound_effect$Type,as.numeric(regexpr(pattern="_",tmp_rebound_effect$Type))+1,200)
    tmp_rebound_effect[,"Type"] <- NULL
    #Combine
    fleet_rebound_vkt <- rbind(fleet_rebound_vkt,tmp_rebound_effect)
  }
  #Format
  fleet_rebound_vkt[,"Model_year"] <- fleet_rebound_vkt[,"Year"]-fleet_rebound_vkt[,"Age"]
  #Delete rows with maximum rebound, if rebound effect. Meaning no driving costs. Except if re_value is null.
  if (re_value!=0){
    fleet_rebound_vkt <- subset(fleet_rebound_vkt,Value!=(1-re_value))
  }
  return(list(fleet_rebound_vkt=fleet_rebound_vkt))
}
