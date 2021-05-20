#' fleet_vint_stock_reg_f
#' Function: Creates the vintaging stock model of the light-duty fleet per technology type, age and year
#' @import modelframework
#' @import tidyr
#' @importFrom reshape2 colsplit
#' @export
fleet_vint_stock_reg_f <-function(aeo_scen=NA, survival_rate_mdl=NA, first_yr=NA, last_yr = NA) {

  # Function set up ---------------------------------------------------------
  
  attribute_f("fleet_vint_stock_reg_f")
  #Functions' outputs
  fleet_sales_reg_f_res <- do.call(fun_res_f,list(fun_name="fleet_sales_reg_f"))
  matrix_sales <- fleet_sales_reg_f_res[["matrix_sales"]]

  # Other parameters and output ---------------------------------------------

  #i_year is the initilization year for the model. It has to be lower than first_yr. Preferably as old as the first stock and sales data to allow the model to correct the stocks
  i_year <- 2011
  last_age_tbc <- 30
  matrix_vint_stock_list <- list()
  fleet_vint_stock <- NULL

  # Matrices tool -----------------------------------------------------------
    
  #Create useful matrix. Identity matrices for calculations
  car_identity_matrix <- diag(x = as.numeric(grepl("Car",rownames(matrix_sales))), nrow=nrow(matrix_sales), ncol=nrow(matrix_sales))
  dimnames(car_identity_matrix) <- list(rownames(matrix_sales),rownames(matrix_sales))
  lt_identity_matrix <- diag(x = as.numeric(grepl("Light truck",rownames(matrix_sales))), nrow=nrow(matrix_sales), ncol=nrow(matrix_sales))
  dimnames(lt_identity_matrix) <- list(rownames(matrix_sales),rownames(matrix_sales))
  #Survival rates matrix. Not diagonal
  car_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Car", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(car_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  lt_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Light truck", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(lt_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  

  # Vintage stock initialization --------------------------------------------

  #First, calculate the first year matrix stock
  matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(matrix_sales),dimnames = list(rownames(matrix_sales),0:last_age_tbc))
  #Update sales
  matrix_vint_stock[,as.character(0)] <- matrix_sales[rownames(matrix_vint_stock),as.character(i_year)]
  #Save
  matrix_vint_stock_list[[as.character(i_year)]] <- matrix_vint_stock
  #Convert matrix in long table
  tmp_stock_dt <- as.data.frame(matrix_vint_stock) %>% 
    cbind(Type=rownames(matrix_vint_stock),stringsAsFactors = FALSE) %>% 
    gather("Age","Value",-Type,convert=TRUE) %>%
    cbind(Year=i_year)
  tmp_stock_dt <- cbind(colsplit(tmp_stock_dt$Type," - ",c("Region","Size","Technology")),subset(tmp_stock_dt,select=-Type))
  #Combine
  fleet_vint_stock <- rbind(fleet_vint_stock,tmp_stock_dt)

  # Iterative vintage stock -------------------------------------------------

  for (y in (i_year+1):last_yr){
    #Initialize matrix_vint_stock
    matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(matrix_sales),dimnames = list(rownames(matrix_sales),0:last_age_tbc))
    #Update sales
    matrix_vint_stock[,as.character(0)] <- matrix_sales[rownames(matrix_vint_stock),as.character(y)]
    #Update rest of stock based on previous year matrix stock and survival rates
    matrix_vint_stock[,as.character(1:last_age_tbc)] <- 
      car_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
      (matrix_vint_stock_list[[as.character(y-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% car_surv_rate_matrix) +
      lt_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
      (matrix_vint_stock_list[[as.character(y-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% lt_surv_rate_matrix)
    #Update the list
    matrix_vint_stock_list[[as.character(y)]] <- matrix_vint_stock
    #Convert matrix in long table
    tmp_stock_dt <- as.data.frame(matrix_vint_stock) %>% 
      cbind(Type=rownames(matrix_vint_stock),stringsAsFactors = FALSE) %>% 
      gather("Age","Value",-Type,convert=TRUE) %>%
      cbind(Year=y)
    tmp_stock_dt <- cbind(colsplit(tmp_stock_dt$Type," - ",c("Region","Size","Technology")),subset(tmp_stock_dt,select=-Type))
    #Combine
    fleet_vint_stock <- rbind(fleet_vint_stock,tmp_stock_dt)
  }
  fleet_vint_stock$Value <- trunc(fleet_vint_stock$Value)
  fleet_vint_stock <- subset(fleet_vint_stock,Value!=0)
  return(list(fleet_vint_stock=fleet_vint_stock))
  }
