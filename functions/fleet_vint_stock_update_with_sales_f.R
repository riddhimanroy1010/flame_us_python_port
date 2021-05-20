#' fleet_vint_stock_update_with_sales_f
#' Function: Update vintaged fleet stock by vehicle type of a given year from previous year vintaged stock and new sales.
#' @export
#Require: Previous year vintaged stock, current year sales by type, current year stock by type
fleet_vint_stock_update_with_sales_f <- function(fleet,year,survival_rate_mdl=NA,adj_stock = NA){
  attribute_f("fleet_vint_stock_update_with_sales_f")
  #Other parameters
  first_proj_yr <- 2021
  last_age_tbc <- 30
  car_identity_matrix <- diag(x = as.numeric(grepl("Car",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(car_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  lt_identity_matrix <- diag(x = as.numeric(grepl("Light truck",rownames(fleet$ldv_on_road_stock))), nrow=nrow(fleet$ldv_on_road_stock), ncol=nrow(fleet$ldv_on_road_stock))
  dimnames(lt_identity_matrix) <- list(rownames(fleet$ldv_on_road_stock),rownames(fleet$ldv_on_road_stock))
  #Survival rates matrix. Not diagonal
  car_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Car", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(car_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  lt_surv_rate_matrix <- diag(x=sapply(1:last_age_tbc, function (x) do.call(survival_rate_f,list(age=x, size="Light truck", survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(lt_surv_rate_matrix) <- list(1:last_age_tbc,1:last_age_tbc)
  #Initialize matrix_vint_stock
  matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(fleet$ldv_sales),dimnames = list(rownames(fleet$ldv_sales),0:last_age_tbc))
  #Update sales
  matrix_vint_stock[,as.character(0)] <- fleet$ldv_sales[rownames(matrix_vint_stock),as.character(year)]
  #Update rest of stock based on previous year matrix stock and survival rates
  matrix_vint_stock[,as.character(1:last_age_tbc)] <- 
    car_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
    (fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% car_surv_rate_matrix) +
    lt_identity_matrix[rownames(matrix_vint_stock),rownames(matrix_vint_stock)] %*% 
    (fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] %*% lt_surv_rate_matrix)
  #Calculate the matrix_scrap. Contains number of scrapped vehicles at age for the given year
  matrix_scrap <- fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:last_age_tbc)] - cbind(matrix_vint_stock[,as.character(1:last_age_tbc)],0)
  dimnames(matrix_scrap) <- list(rownames(matrix_vint_stock),1:(last_age_tbc+1))
  #Adjust if historical values or if specified
  if (year < first_proj_yr | adj_stock != "n"){
    adj_matrix_vint_stock <- matrix_vint_stock
    est_stock_wo_sales_matrix <- rowSums(matrix_vint_stock[,as.character(1:last_age_tbc)])
    #If "AEO" is specified, we adjust the stock by vehicle based on the AEO stocks. Attention some inconsistencies create less stock than sales. Force to 0
    if (year < first_proj_yr | adj_stock == "aeo"){
      tot_stock_wo_sales_matrix <- fleet$ldv_on_road_stock[rownames(matrix_vint_stock),as.character(year)]-fleet$ldv_sales[rownames(matrix_vint_stock),as.character(year)]
      tot_stock_wo_sales_matrix[tot_stock_wo_sales_matrix<0] <- 0
      #Otherwise, we adjust the stock by type by considering the total stock of AEO and keeping the same proportions that previously calculated
    } else {
      tot_stock_wo_sales_matrix <- (est_stock_wo_sales_matrix[rownames(matrix_vint_stock)]/sum(est_stock_wo_sales_matrix))*(sum(fleet$ldv_on_road_stock[,as.character(year)])-sum(fleet$ldv_sales[,as.character(year)]))
    }
    #diff_stock_wo_sales_matrix contains the differences between the estimated stock by type and wanter stock
    diff_stock_wo_sales_matrix <- as.matrix(tot_stock_wo_sales_matrix[rownames(adj_matrix_vint_stock)]-est_stock_wo_sales_matrix[rownames(adj_matrix_vint_stock)])
    #For negative differences, estimated stock is higher. We decrease evenly all vehicle ages older than 0 by keeping their ratio
    adj_matrix_vint_stock[which(diff_stock_wo_sales_matrix<0),as.character(1:last_age_tbc)] <- matrix_vint_stock[which(diff_stock_wo_sales_matrix<0),as.character(1:last_age_tbc)] +
      (diag(x=diff_stock_wo_sales_matrix[which(diff_stock_wo_sales_matrix<0),],nrow = length(which(diff_stock_wo_sales_matrix<0))) %*% 
         (diag(x=1/vapply(rowSums(adj_matrix_vint_stock[which(diff_stock_wo_sales_matrix<0),as.character(1:last_age_tbc),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(diff_stock_wo_sales_matrix<0))) %*% 
            adj_matrix_vint_stock[which(diff_stock_wo_sales_matrix<0),as.character(1:last_age_tbc)]))
    #For positive differences, estimated stock is lower. Attention: We are limited by the previous year stock. We use the number of scrapped vehicles and reduce the number of scrapped vehicles
    #We first check that the number of vehicles to re-integrate is not higher than the total scrapped. If yes, force to the total scrapped
    diff_stock_wo_sales_matrix[which(diff_stock_wo_sales_matrix>0)] <- vapply(which(diff_stock_wo_sales_matrix>0),function(x)ifelse(sum(matrix_scrap[x,])-diff_stock_wo_sales_matrix[x]<0,sum(matrix_scrap[x,]),diff_stock_wo_sales_matrix[x]),FUN.VALUE=0)
    #Then, we adjust the number of scrapped vehicles by considering same ratio of scrapped vehicles by age
    adj_matrix_vint_stock[which(diff_stock_wo_sales_matrix>0),as.character(1:last_age_tbc)] <- matrix_vint_stock[which(diff_stock_wo_sales_matrix>0),as.character(1:last_age_tbc)] +
      (diag(x=diff_stock_wo_sales_matrix[which(diff_stock_wo_sales_matrix>0),],nrow = length(which(diff_stock_wo_sales_matrix>0))) %*% 
         (diag(x=1/vapply(rowSums(matrix_scrap[which(diff_stock_wo_sales_matrix>0),as.character(1:last_age_tbc),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(diff_stock_wo_sales_matrix>0))) %*% 
            matrix_scrap[which(diff_stock_wo_sales_matrix>0),as.character(1:last_age_tbc)]))
    #Update the final matrices
    matrix_vint_stock <- adj_matrix_vint_stock
    matrix_scrap <- fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:last_age_tbc)] - cbind(matrix_vint_stock[,as.character(1:last_age_tbc)],0)
    dimnames(matrix_scrap) <- list(rownames(matrix_vint_stock),1:(last_age_tbc+1))
  }
  fleet$vint_stock[[as.character(year)]] <- trunc(matrix_vint_stock)
  fleet$vint_scrap[[as.character(year)]] <- trunc(matrix_scrap)
  #Update the current year on-road stock with actual values
  fleet$ldv_on_road_stock[rownames(matrix_vint_stock),as.character(year)] <- trunc(rowSums(matrix_vint_stock))
  return(fleet)
}
