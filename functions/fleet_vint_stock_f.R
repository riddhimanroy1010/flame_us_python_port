#' fleet_vint_stock_f
#' Function: Calculates the vintaged stock of the U.S. LDVs by vehicle technology, size and model year.
#' @importFrom reshape2 acast
#' @export
fleet_vint_stock_f <- function(first_yr=NA,last_yr=NA,ldv_stock_proj=NA,aeo_scen=NA){
  attribute_f("fleet_vint_stock_f")
  #Initialize
  fleet <- do.call(fleet_initialize_f,list())
  #Create historical vintaged stock
  fleet <- do.call(fleet_vint_stock_initialization_f,list(fleet=fleet))
  first_proj_yr <- 2021
  #Projected vintaged stock
  if (ldv_stock_proj=="aeo"){
    #Update stock with projected data
    proj_stock_aeo  <- get_input_f(input_name = 'fleet_stock_proj_aeo')
    matrix_stock_proj <- acast(data=subset(proj_stock_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    fleet$ldv_on_road_stock[rownames(matrix_stock_proj),colnames(matrix_stock_proj)] <- matrix_stock_proj
    fleet$ldv_on_road_stock_tot[,colnames(matrix_stock_proj)] <- colSums(matrix_stock_proj)
    #Update technology market share
    fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
    fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
    #Update sales
    proj_sales_aeo  <- get_input_f(input_name = 'fleet_sales_proj_aeo')
    tot_sales_aeo <- aggregate(formula=Value ~ Year,data=subset(proj_sales_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr),FUN=sum)
    matrix_tot_sales <- diag(x=tot_sales_aeo$Value,nrow=nrow(tot_sales_aeo),ncol=nrow(tot_sales_aeo))
    dimnames(matrix_tot_sales) <- list(tot_sales_aeo$Year,tot_sales_aeo$Year)
    fleet$ldv_sales[rownames(fleet$technology_market_share),colnames(matrix_tot_sales)] <- fleet$technology_market_share[,colnames(matrix_tot_sales)] %*% matrix_tot_sales
    for (year in first_proj_yr:last_yr){
      fleet <- do.call(fleet_vint_stock_update_with_sales_f,list(fleet=fleet,year=year))
    }
  } else if(ldv_stock_proj=="gcam"){
    #Update total projected stock
    fleet <- do.call(fleet_ldv_stock_update_gcam_f,list(fleet=fleet))
    #Update technology market share. Keep market share of AEO.
    fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
    fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
    for (year in first_proj_yr:last_yr){
      fleet <- do.call(fleet_vint_stock_update_with_stock_f,list(fleet=fleet,year=year))
    }
  } else if(ldv_stock_proj=="constant"){
    #Update total projected stock
    fleet <- do.call(fleet_ldv_stock_update_constant_f,list(fleet=fleet))
    #Update technology market share. Keep market share of AEO.
    fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
    fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
    for (year in first_proj_yr:last_yr){
      fleet <- do.call(fleet_vint_stock_update_with_stock_f,list(fleet=fleet,year=year))
    }
  }
  return(fleet$get_list_dataframe())
}
