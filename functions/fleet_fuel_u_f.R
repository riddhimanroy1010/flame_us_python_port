#' fleet_fuel_u_f
#' Function: Calculates on-road fuel use by LDVs
#' @export
fleet_fuel_u_f<-function (first_yr = NA,last_yr = NA){
  attribute_f("fleet_fuel_u_f")
  #Inputs files
  vh_techno <- get_input_f(input_name = "model_matching_technology")
  #Functions' Outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  #fleet_vkt
  fleet_vkt_f_res <- do.call(fun_res_f,list(fun_name="fleet_vkt_f"))
  fleet_vint_vkt <- fleet_vkt_f_res[["fleet_vint_vkt"]]
  #Creation output files
  #fuel_use is the data.frame with the fuel use per year, scenario and fuel
  dt_col <- c("Year","Age","Size","Technology","Fuel","Unit","Value")
  fleet_vint_fuel_use <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop for size
  for (size in c("Car", "Light truck")) {
    #Loop for technology
    for (techno in unique(vh_techno$Own)) {
      #Loop for fuel_type
      #model_year_list is the list of Model Years we should consider in the matrices
      model_year_list <- unique(subset(fleet_fc_dt, Size==size & Technology==techno)$Model_year)[order(unique(subset(fleet_fc_dt, Size==size & Technology==techno)$Model_year))]
      #Create matrix of vintaged vkt. Rows: Year. Cols: Model year. Unit: km. If model year is older than the minimum model year data, assume it is the oldest
      stock_vkt <- subset(fleet_vint_vkt,Size==size & Technology == techno & Year%in%c(first_yr:last_yr))
      stock_vkt[,"Model_year"] <- sapply(stock_vkt[,"Year"] - stock_vkt[,"Age"],function(x) ifelse(x < min(model_year_list),min(model_year_list),x))
      #tmp_mat_vkt is a temporary matrix. Dimensions may not correspond to final format
      tmp_mat_vkt <- acast(stock_vkt, Year ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
      #matrix_vkt contains the vkt by year and model year in final format
      matrix_vkt <- matrix(0,ncol = length(model_year_list),nrow = length(first_yr:last_yr),dimnames = list(first_yr:last_yr,model_year_list))
      matrix_vkt[rownames(tmp_mat_vkt),colnames(tmp_mat_vkt)] <- tmp_mat_vkt
      #Fuel types per technology
      fuel_l <- unlist(strsplit(vh_techno$`Fuel type`[which(vh_techno$Own == techno)][1], ";"))
      for (fuel_type in fuel_l) {
        fuel_unit <- subset(vh_techno,`Fuel type`==fuel_type)[1,'Fuel unit']
        #Create matrix of fuel Consumption. Diagonal matrix. Unit: fuel unit / 100 km
        fc_dt <- subset(fleet_fc_dt, Size==size & Technology==techno & Fuel==fuel_type)
        matrix_fc <- diag(x=fc_dt$Value[order(fc_dt$Model_year)],nrow=length(fc_dt$Model_year),ncol=length(fc_dt$Model_year))
        dimnames(matrix_fc) <- list(fc_dt$Model_year,fc_dt$Model_year)
        #Create matrix of VKT share. Diagonal matrix with the share of vkt on fuel_type by model year
        vkt_share <- subset(fleet_uf_dt,Size==size & Technology==techno & Fuel==fuel_type & Model_year%in%model_year_list)
        matrix_vkt_share <- diag(x=vkt_share$Value[order(vkt_share$Model_year)],nrow=length(vkt_share$Model_year),ncol=length(vkt_share$Model_year))
        dimnames(matrix_vkt_share) <- list(vkt_share$Model_year,vkt_share$Model_year)
        #Calculate the fuel use by technology
        matrix_fuel_use <- (matrix_vkt %*% matrix_vkt_share) %*% matrix_fc/100
        #Convert matrix into long table
        tmp_fuel_use_dt <- as.data.frame(matrix_fuel_use) %>% 
          cbind(Year=as.numeric(rownames(matrix_fuel_use)),stringsAsFactors = FALSE) %>% 
          gather("Model_year","Value",-Year,convert=TRUE) %>% 
          cbind(Technology=techno,Size=size,Fuel=fuel_type,Unit=fuel_unit,stringsAsFactors = FALSE)
        tmp_fuel_use_dt[,"Age"] <- tmp_fuel_use_dt[,"Year"] - tmp_fuel_use_dt[,"Model_year"]
        #Combine
        fleet_vint_fuel_use <- rbind(fleet_vint_fuel_use,subset(tmp_fuel_use_dt,Age %in% c(0:30),-Model_year))
      }
    }
  }
  fleet_fuel_use_tot <- aggregate(data = fleet_vint_fuel_use,Value ~ Fuel + Year + Unit,FUN=sum)
  results<-list(fleet_fuel_use_tot=fleet_fuel_use_tot,
                fleet_vint_fuel_use=fleet_vint_fuel_use)
  return(results)
}
