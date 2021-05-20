#' vehicle_hist_material_composition_f
#' Function: Returns historical material composition of a specified vehicle technology and size, and save the information in the associated vehicleClass object.
#' @importFrom reshape2 acast
#' @export
vehicle_hist_material_composition_f = function(vehicle,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_hist_material_composition_f")
  #Other parameters
  age_tbc = 30
  first_hist_yr = first_yr-age_tbc
  last_hist_yr <- max(as.numeric(colnames(vehicle$fuel_consumption)[!is.na(vehicle$fuel_consumption[1,])]))
  #Inputs
  material_dt <- get_input_f(input_name = "model_matching_material")
  hist_mc  <- get_input_f(input_name = 'fleet_mt_comp_hist')
  #Create matrix of material composition in the fields
  vehicle$material_composition <- matrix(NA,nrow=length(unique(material_dt$Own)),ncol=last_yr-first_hist_yr+1,dimnames=list(unique(material_dt$Own),first_hist_yr:last_yr))
  #Update historical material composition
  mat_hist_mc <- acast(data=subset(hist_mc, Technology==vehicle$technology & Size==vehicle$size & Model_year>=first_hist_yr & Material!="Total"), Material ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  vehicle$material_composition[rownames(mat_hist_mc),colnames(mat_hist_mc)] <- mat_hist_mc
  #Functions: Calculate material composition by subcomponent
  fleet_i_mat_cont_f_res <- do.call(fun_res_f,list(fun_name="fleet_i_mat_cont_f"))
  veh_mat_cont <- subset(fleet_i_mat_cont_f_res[["fleet_mat_cont"]],Size==vehicle$size & Technology==vehicle$technology,select=-c(Size,Technology))
  #Assume material composition from component distribution replace historical material composition from first_yr, to last_hist_yr
  for (year in first_yr:last_hist_yr){
    #Update material composition in the initial year
    mat_i_mc <- acast(data=veh_mat_cont, Material ~ "Total" , value.var='Weight',fun.aggregate=sum, margins=FALSE)
    vehicle$material_composition[rownames(mat_i_mc),as.character(year)] <- mat_i_mc
    #update material composition by subcomponent in the initial year
    vehicle$material_component_composition <- rbind(vehicle$material_component_composition,cbind(veh_mat_cont,"Model_year"=year))
  }
  return(vehicle)
}
