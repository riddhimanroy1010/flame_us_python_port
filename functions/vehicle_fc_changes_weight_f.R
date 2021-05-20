#' vehicle_fc_changes_weight_f
#' Function: Calculate prospective fuel consumption values of LDVs from previous year fuel consumption value and changes in vehicle weight. Update the information in the vehicleClass object.
#' @export
vehicle_fc_changes_weight_f = function(vehicle,model_year){
  attribute_f("vehicle_fc_changes_weight_f")
  delta_wgt <- sum(vehicle$material_composition[,as.character(model_year)]-vehicle$material_composition[,as.character(model_year-1)])
  mat_frv <- do.call(vehicle_frv_f,list(vehicle,model_year))
  vehicle$fuel_consumption[,as.character(model_year)] <- vehicle$fuel_consumption[,as.character(model_year)]+delta_wgt*mat_frv[vehicle$fuel_type,]/100
  return(vehicle)
}
