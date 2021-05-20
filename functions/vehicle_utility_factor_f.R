#' vehicle_utility_factor_f
#' Function: Returns utility factor for a fuel type of a vehicle technology and size. Relevant for plug-in hybrid electric vehicles.
#' @export
vehicle_utility_factor_f = function(vehicle,model_year){
  attribute_f("vehicle_utility_factor_f")
  #Inputs
  conv  <- get_input_f(input_name = 'conversion_units')
  #Function that calculate the vkt share from range
  uf_f<-function(range){
    #Range is in miles
    range_miles<-range*conv["mile","1 km"]
    uf=-7.73E-09*range_miles^4+2.63E-06*range_miles^3-3.7E-04*range_miles^2+2.66E-02*range_miles
    return(uf)
  }
  if (length(vehicle$fuel_type)==1){
    vehicle$utility_factor[,as.character(model_year)] <- 1
  } else {
    range <- vehicle$specifications["range",as.character(model_year)]
    for (fuel in vehicle$fuel_type){
      vehicle$utility_factor["Electricity",as.character(model_year)] <- uf_f(range=range)
      vehicle$utility_factor[setdiff(vehicle$fuel_type,"Electricity"),as.character(model_year)] <- 1-uf_f(range=range)
    }
  }
  return(vehicle)
}
