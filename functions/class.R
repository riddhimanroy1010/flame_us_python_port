#' vehicleClass
#' @import tidyr
#' @import modelframework
#' @export
vehicleClass <- setRefClass("vehicleClass",
                             fields = list(technology="character",
                                           size="character",
                                           fuel_type="character",
                                           fuel_consumption="matrix",
                                           utility_factor="matrix",
                                           specifications="matrix",
                                           battery_type="character",
                                           material_composition="matrix",
                                           material_component_composition="data.frame"),
                             methods = list(
                               ###>Function: Initialize the object
                               initialize = function(technology=as.character(),size=as.character(),...){
                                 #Inputs
                                 vh_techno <- get_input_f(input_name = "model_matching_technology")
                                 #Update fields
                                 .self$technology <<- technology
                                 .self$size <<- size
                                 .self$fuel_type <<- unlist(strsplit(subset(vh_techno,Own==technology)[,"Fuel type"],";"))
                               },
                               ###>Function: Return the fields of the vehicle as data.frame
                               get_data_frame = function(field_name){
                                 field_values <- .self$field(field_name)
                                 if (is.matrix(field_values)){
                                   #Convert matrix into long dataframe
                                   field_dt <- as.data.frame(field_values,stringsAsFactors = FALSE) %>% 
                                     cbind(Data=rownames(field_values),stringsAsFactors = FALSE) %>% 
                                     gather("Model_year","Value",-Data,convert=TRUE) %>%
                                     cbind(Technology=.self$technology,Size=.self$size,stringsAsFactors = FALSE) %>%
                                     subset(.,select=c(Technology,Size,Model_year,Data,Value))
                                   #Update colnames
                                   colnames(field_dt)[colnames(field_dt)=="Data"] <- switch(field_name,
                                                                                            "fuel_consumption"="Fuel",
                                                                                            "utility_factor"="Fuel",
                                                                                            "specifications"="Attribute",
                                                                                            "material_composition"="Material")
                                 } else if(class(field_values)=="data.frame"){
                                   field_dt <- field_values %>%
                                     cbind(Technology=.self$technology,Size=.self$size,stringsAsFactors = FALSE)
                                 }
                                 return(field_dt)
                               }
                             ))

#' fleetClass
#' @import tidyr
#' @export
fleetClass <- setRefClass("fleetClass",
                             fields = list(vint_stock="list",
                                           vint_scrap="list",
                                           technology_market_share="matrix",
                                           ldv_sales="matrix",
                                           ldv_on_road_stock="matrix",
                                           ldv_on_road_stock_tot="matrix"),
                             methods = list(
                               ###>Function: Initialize the object
                               initialize = function(){
                               },
                               ###>Function: Return the fields of the vehicle as data.frame
                               get_data_frame = function(field_name){
                                 field_values <- .self$field(field_name)
                                 if (is.matrix(field_values)){
                                   #Convert matrix into long dataframe
                                   field_dt <- as.data.frame(field_values,stringsAsFactors = FALSE) %>% 
                                     cbind(Type=rownames(field_values),stringsAsFactors = FALSE) %>% 
                                     gather("Year","Value",-Type,convert=TRUE)
                                   field_dt[,"Size"] <- substr(field_dt$Type,0,as.numeric(regexpr(pattern="_",field_dt$Type))-1)
                                   field_dt[,"Technology"] <- substr(field_dt$Type,as.numeric(regexpr(pattern="_",field_dt$Type))+1,200)
                                   field_dt[,"Type"] <- NULL
                                 } else if(class(field_values)=="list"){
                                   for (i in seq_len(length(field_values))){
                                     tmp_stock_dt <- as.data.frame(field_values[[i]]) %>% 
                                       cbind(Type=rownames(field_values[[i]]),stringsAsFactors = FALSE) %>% 
                                       gather("Age","Value",-Type,convert=TRUE) %>%
                                       cbind(Year=as.numeric(names(field_values)[i]))
                                     tmp_stock_dt[,"Size"] <- substr(tmp_stock_dt$Type,0,as.numeric(regexpr(pattern="_",tmp_stock_dt$Type))-1)
                                     tmp_stock_dt[,"Technology"] <- substr(tmp_stock_dt$Type,as.numeric(regexpr(pattern="_",tmp_stock_dt$Type))+1,200)
                                     tmp_stock_dt[,"Type"] <- NULL
                                     #Update output 
                                     field_dt <- rbind(get0("field_dt"),tmp_stock_dt)
                                   }
                                 }
                                 return(field_dt)
                               },
                               ###Function: Return all the fields into list of dataframe
                               get_list_dataframe = function(){
                                 return(list(fleet_vint_stock=.self$get_data_frame("vint_stock"),fleet_vint_scrap=.self$get_data_frame("vint_scrap")))
                               }
                             ))
