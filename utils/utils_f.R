library(reshape2)
library(readxl)

#' attribute_f
#'
#' This function assigns in the parent environment the attribute of the specified function
#' @param fun_name Name of the function
#' @export
attribute_f<-function(fun_name){
  #Environmental setup
  ##If the environment which contains the functions' attributes does not exist, it creates it, and upload the attribute values
  if (!exists("fun.att.env",where = .GlobalEnv)){
    assign("fun.att.env",new.env(),envir = .GlobalEnv)
    #Upload attribute value
    assign("function_attributes",read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings = ""),envir = fun.att.env)
    assign("attribute_value",read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE,check.names = FALSE),envir = fun.att.env)
  }
  #Define parent environment as environment
  e=parent.frame()
  #attr_to_update is the list of attributes of function fun_name to update
  attr_to_update <- unique(subset(get("function_attributes", envir = fun.att.env),Function==fun_name & Type%in%c("External","Scenario"))$Attribute)
  #Check if arguments, Otherwise nothing
  if (length(attr_to_update)!=0){
    #Loop on the arguments not assigned in the parent environment
    for (args in attr_to_update[sapply(attr_to_update,function(x)is.na(get0(x,envir = e,ifnotfound = NA)),USE.NAMES = FALSE)]){
      #If argument is numeric. Convert in numeric
      assign(args,ifelse(subset(get("attribute_value", envir = fun.att.env),Attribute==args)$Type=="num",as.numeric(subset(get("attribute_value", envir = fun.att.env),Attribute==args)$Value),subset(get("attribute_value", envir = fun.att.env),Attribute==args)$Value), envir = e)
    }
  }
}
#' fun_res_f
#'
#' Return the results of function. First check if function results are saved in local or saved environment.
#' @param fun_name Function name
#' @export
fun_res_f <- function(fun_name){
  #Environmental setup
  ##If the environment which contains the functions' results does not exist, it creates it.
  if (!exists("fun.res.env",where = .GlobalEnv)){
    assign("fun.res.env",new.env(),envir = .GlobalEnv)
  }
  #Check if the results of the function are not alread in fun.res.env
  if (exists(paste0(fun_name,"_res"),where = fun.res.env)){
    #Get the results of the function from fun.res.env
    fun_res <- get(paste0(fun_name,"_res"), envir = fun.res.env)
  } else {
    #Read the function, generate the results and assign it in fun.res.env
    fun_res <- do.call(get(fun_name),list())
    assign(paste0(fun_name,"_res"),fun_res,envir = fun.res.env)
  }
  return(fun_res)
}

#' fun_res_with_attributes
#'
#' Returns the functions results with the value of the specified parameters. Delete in all environments the depend functions
#' @param fun_name Function name
#' @param list_attributes List of attributes with values
#' @param default Shall it generate the simulation results with default values?
#' @export
fun_res_with_attributes <- function(fun_name,
                                    list_attributes,
                                    default="n"){
  if (default=="n"){
    #Get list of dependent functions to the attributes
    list_dpt_fct <- get_dependent_functions(names(list_attributes))
    del_fun_res(fct_list=list_dpt_fct)
    #Update attribute values to specified
    update_attribute_values(attributes_to_update=list_attributes)
  } else if (default=="y"){
    #Reset attribute values
    reset_attribute_values()
    #Delete all previously saved results
    suppressWarnings(remove(list="fun.res.env",envir = .GlobalEnv))
  }
  return(do.call(fun_res_f,list(fun_name=fun_name)))
}

#' del_fun_res
#'
#' Delete the functions results in the local and saved environment
#' @param fct_list List of function results to delete. If all delete the environment fun.res.env
#' @export
del_fun_res <- function(fct_list){
  if("all"%in%fct_list){
    if (exists(x="fun.res.env",envir = .GlobalEnv)){
      remove(list="fun.res.env",envir = .GlobalEnv)
    }
  } else {
    if (exists(x="fun.res.env",envir = .GlobalEnv)){
      suppressWarnings(remove(list=paste0(fct_list,"_res"),envir = fun.res.env))
    }
    suppressWarnings(remove(list=paste0(fct_list,"_res"),envir = globalenv()))
  }
}

#' get_dependent_functions
#'
#' Return the list of functions that depend on the list of attributes specified.
#' @param attribute_list List of attributes
#' @export
get_dependent_functions <- function(attribute_list){
  #Get functions affected by the attributes to consider
  matrix_attributes <- as.matrix(read.csv("architecture/function_attributes_matrix.csv",stringsAsFactors = FALSE, check.names = FALSE,row.names = 1))
  matrix_sources <- as.matrix(read.csv("architecture/function_sources_matrix.csv",stringsAsFactors = FALSE, check.names = FALSE,row.names = 1))
  #matrix_depend_sources show the depedencies of the matrix functions. Rows are the functions and columns are the dependent inputs
  matrix_depend_sources <- solve(diag(ncol(matrix_sources))-t(matrix_sources))
  #Issue after calculations: negative values
  matrix_depend_sources[matrix_depend_sources<0] <- 0
  #matrix_depend_input shows the dependancis of the matrix functions and attributes. Rows are the functions and columns are the cumulative used of the attributes
  matrix_depend_input <- matrix_depend_sources[rownames(matrix_attributes),rownames(matrix_attributes)] %*% matrix_attributes
  #list_depend_fct contains the list of all the functions that are influenced by the edit attributes. Should be removed from the saved environment
  list_depend_fct <- unique(unlist(lapply(attribute_list,function(x)rownames(matrix_depend_input)[which(matrix_depend_input[,x]!=0)])))
  return(list_depend_fct)
}

#' get_dependent_attributes
#'
#' Return all the attributes that have an influence on the specified function
#' @param function_tbc List of functions
#' @export
get_dependent_attributes <- function(function_tbc){
  #
  function_attributes<-read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings = "")
  matrix_sources <- read.csv("architecture/function_sources_matrix.csv",stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  #inv_matrix_sources show the depedencies of the matrix functions. Columns are the functions and rows are the dependent inputs
  inv_matrix_sources <- t(solve(diag(ncol(matrix_sources))-t(matrix_sources)))
  #Issue after calculations: negative values
  inv_matrix_sources[inv_matrix_sources<0] <- 0
  #source_list contains the list of sources that are directly or indirectly used by function_tbc
  source_list <- rownames(inv_matrix_sources)[which(inv_matrix_sources[,function_tbc]!=0)]
  #param_tbc is the list of attributes of the source_list functions
  param_tbc <- unique(subset(function_attributes,Function%in%source_list & Type=="External")$Attribute)
  return(param_tbc)
}

#' get_scenario_attributes
#'
#' Returns the names list of attributes of the scen_tbc
#' @param scen_tbc Scenario
#' @import readxl
#' @export
get_scenario_attributes <- function(outputs_path,scen_tbc){
  scenarios <- as.data.frame(read_xlsx(paste0(outputs_path,"/scenarios.xlsx"),sheet="scenarios",.name_repair = "minimal"))
  #Fill empty spaces with NA
  scenarios[scenarios==""] <- NA
  #Delete rows not associated with sim_tbc
  scen_attribute_value <- subset(scenarios, Scenario==scen_tbc)
  #Delete columns with NA
  scen_attribute_value <- scen_attribute_value[,!is.na(scen_attribute_value)[1,]]
  scen_names <- setdiff(as.character(subset(scen_attribute_value,Attribute=="name")),c(scen_tbc,"name","par"))
  attribute_list <- subset(scen_attribute_value,Type!="par")$Attribute
  matrix_attributes <- matrix(sapply(scen_names,function(x)subset(scen_attribute_value,Attribute!="name")[,subset(scen_attribute_value,Attribute=="name")==x]),
                              nrow=length(attribute_list),
                              dimnames = list(attribute_list,scen_names))
  return(matrix_attributes)
}

#' get_discrete_simulation_attributes
#'
#' Returns the names list of attributes of the sim_tbc
#' @param sim_tbc Simulation
#' @import readxl
#' @export
get_discrete_simulation_attributes <- function(outputs_path,sim_tbc){
  simulations <- as.data.frame(read_xlsx(paste0(outputs_path,"/simulations.xlsx"),sheet = "discrete",.name_repair = "minimal"))
  #Fill empty spaces with NA
  simulations[simulations==""] <- NA
  #Delete rows not associated with sim_tbc
  sim_attribute_value <- subset(simulations, Simulation==sim_tbc)
  #Delete columns with NA
  sim_attribute_value <- sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  sim_names <- setdiff(as.character(subset(sim_attribute_value,Attribute=="name")),c(sim_tbc,"name","par"))
  attribute_list <- subset(sim_attribute_value,Type!="par")$Attribute
  matrix_attributes <- matrix(sapply(sim_names,function(x)subset(sim_attribute_value,Attribute!="name")[,subset(sim_attribute_value,Attribute=="name")==x]),
                              nrow=length(attribute_list),
                              dimnames = list(attribute_list,sim_names))
  return(matrix_attributes)
}

#' get_continuous_simulation_attributes
#'
#' Returns the names list of attributes of the sim_tbc
#' @param sim_tbc Simulation
#' @import readxl
#' @export
get_continuous_simulation_attributes <- function(outputs_path,sim_tbc){
  simulations <- as.data.frame(read_excel(paste0(outputs_path,"/simulations.xlsx"),sheet = "continuous",.name_repair = "minimal"))
  #Fill empty spaces with NA
  simulations[simulations==""] <- NA
  #Delete rows not associated with sim_tbc
  sim_attribute_value <- subset(simulations, Simulation==sim_tbc)
  #Delete columns with NA
  sim_attribute_value <- sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  #Get attribute name with continuous values
  continuous_attribute <- subset(sim_attribute_value,Type=="range")$Attribute
  #Get list of all atribtues to change
  attribute_list <- subset(sim_attribute_value,Type!="par")$Attribute
  #Get name of the simulations
  sim_names <- setdiff(as.character(subset(sim_attribute_value,Attribute=="name")),c(sim_tbc,"name","par"))
  #Create a first matrix with all the attributes by simulation name
  tmp_matrix_attributes <- matrix(sapply(sim_names,function(x)subset(sim_attribute_value,Attribute!="name")[,subset(sim_attribute_value,Attribute=="name")==x]),
                              nrow=length(attribute_list),
                              dimnames = list(attribute_list,sim_names))
  matrix_attributes <- NULL
  for (sim_name in sim_names){
    seq_par <- sapply(continuous_attribute,function(x)sapply(c("from","to","by"),function(y)as.numeric(substring(grep(y,unlist(strsplit(subset(sim_attribute_value,Attribute==x)[,subset(sim_attribute_value,Attribute=="name")==sim_name],";")),value=TRUE),nchar(y)+2,100))))
    cont_attr_values <- setNames(lapply(colnames(seq_par),function(x)seq(from=seq_par["from",x],to=seq_par["to",x],by=seq_par["by",x])),continuous_attribute)
    #Create matrix with all values
    matrix <- matrix(NA, nrow=nrow(tmp_matrix_attributes),ncol=nrow(expand.grid(cont_attr_values)),dimnames = list(rownames(tmp_matrix_attributes),replicate(nrow(expand.grid(cont_attr_values)),sim_name)))
    matrix[continuous_attribute,] <- t(expand.grid(cont_attr_values))
    matrix[!rownames(matrix)%in%continuous_attribute,] <- tmp_matrix_attributes[!rownames(matrix)%in%continuous_attribute,sim_name]
    #Combined in final matrix
    matrix_attributes <- cbind(matrix_attributes,matrix)
  }
  return(matrix_attributes)
}

#' get_simulation_attribute_to_keep
#'
#' Returns the list of attributes to show in the resuls of the simulations
#' @param sim_tbc Simulation
#' @import readxl
#' @param sim_type Type of simulation
#' @export
get_simulation_attribute_to_keep <- function(outputs_path,sim_tbc,sim_type="discrete"){
  simulations <- as.data.frame(read_excel(paste0(outputs_path,"/simulations.xlsx"),sheet = sim_type,.name_repair = "minimal"))
  #Fill empty spaces with NA
  simulations[simulations==""] <- NA
  #Delete rows not associated with sim_tbc
  sim_attribute_value <- subset(simulations, Simulation==sim_tbc)
  #Delete columns with NA
  sim_attribute_value <- sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  attr_to_keep <- subset(sim_attribute_value, Type %in% c("attr_tk","discrete_tk","range"))$Attribute
  return(attr_to_keep)
}

#' get_sens_analysis_attributes
#'
#' Returns the names list of attributes of the sens_tbc
#' @param sens_tbc Sensitivity analyses
#' @param function_tbc Function
#' @import readxl
#' @export
get_sens_analysis_attributes <- function(outputs_path,sens_tbc,function_tbc){
  if (sens_tbc!="complete"){
    sens_analysis <- as.data.frame(read_excel(paste0(outputs_path,"/sens_analysis.xlsx"),sheet = "single_factor",.name_repair = "minimal"))
    param_tbc <- unlist(strsplit(sens_analysis[sens_analysis$Sensitivity==sens_tbc,"Attributes"],split=";"))
  } else if (sens_tbc=="complete"){
    param_tbc <- get_dependent_attributes(function_tbc=function_tbc)
  }
  return(param_tbc)
}

#' get_mf_sens_analysis_attributes
#'
#' Returns the names list of attributes of the mf_sens_tbc
#' @param mf_sens_tbc Multi-factor sensitivity analyses
#' @import readxl
#' @export
get_mf_sens_analysis_attributes <- function(outputs_path,mf_sens_tbc){
    sens_analysis <- as.data.frame(read_excel(paste0(outputs_path,"/sens_analysis.xlsx"),sheet = "multi_factor",.name_repair = "minimal"))
    #Get attributes
    mf_s_a_attributes <- list(function_to_eval=subset(sens_analysis,Name==mf_sens_tbc)$Fct_to_eval,
                              subfunction_to_eval=subset(sens_analysis,Name==mf_sens_tbc)$Subfct_to_eval,
                              dts_name=subset(sens_analysis,Name==mf_sens_tbc)$Dts_name,
                              subset_conditions_l=strsplit(subset(sens_analysis,Name==mf_sens_tbc)$Conditions,";")[[1]],
                              variable_value_name=subset(sens_analysis,Name==mf_sens_tbc)$Value_name,
                              variable_to_aggregate=strsplit(subset(sens_analysis,Name==mf_sens_tbc)$Aggregate,";")[[1]],
                              cumulative=subset(sens_analysis,Name==mf_sens_tbc)$Cumulative)

  return(mf_s_a_attributes)
}

#' load_attribute_value
#'
#' Load attribute value .csv file in the fun.att.env
#' @export
load_attribute_value <- function(){
  if (!exists("fun.att.env",where = .GlobalEnv)){
    assign("fun.att.env",new.env(),envir = .GlobalEnv)
  }
    #Upload attribute value
    assign("function_attributes",read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings = ""),envir = fun.att.env)
    assign("attribute_value",read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE,check.names = FALSE),envir = fun.att.env)
}

#' reset_attribute_value_csv
#'
#' Reset the list of attribute values with default values in the .csv file
#' @export
reset_attribute_value_csv <- function(){
  attribute_value <- read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
}

#' reset_attribute_values
#'
#' Reset the list of attribute values with default values in the attribute environment
#' @export
reset_attribute_values <- function(){
  attribute_value <- get("attribute_value", envir = fun.att.env)
  attribute_value$Value <- attribute_value$Default
  assign("attribute_value",attribute_value,envir = fun.att.env)
}

#' update_attribute_values
#'
#' Update the list of attribute values from list
#' @param attributes_to_update List of attributes with values
#' @export
update_attribute_values <- function(attributes_to_update){
  attribute_value <- get("attribute_value", envir = fun.att.env)
  if (is.matrix(attributes_to_update)){
    for (attr in rownames(attributes_to_update)) {
      attribute_value[attribute_value$Attribute == attr, "Value"] <- attributes_to_update[attr,1]
    }
  } else if(any(class(attributes_to_update)%in%c("character","list"))) {
    for (attr in names(attributes_to_update)) {
      attribute_value[attribute_value$Attribute == attr, "Value"] <- attributes_to_update[[attr]]
    }
  }
  assign("attribute_value",attribute_value,envir = fun.att.env)
}

#' get_default_value
#'
#' Get the default value of the attribute
#' @param attribute Attribute
#' @export
get_default_value <- function(attribute){
  return(subset(get("attribute_value", envir = fun.att.env),Attribute==attribute)[,"Default"])
}

#' get_attribute_value
#'
#' Get the current value of the attribute
#' @param attribute Attribute
#' @export
get_attribute_value <- function(attribute){
  return(subset(get("attribute_value", envir = fun.att.env),Attribute==attribute)[,"Value"])
}

#' get_all_attribute_value
#'
#' Get all potential values of the attribute except the default value if get_default_attr_val==n
#' @param attribute Attribute
#' @param get_default_attr_val Shall it get the default value?
#' @export
get_all_attribute_value <- function(attribute,get_default_attr_val="n"){
  attribute_value <- get("attribute_value", envir = fun.att.env)
  if (get_default_attr_val=="n"){
    def_par <- subset(attribute_value,Attribute==attribute)$Default
    par_value_l <- setdiff(unlist(strsplit(subset(attribute_value,Attribute==attribute)$All,split=";")),def_par)
  } else{
    par_value_l <- unlist(strsplit(subset(attribute_value,Attribute==attribute)$All,split=";"))
  }
  return(par_value_l)
}

#' get_attribute_name
#'
#' Get the name of the attribute
#' @param attribute Attribute
#' @export
get_attribute_name <- function(attribute){
  return(subset(get("attribute_value", envir = fun.att.env),Attribute==attribute)[,"Name"])
}

#' read_input_f
#'
#' Read a table and return a dataframe
#' @import readxl
#' @export
read_input_f <- function(file_format,file_path,sheet_name=NA,rownames){
  if (file_format==".csv"){
    input <- read.csv(file=file_path,header=TRUE,stringsAsFactors = FALSE, check.names = FALSE, row.names = rownames)
  } else if (file_format==".xlsx"){
    input <- as.data.frame(read_xlsx(path=file_path,sheet=sheet_name),stringsAsFactors = FALSE)
  }
  return(input)
}

#' save_input_data_f
#'
#' Read and save all input data in a R format
#' @export
save_input_data_f <- function(){
  #Delete data input environment
  if (exists(x="fun.datainput.env",envir = .GlobalEnv)){
    remove(list="fun.datainput.env",envir = .GlobalEnv)
  }
  #Create an empty environment
  assign("fun.datainput.env",new.env(),envir = .GlobalEnv)
  #Population the environment
  data_input_mngt <- read.csv("inputs/data_input_management.csv",stringsAsFactors = FALSE)
  #Read and assign all inputs
  for (i in 1:nrow(data_input_mngt)){
    if (is.na(data_input_mngt[i,"Rownames"])){
      tmp_rownames <- NULL
    } else {
      tmp_rownames <- as.numeric(data_input_mngt[i,"Rownames"])
    }
    assign(data_input_mngt[i,"Variable_name"],read_input_f(file_format = data_input_mngt[i,"File_format"],file_path = data_input_mngt[i,"File"], sheet_name = data_input_mngt[i,"Sheet_name"], rownames = tmp_rownames),envir = fun.datainput.env)
  }
  #Save input data in
  save(list = ls(all.names = TRUE,fun.datainput.env), file = "inputs/input_data_environment.RData", envir = fun.datainput.env)
  #Clean environment
  remove(list="fun.datainput.env",envir = .GlobalEnv)
}

#' load_input_data_f
#'
#' Load all input data in a new environment
#' @export
load_input_data_f <- function(){
  #Delete data input environment
  if (exists(x="fun.datainput.env",envir = .GlobalEnv)){
    remove(list="fun.datainput.env",envir = .GlobalEnv)
  }
  #Create an empty environment
  assign("fun.datainput.env",new.env(),envir = .GlobalEnv)
  load("inputs/input_data_environment.RData",envir = fun.datainput.env)
}

#' get_input_f
#'
#' Get input input from the data environment
#' @export
get_input_f <- function(input_name){
  return(get(input_name, envir = fun.datainput.env))
}

