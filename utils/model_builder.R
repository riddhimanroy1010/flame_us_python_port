library(reshape2)

#Creates a .csv file with all the model's functions (scripts finishing with _f.R in the function folder), all their attributes (exogenous and endogenous) and the default values taken from the function's script
remove(list=ls(envir=globalenv()),envir=globalenv())
package_path = "functions"
#Create temporary environment
#List all the scripts including functions
f_list <- list.files(path=package_path,pattern = "_f.R",full.names = TRUE)
#Run the functions
for (i in 1:length(f_list)){
  #Run the script
  source(f_list[[i]],local = TRUE)
}
#Extract the function names
f_names<- as.vector(lsf.str(envir=globalenv()))
#Create output files
dt_col <- c("Function","Attribute","Value","Type")
f_dt <- setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Read csv file with list of internal variables
int_att_dt <- read.csv("architecture/list_of_internal_attributes.csv",stringsAsFactors = FALSE)
#int_att_list is the list of internal attributes
int_att_list <- unlist(strsplit(subset(int_att_dt,Attribute_type=="Internal")$Attribute_list,split=";"))
#scen_att_list contains the list of attributes that are scenario attribute (i.e., updated by the user but correspond to scenarios. Not used in sensitivity analyses)
scen_att_list <- unlist(strsplit(subset(int_att_dt,Attribute_type=="Scenario")$Attribute_list,split=";"))
#run_args contains the list of attributes that are computational attribute (i.e., only used for computational purposes. Not used in sensitivity analyses)
model_att_list <- unlist(strsplit(subset(int_att_dt,Attribute_type=="Model")$Attribute_list,split=";"))
#Loop on functions
for (f in f_names){
  #Extract attributes of function "f"
  list<-as.vector(formals(f))
  if (length(list)!=0){
    #Rows of data frame
    rows<-nrow(f_dt)+1:length(list)
    #Fill function names, and argument names
    f_dt[rows,"Function"]<-f
    f_dt[rows,"Attribute"]<-names(list)
    #For each argument, exctract the default value ("" if no value)
    if (length(names(list))>0){
      for (l in 1:length(names(list))){
        f_dt[rows[l],"Value"]<-ifelse(is.symbol(list[[l]]),"",as.character(list[[l]]))
        if(names(list)[l]%in%int_att_list){
          f_dt[rows[l],"Type"]<-"Internal"
        } else if (names(list)[l]%in%scen_att_list){
          f_dt[rows[l],"Type"]<-"Scenario"
        } else if (names(list)[l]%in%model_att_list){
          f_dt[rows[l],"Type"]<-"Model"
        } else{
          f_dt[rows[l],"Type"]<-"External"
        }
      }
    }
  } else {
    f_dt[nrow(f_dt)+1,] <- c(f,NA,NA,NA)
  }
}
ex_f_dt <- data.frame(Function=unique(f_dt$Function),Attributes=sapply(unique(f_dt$Function),function(x)paste0(subset(f_dt,Function==x & Type=="External")$Attribute,collapse=";")))
#Write attribute function attribute values
write.csv(f_dt,"architecture/function_attributes.csv",row.names = FALSE,na="")
write.csv(ex_f_dt,"architecture/function_explicit_attributes.csv",row.names = FALSE,na="")

#Read attribute_value
attribute_value <- read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
#current_args is the list of attributes already in attribute_value
current_args <- attribute_value$Attribute
#ex_args_l is the list of attributes used in the functions
ex_args_l <- sort(unique(f_dt[f_dt$Type%in%c("External","Scenario"),"Attribute"]))
#attr_list is the union of current_args and ex_args_l
attr_list <- union(current_args,ex_args_l)
#Update attribute_value with default values
for (attr in attr_list){
  #If attribute is in attribute_value, update used value with default values
  if (attr %in% current_args){
    attribute_value[attribute_value$Attribute==attr,"Value"] <- attribute_value[attribute_value$Attribute==attr,"Default"]
  } else if (!attr %in% current_args){
    attribute_value[nrow(attribute_value)+1,"Attribute"] <- attr
  }
}
#Order attribute_value by attributes name
attribute_value <- attribute_value[order(attribute_value$Attribute),]
write.csv(attribute_value,"architecture/attribute_value.csv",row.names = FALSE)

#Construct an attribute matrix with etxernal attributes
data_for_matrix <- subset(f_dt,Type=="External")
data_for_matrix$Value <- 1
function_attribute_matrix <- matrix(0,nrow=length(unique(f_dt$Function)),ncol=length(unique(subset(f_dt,Type=="External")$Attribute)),dimnames = list(unique(f_dt$Function),unique(subset(f_dt,Type=="External")$Attribute)))
function_attribute_matrix[unique(subset(f_dt,Type=="External")$Function),unique(subset(f_dt,Type=="External")$Attribute)] <- acast(data=data_for_matrix, Function ~ Attribute , value.var='Value',fun.aggregate=sum, margins=FALSE)[unique(subset(f_dt,Type=="External")$Function),unique(subset(f_dt,Type=="External")$Attribute)]
write.csv(function_attribute_matrix,"architecture/function_attributes_matrix.csv",row.names = TRUE)
remove(list=ls(envir=globalenv()),envir=globalenv())

# Creates three .csv file with the model's functions inputs (external or internal to the model)
package_path = "functions"
script_list <- list.files(path=package_path,pattern = "_f.R",full.names = TRUE)
#Ouputs
dt_col<-c("Function","Input","Type")
input_dt<-setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
input_lists<-NULL
#search_cha_dt is the dataframw with characters to find in different lines
search_cha_dt <- data.frame(cha_to_find=c("read.csv","read_excel","list.files","source","fun_res_f","get_input_f","get_input_f","do.call"),
                            start_cha=c("read.csv","read_excel","list.files","function","fun_name","input_name =","input_name=","do.call"),
                            offset_num_start=c(2,2,2,2,2,2,3,1),
                            end_cha=c('.csv"','.xlsx"','full.names','.R"','_f"',')',')',"_f,"),
                            offset_num_end=c(2,2,2,2,2,3,3,2),
                            type=c('.csv','.xlsx','List of files','.R',".R","Input","Input",".R"),
                            stringsAsFactors = FALSE)
#f_name_list is the list of script names
f_name_list <- substring(script_list,as.numeric(regexpr(pattern=package_path,script_list))+nchar(package_path)+1,as.numeric(regexpr(pattern="f.R",script_list)))
for (i in 1:length(script_list)){
  #Function names
  f_name <- f_name_list[i]
  #Read .R file as file
  file<-file(script_list[i],open="r")
  #Convert into lines
  text_file <- readLines(file)
  close(file)
  #input_check veryfies if inputs. If not, create empty line
  input_check=FALSE
  for (j in 1:nrow(search_cha_dt)){
    cha_ts1 <- search_cha_dt[j,"cha_to_find"]
    cha_ts2 <- search_cha_dt[j,"start_cha"]
    cha_ts3 <- search_cha_dt[j,"end_cha"]
    offset_num_end <- search_cha_dt[j,"offset_num_end"]
    offset_num_start <- search_cha_dt[j,"offset_num_start"]
    type <- search_cha_dt[j,"type"]
    tmp_lines <- text_file[grepl(cha_ts1,text_file)&grepl(cha_ts2,text_file)&grepl(cha_ts3,text_file)]
    if (length(tmp_lines)!=0){
      #Update input table with previous list
      for (k in 1:length(tmp_lines)){
        if (grepl("paste0",tmp_lines[k])){
          #Get input and update table
          tmp_input<-substring(tmp_lines[k], as.numeric(regexpr(pattern="paste0",tmp_lines[k]))+8,as.numeric(regexpr(pattern=cha_ts3,tmp_lines[k]))+nchar(cha_ts3)-offset_num_end)
          input_dt[nrow(input_dt)+1,]<-c(f_name,tmp_input,type)
        }else{
          #Get input and update table
          tmp_input<-substring(tmp_lines[k], as.numeric(regexpr(pattern=cha_ts2,tmp_lines[k]))+nchar(cha_ts2)+offset_num_start,as.numeric(regexpr(pattern=cha_ts3,tmp_lines[k]))+nchar(cha_ts3)-offset_num_end)
          input_dt[nrow(input_dt)+1,]<-c(f_name,tmp_input,type)
        }
        #Delete the line from the file
        text_file <- text_file[text_file!=tmp_lines[k]]
      }
      input_check=TRUE
    }
  }
  #If no input, empty line
  if (input_check==FALSE){
    input_dt[nrow(input_dt)+1,]<-c(f_name,NA,NA)
  }
}
#Format. Delete . after _f
input_dt$Input <- gsub("_f.R","_f",input_dt$Input)
input_dt <- subset(input_dt,Input!="fun_res_f")
input_lists<-sort(unique(input_dt$Input))
#Create matrix of scripts and inputs
#matrix_inputs represents the inputs in matrix format. Rows are the inputs and columns are the functions
matrix_inputs <- matrix(0,nrow = length(input_lists),ncol = length(f_name_list),dimnames = list(input_lists,f_name_list))
for (f_name in f_name_list){
  tmp_inpu_list <- subset(input_dt,Function==f_name)$Input
  if (!NA %in% tmp_inpu_list){
    for (input in tmp_inpu_list){
      matrix_inputs[input,f_name] <- 1
    }
  }
}
#matrix_sources represents the source inputs in matrix format. Rows are the source functions and columns are the functions
matrix_sources <- matrix(0,nrow = length(f_name_list),ncol = length(f_name_list),dimnames = list(f_name_list,f_name_list))
for (f_name in f_name_list){
  tmp_inpu_list <- subset(input_dt,Type==".R" & Function==f_name & !grepl("plots/",Input) & grepl("_f",Input))$Input
  if (!NA %in% tmp_inpu_list){
    for (input in tmp_inpu_list){
      matrix_sources[input,f_name] <- 1
    }
  }
}
write.csv(input_dt,"architecture/function_inputs.csv",row.names = FALSE,na="")
write.csv(matrix_inputs,"architecture/function_inputs_matrix.csv",row.names = TRUE,na="")
write.csv(matrix_sources,"architecture/function_sources_matrix.csv",row.names = TRUE,na="")
remove(list=ls(envir=globalenv()),envir=globalenv())

###>Script: Obtain the list of functions and their description. Write a .csv file.
fct_attributes <- read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE)
script_list <- list.files(path="functions",pattern = "_f.R",full.names = TRUE)
#Ouputs
dt_col<-c("Function","Type","Description")
description_dt<-setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
input_lists<-NULL
#search_cha_dt is the dataframw with characters to find in different lines
search_cha_dt <- data.frame(cha_to_find=c("read.csv","read_excel","list.files","source","fun_res_f"),
                            start_cha=c("read.csv","read_excel","list.files","function","fun_name"),
                            end_cha=c('.csv"','.xlsx"','full.names','.R"','_f"'),
                            type=c('.csv','.xlsx','List of files','.R',".R"),
                            stringsAsFactors = FALSE)
#f_name_list is the list of script names
f_name_list <- substring(script_list,as.numeric(regexpr(pattern="/",script_list))+1,as.numeric(regexpr(pattern=".R",script_list))-1)
for (i in 1:length(script_list)){
  #Function names
  f_name <- f_name_list[i]
  #Type of function
  type <- ifelse("Internal" %in% subset(fct_attributes,Function==f_name)$Type,"Internal","Result")
  #Read .R file as file
  file <- file(script_list[i],open="r")
  #Convert into lines
  text_file<-readLines(file)
  close(file)
  #input_check veryfies if inputs. If not, create empty line
  input_check=FALSE
  tmp_lines<-text_file[grepl("#' Function: ",text_file)]
  if (length(tmp_lines)!=0){
    tmp_desc <- substring(tmp_lines, 14,nchar(tmp_lines))
    description_dt[nrow(description_dt)+1,]<-c(f_name,type,tmp_desc)
  } else {
    description_dt[nrow(description_dt)+1,]<-c(f_name,type,NA)
  }
  
}
write.csv(description_dt,"architecture/function_descriptions.csv",row.names = FALSE,na="")
remove(list=ls(envir=globalenv()),envir=globalenv())

#Create the workflow chart of the model
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

fct_source_mat <- read.csv("architecture/function_sources_matrix.csv",stringsAsFactors = FALSE,row.names = 1)
first_part_str <- paste("digraph {"," ","graph[layout = dot, rankdir = LR]","",sep="\n")
second_part_str <- paste0(colnames(fct_source_mat),collapse = "\n")
third_part_str <- " "
for (column in colnames(fct_source_mat)){
  for (r in which(fct_source_mat[,column]==1)){
    third_part_str <- paste(third_part_str,paste0(rownames(fct_source_mat)[r]," -> ",column),sep="\n")
  }
}
final_str <- paste0(first_part_str,second_part_str,third_part_str,"\n}")
grViz(final_str) %>% export_svg %>% charToRaw %>% rsvg_png("architecture/flame_workflow_chart.png")
remove(list=ls(envir=globalenv()),envir=globalenv())