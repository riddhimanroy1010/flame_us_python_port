#Script that runs all scripts in the model
#Update all library
library(readxl)
library(reshape2)
library(tidyr)
library(stringr)

#Load model framework functions
f_list <- list.files(path = "utils", pattern = "_f.R",full.names = TRUE)
for (i in 1:length(f_list)) {
  source(f_list[[i]])
}
remove(list=c("i","f_list"))
#

#Compile all functions
source("functions/class.R")
f_list <- list.files(path = "functions", pattern = "_f.R",full.names = TRUE)
for (i in 1:length(f_list)) {
  source(f_list[[i]])
}
remove(list=c("i","f_list"))
#