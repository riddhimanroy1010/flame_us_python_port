###>Function: Creates the historical material composition of all light duty vehicles
library(readxl)
#Assign arguments default values
source("architecture/attribute_f.R",local = TRUE)
attribute_f("fleet_mc_hist_f")
#Sources
source("functions/fleet_i_mat_cont_f.R",local=TRUE)
source("functions/fleet_i_comp_wgt_f.R",local=TRUE)
#Input files
mat_content <- read.csv("inputs/user/tedb_mat_comp_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
material <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "material"))
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
wgt_dt <- read.csv("inputs/model/epa_fleet_wgt_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
alu_hist <- read.csv("inputs/user/ducker_alu_content_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
conv <- read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
#Other inputs
fleet_i_comp_wgt_f_res <- do.call(fleet_i_comp_wgt_f,list())
fleet_compo_wgt <- fleet_i_comp_wgt_f_res[["fleet_compo_wgt"]]
fleet_i_mat_cont_f_res <- do.call(fleet_i_mat_cont_f,list())
fleet_i_mc <- fleet_i_mat_cont_f_res[["fleet_i_mc"]]
#Creation output file
first_yr <- min(wgt_dt$Model_year)
last_yr <- max(wgt_dt$Model_year)
tedb_data_year_list <- as.numeric(colnames(mat_content)[grepl(pattern="[[:digit:]]{1}",colnames(mat_content))])
dt_col <- c("Model_year","Size","Technology","Material","Value")
fleet_mt_comp <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (y in first_yr:last_yr) {
  for (size in c("Car","Light truck")){
    for (techno in unique(vh_techno$Own)){
      if (techno %in% c("ICEV-G","ICEV-D","FFV")){
        #First get vehicle curb weight
        #IF EPA data available, get EPA curb weight.
        if (nrow(subset(wgt_dt,Technology==techno & Size==size & Source=="epa" & Model_year==y))!=0){
          CW <- subset(wgt_dt,Technology==techno & Size==size & Source=="epa" & Model_year==y)$Value
          #If no technology-specific EPA data for model year but conventional vehicle, consider the "all" techno value
        } else {
          CW <- subset(wgt_dt,Technology=="All" & Size==size & Source=="epa" & Model_year==y)$Value
        }
        #Second, consider material composition
        #Assumption: All vehicle technology have the same material composition. May not be true for alternative vehicles
        if (y %in% tedb_data_year_list){
          for (mat in unique(material$Own)) {
            #If aluminum, needs to be splitted in Wrought and cast
            if (grepl('Aluminum',mat)) {
              tmp_mc <- CW*subset(mat_content,Data=='Aluminum')[,as.character(y)]*subset(alu_hist,Data==mat)[,as.character(y)]
            } else {
              tmp_mc <- CW*sum(subset(mat_content,Data%in%unlist(strsplit(subset(material,Own==mat)$TEDB,split="; ")))[,as.character(y)])
            }
            fleet_mt_comp[nrow(fleet_mt_comp)+1,] <- c(y,size,techno,mat,tmp_mc)
          }
          #If no material composition data available, consider 1995 data but adjust aluminum content
          #Assumption the aluminum subsituted Mild steel
        } else {
          year_tbc <- min(tedb_data_year_list)
          new_alu_content <- subset(alu_hist,Data=='Aluminum')[,as.character(y)]*conv["kg","1 lb"]/subset(wgt_dt,Model_year==y & Size=="All" & Technology=="All")$Value
          for (mat in unique(material$Own)) {
            #If aluminum, needs to be splitted in Wrought and cast
            if (grepl('Aluminum',mat)) {
              tmp_mc <- CW*new_alu_content*subset(alu_hist,Data==mat)[,as.character(y)]
            } else if (grepl('Mild steel',mat)){
              #new_steel_content is the new content of steel
              new_steel_content <- sum(subset(mat_content,Data%in%unlist(strsplit(subset(material,Own==mat)$TEDB,split="; ")))[,as.character(year_tbc)]) + (subset(mat_content,Data=='Aluminum')[,as.character(year_tbc)]-new_alu_content)
              tmp_mc <- CW*new_steel_content
            } else {
              new_steel_content <- sum(subset(mat_content,Data%in%unlist(strsplit(subset(material,Own==mat)$TEDB,split="; ")))[,as.character(year_tbc)])
              tmp_mc <- CW*new_steel_content
            }
            fleet_mt_comp[nrow(fleet_mt_comp)+1,] <- c(y,size,techno,mat,tmp_mc)
          }
        }
      } else {
        for (mat in unique(material$Own)) {
          fleet_mt_comp[nrow(fleet_mt_comp)+1,] <- c(y,size,techno,mat,subset(fleet_i_mc,Size==size & Technology==techno & Material==mat)$Weight)
        }
        CW <- subset(fleet_compo_wgt,Component=="Total" & Size==size & Technology==techno)$Weight
      }
      fleet_mt_comp[nrow(fleet_mt_comp)+1,] <- c(y,size,techno,"Total",CW)
    }
  }
}
write.csv(fleet_mt_comp,"inputs/model/fleet_mt_comp_hist.csv", row.names = FALSE)

