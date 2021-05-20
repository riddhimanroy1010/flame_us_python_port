#Extract fleet average curb weight data by vehicle siz and type from EPA tables (Appendix I of fuel economy trends)
library(readxl)
#Conversion file
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)

app_i_dt <- read_excel("inputs/data/2017_fe_trends_epa_app_i.xlsx", col_names = TRUE)
colnames(app_i_dt)[colnames(app_i_dt)=="Prod (000)"] <- "Prod"
app_i_dt$`Prod`[is.na(app_i_dt$`Prod`)] <- 0
#Delete rows without prod ("-")
app_i_dt <- subset(app_i_dt,Prod!="-" & Prod!=0)
app_i_dt$Prod <- as.numeric(app_i_dt$Prod)
colnames(app_i_dt)[colnames(app_i_dt)=="Weight\r\n(lb)"] <- "Weight"
app_i_dt$`Weight`[is.na(app_i_dt$`Weight`)] <- 0
app_i_dt$Weight <- as.numeric(app_i_dt$Weight)

app_d_dt <- read_excel("inputs/data/2017_fe_trends_epa_app_d.xlsx", col_names = TRUE)
colnames(app_d_dt)[colnames(app_d_dt)=="Car or\r\nTruck"] <- "Size"
colnames(app_d_dt)[colnames(app_d_dt)=="Vehicle\r\nType"] <- "Type"
app_d_dt[app_d_dt=="Truck"] <- "Light truck"
colnames(app_d_dt)[colnames(app_d_dt)=="Prod (000)"] <- "Prod"
app_d_dt$`Prod`[is.na(app_d_dt$`Prod`)] <- 0
#Delete rows without prod ("-")
app_d_dt <- subset(app_d_dt,Prod!="-" & Prod!=0)
app_d_dt$Prod <- as.numeric(app_d_dt$Prod)
colnames(app_d_dt)[colnames(app_d_dt)=="Weight\r\n(lb)"] <- "Weight"
app_d_dt$`Weight`[is.na(app_d_dt$`Weight`)] <- 0
app_d_dt$Weight <- as.numeric(app_d_dt$Weight)

veh_size <- data.frame("Category" = sort(unique(app_i_dt$'Vehicle Type')), stringsAsFactors = FALSE)
veh_size[which(veh_size$Category=="Car" | veh_size$Category=="Car SUV"),"Size"] <- "Car"
veh_size[which(veh_size$Category=="Pickup" | veh_size$Category=="Truck SUV"| veh_size$Category=="Van"),"Size"] <- "Light truck"

veh_techno <- data.frame("Epa_techno" = setdiff(unique(app_i_dt$`Engine Type and Valves`),NA), stringsAsFactors = FALSE)
veh_techno[veh_techno$Epa_techno=='Diesel','Technology'] <- "ICEV-D"
veh_techno[veh_techno$Epa_techno=='Alternative Fuel','Technology'] <- "AFV"
veh_techno[is.na(veh_techno$Technology),'Technology'] <- "ICEV-G"

year_list <- setdiff(unique(app_d_dt$`Model Year`),NA)
#Output
dt_col <- c("Model_year","Size","Technology","Value")
dtf_wgt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (year in year_list){
  tmp_dt <- subset(app_d_dt,`Model Year`==year & Type=="All" & Size=="All")
  wgt_val <- sum(tmp_dt$Prod*tmp_dt$Weight)/sum(tmp_dt$Prod)
  dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,"All","All",wgt_val)
  for (size in unique(veh_size$Size)){
    tmp_dt <- subset(app_d_dt,`Model Year`==year & Size==size & Type=="All")
    if (sum(tmp_dt$Prod)>0){
      wgt_val <- sum(tmp_dt$Prod*tmp_dt$Weight)/sum(tmp_dt$Prod)
      dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,size,"All",wgt_val)
      for (techno in unique(veh_techno$Technology)){
        tmp_dt <- subset(app_i_dt,`Model Year`==year & `Vehicle Type`%in%subset(veh_size,Size==size)$Category & `Engine Type and Valves`%in%subset(veh_techno,Technology==techno)$Epa_techno)
        if (sum(tmp_dt$Prod)>0){
          wgt_val <- sum(tmp_dt$Prod*tmp_dt$Weight)/sum(tmp_dt$Prod)
          dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,size,techno,wgt_val)
        }
      }
    }
  }
}
dtf_wgt[,"Source"] <- "epa"
#Convert lbs in kg
dtf_wgt$Value <- as.numeric(dtf_wgt$Value)*conv["kg","1 lb"]
dtf_wgt[,'Unit'] <- "kg"

write.csv(dtf_wgt, "inputs/model/epa_fleet_wgt_hist.csv", row.names = FALSE)

