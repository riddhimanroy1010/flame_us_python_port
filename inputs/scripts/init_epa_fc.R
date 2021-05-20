#Extract fleet average curb weight data by vehicle siz and type from EPA tables (Appendix I of fuel economy trends)
library(readxl)
#Conversion file
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
fuel_conv <- read.csv("inputs/data/fuel_conversion.csv",stringsAsFactors = FALSE,check.names = FALSE)

app_i_dt <- as.data.frame(read_excel("inputs/data/2018_fe_trends_epa_app_i.xlsx", col_names = TRUE))
colnames(app_i_dt)[colnames(app_i_dt)=="Prod (000)"] <- "Prod"
app_i_dt$`Prod`[is.na(app_i_dt$`Prod`)] <- 0
#Delete rows without prod ("-")
app_i_dt <- subset(app_i_dt,Prod!="-" & Prod!=0)
app_i_dt$Prod <- as.numeric(app_i_dt$Prod)
colnames(app_i_dt)[colnames(app_i_dt)=="Weight\r\n(lbs)"] <- "Weight"
colnames(app_i_dt)[colnames(app_i_dt)=="Model \r\nYear"] <- "Model Year"

app_i_dt$`Weight`[is.na(app_i_dt$`Weight`)] <- 0
app_i_dt$Weight <- as.numeric(app_i_dt$Weight)
#Format and convert adjusted Fuel Economy
colnames(app_i_dt)[colnames(app_i_dt)=="...9"] <- "Adj_fc"
app_i_dt$Adj_fc <- as.numeric(app_i_dt$Adj_fc)
#Convert MPG in L of gasoline /100km
app_i_dt$Adj_fc <- 1/app_i_dt$Adj_fc*conv["L","1 gal"]*conv["mile","1 km"]*100

app_d_dt <- as.data.frame(read_excel("inputs/data/2018_fe_trends_epa_app_d.xlsx", col_names = TRUE))
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
#Format and convert adjusted Fuel Economy
colnames(app_d_dt)[colnames(app_d_dt)=="...9"] <- "Adj_fc"
app_d_dt$Adj_fc <- as.numeric(app_d_dt$Adj_fc)
#Convert MPG in L of gasoline /100km
app_d_dt$Adj_fc <- 1/app_d_dt$Adj_fc*conv["L","1 gal"]*conv["mile","1 km"]*100

veh_size <- data.frame("Category" = sort(unique(app_i_dt$'Vehicle Type')), stringsAsFactors = FALSE)
veh_size[veh_size$Category%in%c("Car","Car SUV","Sedan/Wagon"),"Size"] <- "Car"
veh_size[veh_size$Category%in%c("Pickup","Truck SUV","Van"),"Size"] <- "Light truck"

veh_techno <- data.frame("Epa_techno" = setdiff(unique(app_i_dt$`Engine Type and Valves Per Cylinder`),NA), stringsAsFactors = FALSE)
veh_techno[veh_techno$Epa_techno=='Diesel','Technology'] <- "ICEV-D"
veh_techno[veh_techno$Epa_techno=='Alternative Fuel','Technology'] <- "AFV"
veh_techno[is.na(veh_techno$Technology),'Technology'] <- "ICEV-G"

year_list <- setdiff(unique(app_d_dt$`Model Year`),NA)
#Output
dt_col <- c("Model_year","Size","Technology","Fuel_type","Value")
dtf_fe <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (year in year_list){
  tmp_dt <- subset(app_d_dt,`Model Year`==year & Type=="All" & Size=="All")
  fc_val <- sum(tmp_dt$Prod*tmp_dt$Adj_fc)/sum(tmp_dt$Prod)
  dtf_fe[nrow(dtf_fe)+1,] <- c(year,"All","All","Gasoline",fc_val)
  for (size in unique(veh_size$Size)){
    tmp_dt <- subset(app_d_dt,`Model Year`==year & Size==size & Type=="All")
    if (sum(tmp_dt$Prod)>0){
      fc_val <- sum(tmp_dt$Prod*tmp_dt$Adj_fc)/sum(tmp_dt$Prod)
      dtf_fe[nrow(dtf_fe)+1,] <- c(year,size,"All","Gasoline",fc_val)
      for (techno in unique(veh_techno$Technology)){
        tmp_dt <- subset(app_i_dt,`Model Year`==year & `Vehicle Type`%in%subset(veh_size,Size==size)$Category & `Engine Type and Valves Per Cylinder`%in%subset(veh_techno,Technology==techno)$Epa_techno)
        if (sum(tmp_dt$Prod)>0){
          if (techno!="AFV"){
            fuel_type <- subset(vh_techno,Own==techno)$`Fuel type`
            conv_val <- subset(fuel_conv,Fuel=="Gasoline" & Data=="Conversion factor")$Value/subset(fuel_conv,Fuel==fuel_type & Data=="Conversion factor")$Value
          } else {
            fuel_type <- "Gasoline"
            conv_val <- 1
          }
          fc_val <- sum(tmp_dt$Prod*tmp_dt$Adj_fc)/sum(tmp_dt$Prod)*conv_val
          dtf_fe[nrow(dtf_fe)+1,] <- c(year,size,techno,fuel_type,fc_val)
        }
      }
    }
  }
}
dtf_fe[,"Source"] <- "epa"
#Convert lbs in kg
dtf_fe[,'Unit'] <- "L/100km"

write.csv(dtf_fe, "inputs/model/epa_fleet_fc_hist.csv", row.names = FALSE)

