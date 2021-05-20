###>Script: Compile fuel consumption values from sales-weighted values and fuel economy per model for EV.
library(readxl)
library(tidyr)
library(reshape2)
#Input files
fe_dt <- read.csv("inputs/data/fueleconomy_v200116.csv", stringsAsFactors = FALSE, check.names = FALSE)
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
conv <- read.csv("inputs/user/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
ev_sales <- read.csv("inputs/data/ev_sales_us_v200116.csv", stringsAsFactors = FALSE, check.names = FALSE)
ev_sales_model_matching <- read.csv("inputs/data/ev_sales_us_model_matching_v200116.csv", stringsAsFactors = FALSE, check.names = FALSE)
ev_model_us_battery_capacity <- read.csv("inputs/data/ev_model_us_battery_capacity.csv", stringsAsFactors = FALSE, check.names = FALSE)

#Format fe_dt
fe_dt <- subset(fe_dt,atvType%in%c("EV","Plug-in Hybrid"))
fe_dt <- fe_dt[,order(colnames(fe_dt))]
#Convert combined fuel economy into fuel consumption
data_tbc <- c("comb08")
for (d in data_tbc){
  fe_dt[,paste0(d,"_ed")]<-1/fe_dt[,d]*conv["L","1 gal"]*conv["mile","1 km"]*100
}
#Convert combined electricty consumption in kWh/100km
col_tbc<-c("combE")
for (d in col_tbc){
  fe_dt[,paste0(d,"_ed")]<-fe_dt[,d]*conv["mile","1 km"]
}
#Convert range in km
fe_dt$range <- fe_dt$range/conv["mile","1 km"]
fe_dt$rangeA <- as.numeric(fe_dt$rangeA)
fe_dt$rangeA[is.na(fe_dt$rangeA)] <- 0
fe_dt$rangeA <- fe_dt$rangeA/conv["mile","1 km"]
#veh_class links vehicle class and our size
veh_class <- data.frame("Class" = unique(fe_dt$VClass), stringsAsFactors = FALSE)
car <- c(1,3,5,8,9,10,12,13)
veh_class[car,"Size"] <- "Car"
veh_class[-car,"Size"] <- "Light truck"
#Update fe_data with our size
fe_dt$Size <- sapply(1:nrow(fe_dt),function(x)subset(veh_class,Class==fe_dt[x,"VClass"])$Size)

#Update fe_data with ou vehicle type
fe_dt$Technology <- rename_values(fe_dt$atvType,list("BEV"="EV","PHEV"="Plug-in Hybrid"))

#Eliminate some columns
fe_dt <- fe_dt[,c("Size","Technology","year","make","model","range","rangeA","comb08_ed","combE_ed")]

#Estimate the battery capacity. First BEV
fe_dt$Battery_capacity <- sapply(1:nrow(fe_dt),function(x)ifelse(fe_dt[x,"Technology"]=="BEV",fe_dt[x,"range"],fe_dt[x,"rangeA"])*fe_dt[x,"combE_ed"]/100*0.8/0.9)

#Format sales
ev_sales[is.na(ev_sales)] <- 0
ev_sales <- gather(data=ev_sales,key="Year",value="Value",-c(Data,Vehicle,Type),convert=TRUE)
ev_sales <- subset(ev_sales,Value!=0)
#Update make and model that matches with fueleconomy.gov database
#Update size, range and fuel consumption
for (i in 1:nrow(ev_sales)){
  #Update make, model and size
  ev_sales[i,"Make"] <- subset(ev_sales_model_matching,Vehicle==ev_sales[i,"Vehicle"])$Make
  ev_sales[i,"Model"] <- subset(ev_sales_model_matching,Vehicle==ev_sales[i,"Vehicle"])[,as.character(ev_sales[i,"Year"])]
  ev_sales[i,"Size"] <- unique(subset(fe_dt,make==ev_sales[i,"Make"] & model==ev_sales[i,"Model"])$Size)
  #Update range, battery capacity, fuel consumptions (CD and CS modes for PHEV)
  #if model_year exists, consider it
  if (ev_sales[i,"Year"]%in%subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"])$year){
    year_tbc <- ev_sales[i,"Year"]
  #Else if earlier year exists, consider the maximum of earlier
  } else if (any(ev_sales[i,"Year"] > subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"])$year)){
    year_tbc <- max(subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"] & year<ev_sales[i,"Year"])$year)
  #Else, consider the closest year (even above)
  } else {
    year_tbc <- min(subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"])$year)
  }
  ev_sales[i,"FC_cd_mode"] <- subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"] & year==year_tbc)$combE_ed
  ev_sales[i,"Battery_capacity"] <- subset(ev_model_us_battery_capacity,Vehicle==ev_sales[i,"Vehicle"])$Value
  if (ev_sales[i,"Type"]=="BEV"){
    ev_sales[i,"Range"] <- subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"] & year==year_tbc)$range
    ev_sales[i,"Technology"] <- ifelse(ev_sales[i,"Range"]<300,"BEV100","BEV300")
  } else if (ev_sales[i,"Type"]=="PHEV"){
    ev_sales[i,"Range"] <- subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"] & year==year_tbc)$rangeA
    ev_sales[i,"Technology"] <- ifelse(ev_sales[i,"Range"]<30,"PHEV20","PHEV40")
    ev_sales[i,"FC_cs_mode"] <- subset(fe_dt,make==ev_sales[i,"Make"]  & model==ev_sales[i,"Model"] & year==year_tbc)$comb08_ed
  }
}
#Calculate sales weighted
ev_sales$SW_fc_cd_mode <- sapply(1:nrow(ev_sales),function(x)ev_sales[x,"FC_cd_mode"]*ev_sales[x,"Value"]/sum(subset(ev_sales,Year==ev_sales[x,"Year"] & Technology==ev_sales[x,"Technology"] & Size==ev_sales[x,"Size"])$Value))
ev_sales$SW_fc_cs_mode <- sapply(1:nrow(ev_sales),function(x)ev_sales[x,"FC_cs_mode"]*ev_sales[x,"Value"]/sum(subset(ev_sales,Year==ev_sales[x,"Year"] & Technology==ev_sales[x,"Technology"] & Size==ev_sales[x,"Size"])$Value))
ev_sales$SW_battery_capacity <- sapply(1:nrow(ev_sales),function(x)ev_sales[x,"Battery_capacity"]*ev_sales[x,"Value"]/sum(subset(ev_sales,Year==ev_sales[x,"Year"] & Technology==ev_sales[x,"Technology"] & Size==ev_sales[x,"Size"])$Value))


#Create output dataframe
dt_col<-c("Year","Size","Technology","Fuel","Model","Unit","value")
ev_fc_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

#Electrical consumption
#Sales_weighted matrix
matrix_sw_fc_cd_mode <- acast(data=ev_sales, Size+Technology ~ Year , value.var='SW_fc_cd_mode',fun.aggregate=sum, margins=FALSE)
#Adjust data
matrix_sw_fc_cd_mode["Light truck_BEV100",as.character(2016:2019)] <- matrix_sw_fc_cd_mode["Light truck_BEV100","2015"]

tmp_ev_fc_dt <- as.data.frame(matrix_sw_fc_cd_mode) %>% 
  cbind(Type=rownames(matrix_sw_fc_cd_mode),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Type,convert=TRUE)
tmp_ev_fc_dt[,"Size"] <- substr(tmp_ev_fc_dt$Type,0,as.numeric(regexpr(pattern="_",tmp_ev_fc_dt$Type))-1)
tmp_ev_fc_dt[,"Technology"] <- substr(tmp_ev_fc_dt$Type,as.numeric(regexpr(pattern="_",tmp_ev_fc_dt$Type))+1,200)
tmp_ev_fc_dt[,"Type"] <- NULL
tmp_ev_fc_dt <- subset(tmp_ev_fc_dt,Value!=0)
tmp_ev_fc_dt$Fuel <- "Electricity"
tmp_ev_fc_dt$Unit <- "kWh/100km"
tmp_ev_fc_dt$Model <- "Sales weighted"
#Combine
ev_fc_dt <- rbind(ev_fc_dt,tmp_ev_fc_dt)

#Gasoline consumption for PHEV
#Sales_weighted matrix
matrix_sw_fc_cs_mode <- acast(data=subset(ev_sales,Type=="PHEV"), Size+Technology ~ Year , value.var='SW_fc_cs_mode',fun.aggregate=sum, margins=FALSE)

tmp_ev_fc_dt <- as.data.frame(matrix_sw_fc_cs_mode) %>% 
  cbind(Type=rownames(matrix_sw_fc_cs_mode),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Type,convert=TRUE)
tmp_ev_fc_dt[,"Size"] <- substr(tmp_ev_fc_dt$Type,0,as.numeric(regexpr(pattern="_",tmp_ev_fc_dt$Type))-1)
tmp_ev_fc_dt[,"Technology"] <- substr(tmp_ev_fc_dt$Type,as.numeric(regexpr(pattern="_",tmp_ev_fc_dt$Type))+1,200)
tmp_ev_fc_dt[,"Type"] <- NULL
tmp_ev_fc_dt <- subset(tmp_ev_fc_dt,Value!=0)
tmp_ev_fc_dt$Fuel <- "Gasoline"
tmp_ev_fc_dt$Unit <- "L/100km"
tmp_ev_fc_dt$Model <- "Sales weighted"
#Combine
ev_fc_dt <- rbind(ev_fc_dt,tmp_ev_fc_dt)

#Values for 2020
#In the default mode, we assume similar value than 2019
tmp_ev_fc_dt <- subset(ev_fc_dt,Year==2019)
tmp_ev_fc_dt$Year <- 2020
tmp_ev_fc_dt$Model <- "def"
ev_fc_dt <- rbind(ev_fc_dt,tmp_ev_fc_dt)

#In the low and high model, we consider the following representative models
#Following models are based on high selling vehicles with higher or lower FC than sales weighted
low_range = list("Car_BEV100"="VW e-Golf",
                 "Car_BEV300"="Chevrolet Bolt",
                 "Light truck_BEV100"="Toyota RAV4 EV",
                 "Light truck_BEV300"="Hyundai Kona",
                 "Car_PHEV20"="Audi A3 Plug In",
                 "Car_PHEV40"="Toyota Prius Prime",
                 "Light truck_PHEV20"="Volvo XC60",
                 "Light truck_PHEV40"="Chrysler Pacifica")
tmp_ev_fc_dt <- subset(ev_fc_dt,Year==2019)
tmp_ev_fc_dt$Year <- 2020
tmp_ev_fc_dt$Model <- "low"
tmp_ev_fc_dt$Value <- sapply(1:nrow(tmp_ev_fc_dt),function(x)mean(subset(ev_sales,Vehicle==low_range[[paste0(tmp_ev_fc_dt[x,"Size"],"_",tmp_ev_fc_dt[x,"Technology"])]])[,ifelse(tmp_ev_fc_dt[x,"Fuel"]=="Electricity","FC_cd_mode","FC_cs_mode")]))
ev_fc_dt <- rbind(ev_fc_dt,tmp_ev_fc_dt)
#BONUS FOR SI
#Calculate market share in each segment of the representative vehicle
ms_year = 2015
sapply(names(low_range),function(x)subset(ev_sales,Year==ms_year & Vehicle==low_range[[x]])$Value/sum(subset(ev_sales,Year==ms_year & Technology==subset(ev_sales,Year==ms_year & Vehicle==low_range[[x]])$Technology[1] & Size==subset(ev_sales,Vehicle==low_range[[x]])$Size[1])$Value))

high_range = list("Car_BEV100"="Nissan LEAF",
                  "Car_BEV300"="Tesla Model S",
                  "Light truck_BEV100"="Toyota RAV4 EV",
                  "Light truck_BEV300"="Jaguar I-Pace",
                  "Car_PHEV20"="Mercedes C350We Plug-in Hybrid",
                  "Car_PHEV40"="Chevrolet Volt",
                  "Light truck_PHEV20"="Porsche Cayenne S E-Hybrid",
                  "Light truck_PHEV40"="Mitsubishi Outlander")
tmp_ev_fc_dt <- subset(ev_fc_dt,Year==2019)
tmp_ev_fc_dt$Year <- 2020
tmp_ev_fc_dt$Model <- "high"
tmp_ev_fc_dt$Value <- sapply(1:nrow(tmp_ev_fc_dt),function(x)mean(subset(ev_sales,Vehicle==high_range[[paste0(tmp_ev_fc_dt[x,"Size"],"_",tmp_ev_fc_dt[x,"Technology"])]])[,ifelse(tmp_ev_fc_dt[x,"Fuel"]=="Electricity","FC_cd_mode","FC_cs_mode")]))
ev_fc_dt <- rbind(ev_fc_dt,tmp_ev_fc_dt)
#BONUS FOR SI
#Calculate market share in each segment of the representative vehicle
ms_year = 2019
sapply(names(high_range),function(x)subset(ev_sales,Year==ms_year & Vehicle==high_range[[x]])$Value/sum(subset(ev_sales,Year==ms_year & Technology==subset(ev_sales,Year==ms_year & Vehicle==high_range[[x]])$Technology[1] & Size==subset(ev_sales,Vehicle==high_range[[x]])$Size[1])$Value))

#One cases have only one model: Light truck_BEV100 and Light truck_PHEV40. We search in
#Check the results
fc_check_mat <- acast(data=subset(ev_fc_dt,Year==2020 & Fuel=="Electricity"), Size+Technology+Fuel ~ Model , value.var='Value',fun.aggregate=sum, margins=FALSE)

write.csv(ev_fc_dt,"inputs/model/fc_ev_hist.csv",row.names = FALSE)

#Calculate battery capacity
#Create output dataframe
dt_col<-c("Year","Size","Technology","Model","Unit","value")
ev_bat_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

#Electrical consumption
#Sales_weighted matrix
matrix_sw_bat <- acast(data=ev_sales, Size+Technology ~ Year , value.var='SW_battery_capacity',fun.aggregate=sum, margins=FALSE)
#Adjust data
matrix_sw_bat["Light truck_BEV100",as.character(2016:2019)] <- matrix_sw_bat["Light truck_BEV100","2015"]

tmp_ev_bat_dt <- as.data.frame(matrix_sw_bat) %>% 
  cbind(Type=rownames(matrix_sw_bat),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Type,convert=TRUE)
tmp_ev_bat_dt[,"Size"] <- substr(tmp_ev_bat_dt$Type,0,as.numeric(regexpr(pattern="_",tmp_ev_bat_dt$Type))-1)
tmp_ev_bat_dt[,"Technology"] <- substr(tmp_ev_bat_dt$Type,as.numeric(regexpr(pattern="_",tmp_ev_bat_dt$Type))+1,200)
tmp_ev_bat_dt[,"Type"] <- NULL
tmp_ev_bat_dt <- subset(tmp_ev_bat_dt,Value!=0)
tmp_ev_bat_dt$Unit <- "kWh"
tmp_ev_bat_dt$Model <- "Sales weighted"
#Combine
ev_bat_dt <- rbind(ev_bat_dt,tmp_ev_bat_dt)

#Values for 2020
#In the default mode, we assume similar value than 2019
tmp_ev_bat_dt <- subset(ev_bat_dt,Year==2019)
tmp_ev_bat_dt$Year <- 2020
tmp_ev_bat_dt$Model <- "def"
ev_bat_dt <- rbind(ev_bat_dt,tmp_ev_bat_dt)

#In the low and high model, we consider the following representative models
#Following models are based on high selling vehicles with higher or lower FC than sales weighted
tmp_ev_bat_dt <- subset(ev_bat_dt,Year==2019)
tmp_ev_bat_dt$Year <- 2020
tmp_ev_bat_dt$Model <- "low"
tmp_ev_bat_dt$Value <- sapply(1:nrow(tmp_ev_bat_dt),function(x)mean(subset(ev_sales,Vehicle==low_range[[paste0(tmp_ev_bat_dt[x,"Size"],"_",tmp_ev_bat_dt[x,"Technology"])]])[,"Battery_capacity"]))
ev_bat_dt <- rbind(ev_bat_dt,tmp_ev_bat_dt)


tmp_ev_bat_dt <- subset(ev_bat_dt,Year==2019)
tmp_ev_bat_dt$Year <- 2020
tmp_ev_bat_dt$Model <- "high"
tmp_ev_bat_dt$Value <- sapply(1:nrow(tmp_ev_bat_dt),function(x)mean(subset(ev_sales,Vehicle==high_range[[paste0(tmp_ev_bat_dt[x,"Size"],"_",tmp_ev_bat_dt[x,"Technology"])]])[,"Battery_capacity"]))
ev_bat_dt <- rbind(ev_bat_dt,tmp_ev_bat_dt)

#CHANGE
ev_bat_dt[ev_bat_dt$Year==2020 & ev_bat_dt$Model=="high" & ev_bat_dt$Size=="Light truck" & ev_bat_dt$Technology=="BEV300","Value"] <- 100
ev_bat_dt[ev_bat_dt$Year==2020 & ev_bat_dt$Model=="high" & ev_bat_dt$Size=="Light truck" & ev_bat_dt$Technology=="PHEV40","Value"] <- 16
ev_bat_dt[ev_bat_dt$Year==2020 & ev_bat_dt$Model=="low" & ev_bat_dt$Size=="Light truck" & ev_bat_dt$Technology=="PHEV40","Value"] <- 12

#Check the results
bat_check_mat <- acast(data=subset(ev_bat_dt,Year==2020), Size+Technology ~ Model , value.var='Value',fun.aggregate=sum, margins=FALSE)

write.csv(ev_bat_dt,"inputs/model/battery_capacity_ev_hist.csv",row.names = FALSE)

#Create a dataframe for SI

dt_col <- c("Size","Technology","Fuel","Case","Vehicle","FC","Battery")
dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (size in c("Car","Light truck")){
  for (techno in c("BEV100","BEV300","PHEV20","PHEV40")){
    dt <- rbind(dt,data.frame(Size=size,Technology=techno,Fuel="Electricity",Case="Low",Vehicle=low_range[[paste0(size,"_",techno)]],FC=mean(subset(ev_sales,Vehicle==low_range[[paste0(size,"_",techno)]])[,"FC_cd_mode"]),Battery=mean(subset(ev_sales,Vehicle==low_range[[paste0(size,"_",techno)]])[,"Battery_capacity"])))
    dt <- rbind(dt,data.frame(Size=size,Technology=techno,Fuel="Electricity",Case="High",Vehicle=high_range[[paste0(size,"_",techno)]],FC=mean(subset(ev_sales,Vehicle==high_range[[paste0(size,"_",techno)]])[,"FC_cd_mode"]),Battery=mean(subset(ev_sales,Vehicle==high_range[[paste0(size,"_",techno)]])[,"Battery_capacity"])))
    if (grepl("PHEV",techno)){
      dt <- rbind(dt,data.frame(Size=size,Technology=techno,Fuel="Gasoline",Case="Low",Vehicle=low_range[[paste0(size,"_",techno)]],FC=mean(subset(ev_sales,Vehicle==low_range[[paste0(size,"_",techno)]])[,"FC_cs_mode"]),Battery=mean(subset(ev_sales,Vehicle==low_range[[paste0(size,"_",techno)]])[,"Battery_capacity"])))
      dt <- rbind(dt,data.frame(Size=size,Technology=techno,Fuel="Gasoline",Case="High",Vehicle=high_range[[paste0(size,"_",techno)]],FC=mean(subset(ev_sales,Vehicle==high_range[[paste0(size,"_",techno)]])[,"FC_cs_mode"]),Battery=mean(subset(ev_sales,Vehicle==high_range[[paste0(size,"_",techno)]])[,"Battery_capacity"])))
      
    }
  }
}
write.csv(dt,"outputs/plots/si/rep_veh.csv",row.names = FALSE)


#Create a dataframe of sales-weighted capacity for SI
write.csv(bat_check_mat,"outputs/plots/si/sales_weighted_bat_Cap.csv")
