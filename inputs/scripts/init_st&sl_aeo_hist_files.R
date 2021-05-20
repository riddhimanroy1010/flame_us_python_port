###>Creates the historical fleet stock and sales derived from the Annual Energy Outlook 2007 to 2018 versions and VISION.
#Inputs
lf_stock<-list.files("inputs/data/aeo_stock_sales",pattern="Stock_by_Technology", full.names=TRUE)
lf_sales<-list.files("inputs/data/aeo_stock_sales",pattern="Sales_by_Technology", full.names=TRUE)
library(readxl)
#Inputs
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
#List of technologies in AEO data
aeo_technology_list <- unique(unlist(strsplit(vh_techno$AEO,";")))
#Output files
dt_col <-c ("Data_type","Year","Size","Technology","Value")
aeo_stock_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Read stock files
for (l in 1:length(lf_stock)){
  #Read file
  dt <- read.csv(lf_stock[l], header = TRUE,skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  #Clean and delete columns
  dt$`api key`<-NULL
  dt$`full name`<-NULL
  dt[,5:ncol(dt)]<-NULL
  #Read values per Year (issues: AEO11 data not available and AEO16 starts at 2014 so 2013 not available. In two cases, consider data from previous AEO versions)
  for (y in 3:ifelse(colnames(dt)[3]==2012|colnames(dt)[3]==2007,4,3)){
    #yr is Year of data
    yr=colnames(dt)[y]
    #Get Cars data
    cars_rows <- intersect(1:which(dt[,1]=="Light Truck Stock"),which(dt[,1] %in% aeo_technology_list))
    for (r in cars_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-vh_techno$Own[sapply(1:nrow(vh_techno),function(x)dt[r,1] %in% unlist(strsplit(vh_techno$AEO[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_stock_dt[nrow(aeo_stock_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,"Car",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
    #Get Light-trucks data
    lt_rows <- intersect(which(dt[,1]=="Light Truck Stock"):which(dt[,1]=="Total Stock"),which(dt[,1] %in% aeo_technology_list))
    for (r in lt_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-vh_techno$Own[sapply(1:nrow(vh_techno),function(x)dt[r,1] %in% unlist(strsplit(vh_techno$AEO[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_stock_dt[nrow(aeo_stock_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,"Light truck",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
  }
}

#Format and aggregate multiple technologies
aeo_stock_dt$Year<-as.numeric(aeo_stock_dt$Year)
aeo_stock_dt$Value<-as.numeric(aeo_stock_dt$Value)
agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type"),response = "Value")
aeo_stock_dt<-aggregate(data = aeo_stock_dt,agg.formula,FUN=sum)
aeo_stock_dt[,"Unit"]<-"vehicle"
aeo_stock_dt[,"Aeo_year"]<-2010
write.csv(aeo_stock_dt,"inputs/model/fleet_stock_hist_aeo_10-13.csv", row.names = FALSE)


dt_col <-c ("Data_type","Year","Size","Technology","Value")
aeo_sale_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Read sales files
for (l in 1:length(lf_sales)){
  #Read file
  dt<-read.csv(lf_sales[l], header = TRUE,skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  #Clean and delete columns
  dt$`api key`<-NULL
  dt$`full name`<-NULL
  dt[,5:ncol(dt)]<-NULL
  #Read values per Year (issues: AEO11 data not available and AEO16 starts at 2014 so 2013 not available. In two cases, consider data from previous AEO versions)
  for (y in 3:ifelse(colnames(dt)[3]==2012|colnames(dt)[3]==2007,4,3)){
    #yr is Year of data
    yr=colnames(dt)[y]
    #Get Cars data
    cars_rows <- intersect(1:which(dt[,1]=="New Light Truck Sales"),which(dt[,1] %in% aeo_technology_list))
    for (r in cars_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-vh_techno$Own[sapply(1:nrow(vh_techno),function(x)dt[r,1] %in% unlist(strsplit(vh_techno$AEO[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_sale_dt[nrow(aeo_sale_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,"Car",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
    #Get Light-trucks data
    lt_rows <- intersect(which(dt[,1]=="New Light Truck Sales"):which(dt[,1]=="Total New Light Truck Sales"),which(dt[,1] %in% aeo_technology_list))
    for (r in lt_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-vh_techno$Own[sapply(1:nrow(vh_techno),function(x)dt[r,1] %in% unlist(strsplit(vh_techno$AEO[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_sale_dt[nrow(aeo_sale_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,"Light truck",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
  }
}
#Format and aggregate multiple technologies
aeo_sale_dt$Year<-as.numeric(aeo_sale_dt$Year)
aeo_sale_dt$Value<-as.numeric(aeo_sale_dt$Value)
agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type"),response = "Value")
aeo_sale_dt<-aggregate(data = aeo_sale_dt,agg.formula,FUN=sum)
aeo_sale_dt[,"Unit"]<-"vehicle"
aeo_sale_dt[,"Aeo_year"]<-2010
write.csv(aeo_sale_dt,"inputs/model/fleet_sales_hist_aeo_10-13.csv", row.names = FALSE)

