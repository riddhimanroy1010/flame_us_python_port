###>Creates dataset of stock and sales projections directly taken from AEO projections.

source("inputs/scripts/api_eia_data.R")
library(readxl)
#Inputs
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year <- 2021
remove(list=c("out_sales_dt","out_stock_dt"))
for (aeo_case in aeo_case_list){
  sales_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="sales",key=key)
  stock_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="stock",key=key)
  #Format
  sales_dt[,"Technology"] <- sapply(sales_dt[,"Technology"],function(x)vh_techno$Own[sapply(1:nrow(vh_techno),function(y)x %in% unlist(strsplit(vh_techno$AEO[y],";")))])
  stock_dt[,"Technology"] <- sapply(stock_dt[,"Technology"],function(x)vh_techno$Own[sapply(1:nrow(vh_techno),function(y)x %in% unlist(strsplit(vh_techno$AEO[y],";")))])
  
  agg.formula <- reformulate(termlabels = c("Technology","Size","Year","Data_type","Aeo_case","Aeo_year","Unit"),response = "Value")
  sales_dt <- aggregate(data = sales_dt,agg.formula,FUN=sum)
  stock_dt <- aggregate(data = stock_dt,agg.formula,FUN=sum)
  
  sales_dt$Value <- trunc(sales_dt$Value)
  stock_dt$Value <- trunc(stock_dt$Value)
  #Output
  out_sales_dt <- rbind(get0("out_sales_dt"),sales_dt)
  out_stock_dt <- rbind(get0("out_stock_dt"),stock_dt)
}

write.csv(out_sales_dt,"inputs/model/fleet_sales_proj_aeo.csv", row.names = FALSE)
write.csv(out_stock_dt,"inputs/model/fleet_stock_proj_aeo.csv", row.names = FALSE)


