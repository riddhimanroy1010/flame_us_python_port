###>Creates dataset of stock and sales projections directly taken from AEO projections.

source("inputs/scripts/api_eia_data.R")
library(readxl)
#Inputs
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_year_list=c(2014:2021)
remove(list=c("out_sales_dt","out_stock_dt"))
for (aeo_year in aeo_year_list){
  sales_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case="REF",aeo_data="sales",key=key)
  stock_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case="REF",aeo_data="stock",key=key)
  #Format
  sales_dt[,"Technology"] <- sapply(sales_dt[,"Technology"],function(x)vh_techno$Own[sapply(1:nrow(vh_techno),function(y)x %in% unlist(strsplit(vh_techno$AEO[y],";")))])
  stock_dt[,"Technology"] <- sapply(stock_dt[,"Technology"],function(x)vh_techno$Own[sapply(1:nrow(vh_techno),function(y)x %in% unlist(strsplit(vh_techno$AEO[y],";")))])
  
  agg.formula <- reformulate(termlabels = c("Technology","Size","Year","Data_type","Aeo_year","Unit"),response = "Value")
  sales_dt <- aggregate(data = sales_dt,agg.formula,FUN=sum)
  stock_dt <- aggregate(data = stock_dt,agg.formula,FUN=sum)
  
  sales_dt$Value <- trunc(sales_dt$Value)
  stock_dt$Value <- trunc(stock_dt$Value)
  
  #Output. Only consider data up to the AEO year-1
  out_sales_dt <- rbind(get0("out_sales_dt"),subset(sales_dt,Year<aeo_year))
  out_stock_dt <- rbind(get0("out_stock_dt"),subset(stock_dt,Year<aeo_year))
}

#For data with multiple years, only consider the one from most recent AEO
multiple_year_list <- setNames(lapply(unique(out_sales_dt$Year),function(x)ifelse(length(unique(subset(out_sales_dt,Year==x)$Aeo_year)>1),max(unique(subset(out_sales_dt,Year==x)$Aeo_year)),unique(subset(out_sales_dt,Year==x)$Aeo_year))),unique(out_sales_dt$Year))

final_sales_dt <- NULL
final_stock_dt <- NULL
for (year in names(multiple_year_list)){
  final_sales_dt <- rbind(final_sales_dt,subset(out_sales_dt,Year==year & Aeo_year==multiple_year_list[[year]]))
  final_stock_dt <- rbind(final_stock_dt,subset(out_stock_dt,Year==year & Aeo_year==multiple_year_list[[year]]))
}

write.csv(final_sales_dt,"inputs/model/fleet_sales_hist_aeo_14-21.csv", row.names = FALSE)
write.csv(final_stock_dt,"inputs/model/fleet_stock_hist_aeo_14-21.csv", row.names = FALSE)


