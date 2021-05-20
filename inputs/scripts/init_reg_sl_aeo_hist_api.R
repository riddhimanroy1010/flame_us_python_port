###>Creates dataset of stock and sales projections directly taken from AEO projections.

source("inputs/scripts/api_eia_data.R")
source("utils/data_processing_f.R")
library(readxl)
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_year_list=c(2014:2019)
remove(list=c("out_sales_dt"))
for (aeo_year in aeo_year_list){
  sales_dt <- getAEOTransportation_reg(aeo_year=aeo_year,aeo_case="REF",key=key)
  #Format
  sales_dt[,"Technology"] <- get_matching_names(sales_dt[,"Technology"],"vehicle_technology","AEO")
  agg.formula <- reformulate(termlabels = setdiff(colnames(sales_dt),"Value"),response = "Value")
  sales_dt <- aggregate(data = sales_dt,agg.formula,FUN=sum)
  sales_dt$Value <- trunc(sales_dt$Value)
  #Output
  out_sales_dt <- rbind(get0("out_sales_dt"),subset(sales_dt,Year<aeo_year))
}

#For data with multiple years, only consider the one from most recent AEO
multiple_year_list <- setNames(lapply(unique(out_sales_dt$Year),function(x)ifelse(length(unique(subset(out_sales_dt,Year==x)$Aeo_year)>1),max(unique(subset(out_sales_dt,Year==x)$Aeo_year)),unique(subset(out_sales_dt,Year==x)$Aeo_year))),unique(out_sales_dt$Year))

final_sales_dt <- NULL
for (year in names(multiple_year_list)){
  final_sales_dt <- rbind(final_sales_dt,subset(out_sales_dt,Year==year & Aeo_year==multiple_year_list[[year]]))
}

write.csv(final_sales_dt,"inputs/model/fleet_sales_reg_hist_aeo_14-19.csv", row.names = FALSE)


