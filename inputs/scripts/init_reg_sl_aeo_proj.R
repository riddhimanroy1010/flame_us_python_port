###>Creates dataset of stock and sales projections directly taken from AEO projections.
library(modelframework)
source("inputs/scripts/api_eia_data.R")

library(readxl)
#Inputs
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year=2019
remove(list=c("out_sales_dt"))
for (aeo_case in aeo_case_list){
  sales_dt <- getAEOTransportation_reg(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Format
  sales_dt[,"Technology"] <- get_matching_names(sales_dt[,"Technology"],"vehicle_technology","AEO")
  agg.formula <- reformulate(termlabels = setdiff(colnames(sales_dt),"Value"),response = "Value")
  sales_dt <- aggregate(data = sales_dt,agg.formula,FUN=sum)
  sales_dt$Value <- trunc(sales_dt$Value)
  sales_dt[,"Aeo_case"] <- aeo_case
  #Output
  out_sales_dt <- rbind(get0("out_sales_dt"),sales_dt)
}

write.csv(out_sales_dt,"inputs/model/fleet_sales_reg_proj_aeo.csv", row.names = FALSE)


