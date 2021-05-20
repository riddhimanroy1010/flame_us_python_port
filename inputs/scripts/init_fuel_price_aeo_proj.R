###>Creates dataset of stock and sales projections directly taken from AEO projections.

source("inputs/scripts/api_eia_data.R")
library(readxl)
#Inputs
conv <- read.csv("inputs/user/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
fuel_matching <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "fuel"))
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year <- 2019
#Output
out_aeo_dt <- NULL
for (aeo_case in aeo_case_list){
  aeo_dt <- getAEOFuelPrices(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Format
  aeo_dt[,"Fuel"] <- as.character(sapply(aeo_dt[,"Fuel"],function(x)subset(fuel_matching,Aeo_type==x)$Own))
  aeo_dt[,"Aeo_case"] <- aeo_case
  aeo_dt <- subset(aeo_dt,Fuel%in%fuel_matching$Own)
  #Output
  out_aeo_dt <- rbind(out_aeo_dt,aeo_dt)
}
#Convert values in the model units 
out_aeo_dt$Value <- sapply(1:nrow(out_aeo_dt),function(x)out_aeo_dt[x,"Value"]*conv[gsub("2018 $/","",out_aeo_dt[x,"Unit"],fixed = TRUE),"1 J"]*subset(fuel_conv,Data=="Conversion factor" & Fuel==out_aeo_dt[x,"Fuel"])$value)
out_aeo_dt$Unit <- sapply(1:nrow(out_aeo_dt),function(x)paste0("2018 $/",subset(fuel_matching,Own==out_aeo_dt[x,"Fuel"])$Unit))

write.csv(out_aeo_dt,"inputs/model/fuel_price_aeo.csv", row.names = FALSE)


