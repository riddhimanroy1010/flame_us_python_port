#EPA Hist emissions
epa_dts <- read.csv("inputs/data/epa_hist_ldv_emissions.csv",stringsAsFactors = FALSE)
mat_epa_hist <- acast(data=epa_dts, Unit ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Get target data
lr_dts <- read.csv("inputs/user/lit_rev_us_ldv_co2_budget.csv",stringsAsFactors = FALSE)
mat_emission <- matrix(NA,nrow = length(unique(lr_dts$Source)),ncol = length(1990:2050),dimnames = list(unique(lr_dts$Source),1990:2050))
for(source in rownames(mat_emission)){
  #
  mat_emission[source,colnames(mat_epa_hist)] <- mat_epa_hist
  for (target in unique(subset(lr_dts,Source==source)$Target)){
    mat_emission[source,as.character(subset(lr_dts,Source==source & Target==target)$Target_year)] <- mat_epa_hist[1,as.character(subset(lr_dts,Source==source & Target==target)$Ref_year)]*(1-subset(lr_dts,Source==source & Target==target)$Reduction_target)
  }
  #Linear regressions
  mat_emission[source,] <- approx(x=as.numeric(colnames(mat_emission)),y=mat_emission[source,],method="linear",xout=as.numeric(colnames(mat_emission)))$y
}
#
rowSums(mat_emission[,as.character(2015:2050)])/1000
#Output
fleet_emission_target <- as.data.frame(mat_emission[,as.character(2015:2050)]) %>% 
  cbind(Source=rownames(mat_emission),Unit="Mt CO2",stringsAsFactors = FALSE) %>%
  subset(.,Year%in%c(first_data_yr:last_yr))
fleet_emission_target <- as.data.frame(mat_emission[,as.character(2015:2050)]) %>% 
  cbind(Source=rownames(mat_emission),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Source,convert=TRUE) %>% 
  cbind(Unit="Mt CO2",stringsAsFactors = FALSE)
write.csv(fleet_emission_target,"inputs/model/lit_rev_ldv_co2_budget.csv",row.names = FALSE)

