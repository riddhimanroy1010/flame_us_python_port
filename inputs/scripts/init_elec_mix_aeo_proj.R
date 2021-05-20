###>Creates prospective electricity mixes from AEO projections.
source("inputs/scripts/api_eia_data.R")
#Load model framework functions
f_list <- list.files(path = "utils", pattern = "_f.R",full.names = TRUE)
for (i in 1:length(f_list)) {
  source(f_list[[i]])
}
remove(list=c("i","f_list"))
load_input_data_f()

#Inputs
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year <- 2021
us_elec_mix <- NULL
us_census_elec_mix <- NULL
aeo_data_raw <- NULL

for (aeo_case in aeo_case_list){
  elec_gen <- getAEOElecGen(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Remove some unnecessary sources
  new_elec_gen <- subset(elec_gen,!Source%in%c("Total Generation","Sales to Customers","Generation for Own Use"))
  #Calculate national mix
  if ("United States" %in% elec_gen$Region){
    elec_gen_us <- subset(new_elec_gen,Region=="United States")
    elec_gen_us[,"Mix"] <- sapply(1:nrow(elec_gen_us),function(x)elec_gen_us[x,"Value"]/sum(subset(elec_gen_us,Year==elec_gen_us[x,"Year"])$Value))
    new_elec_gen <- subset(new_elec_gen,Region!="United States")
  } else {
    agg.formula <- reformulate(termlabels = setdiff(colnames(new_elec_gen),c("Region","Value")),response = "Value")
    elec_gen_us <- aggregate(data = new_elec_gen,agg.formula,FUN=sum)
    elec_gen_us[,"Mix"] <- sapply(1:nrow(elec_gen_us),function(x)elec_gen_us[x,"Value"]/sum(subset(elec_gen_us,Year==elec_gen_us[x,"Year"])$Value))
    elec_gen_us["Region"] <- "United States"
  }
  #convert regions into NERC
  new_elec_gen[,"Region"] <- get_matching_names(new_elec_gen[,"Region"],"geo","NERC_nems","NERC_subregion")
  agg.formula <- reformulate(termlabels = setdiff(colnames(new_elec_gen),c("Value")),response = "Value")
  new_elec_gen <- aggregate(data = new_elec_gen,agg.formula,FUN=sum)
  new_elec_gen[,"Mix"] <- sapply(1:nrow(new_elec_gen),function(x)new_elec_gen[x,"Value"]/sum(subset(new_elec_gen,Year==new_elec_gen[x,"Year"] & Region==new_elec_gen[x,"Region"])$Value))
  #Add national mix
  aeo_data_raw <- rbind(aeo_data_raw,rbind(new_elec_gen,elec_gen_us))
}
#Write raw data
write.csv(aeo_data_raw,"inputs/model/electricity_generation_aeo_raw.csv", row.names = FALSE)

#Write mix data
out_elec_mix <- subset(aeo_data_raw,Source!="Total")[,c("Year","Source","Region","Aeo_year","Aeo_case","Mix")]
#Format
out_elec_mix[,"Source"] <- get_matching_names(original_values=out_elec_mix[,"Source"],
                                              matching_type="electricity",
                                              original_source="AEO",
                                              matched_source="Own")
out_elec_mix <- aggregate(data=out_elec_mix,reformulate(termlabels = setdiff(colnames(out_elec_mix),c("Mix")),response = "Mix"),FUN=sum)

write.csv(out_elec_mix,"inputs/model/us_elec_mix_proj.csv",row.names = FALSE)

# #Write census data: DOES NOT WORK
# us_census_elec_mix <- subset(aeo_data_raw, Source!="Total")
# us_census_elec_mix[,"Census"] <- get_matching_names(original_values=us_census_elec_mix[,"Region"],
#                                                     matching_type="geo",
#                                                     original_source="NERC_nems",
#                                                     matched_source="Census_region")
# us_census_elec_mix[,"Source"] <- get_matching_names(original_values=us_census_elec_mix[,"Source"],
#                                               matching_type="electricity",
#                                               original_source="AEO",
#                                               matched_source="Own")
# us_census_elec_mix <- aggregate(data=us_census_elec_mix,reformulate(termlabels = setdiff(colnames(us_census_elec_mix),c("Value","Region","Unit")),response = "Value"),FUN=sum)
# us_census_elec_mix[,"rel_Value"] <- sapply(1:nrow(us_census_elec_mix),function(x) us_census_elec_mix[x,"Value"]/sum(subset(us_census_elec_mix,Year==us_census_elec_mix[x,"Year"] & Census==us_census_elec_mix[x,"Census"])[,"Value"]))
# us_census_elec_mix$Value <- NULL
# colnames(us_census_elec_mix)[colnames(us_census_elec_mix)=="rel_Value"] <- "Value"
# write.csv(us_census_elec_mix,"inputs/model/us_elec_mix_reg_proj.csv",row.names = FALSE)

