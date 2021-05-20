###>Creates historical electricity mixes.
#Load model framework functions
f_list <- list.files(path = "utils", pattern = "_f.R",full.names = TRUE)
for (i in 1:length(f_list)) {
  source(f_list[[i]])
}
remove(list=c("i","f_list"))
load_input_data_f()

#Inputs from Schivley. Data up to 2017
hist_elec_gen <-read.csv("inputs/data/NERC generation 2018-03-06.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
#Match with own technologies
hist_elec_gen[,"Source"] <- get_matching_names(hist_elec_gen[,"fuel category"],"electricity","Schivley","Own")
#Aggregate by year
an_elec_gen <- aggregate(data=hist_elec_gen,`generation (mwh)` ~ Source + year + nerc,FUN=sum)
#Calculate mixes
an_elec_mix <- an_elec_gen
an_elec_mix[,"Mix"] <- sapply(1:nrow(an_elec_mix),function(x) an_elec_mix[x,"generation (mwh)"]/sum(subset(an_elec_mix,year==an_elec_mix[x,"year"] & nerc==an_elec_mix[x,"nerc"])[,"generation (mwh)"]))
#Format
out_elec_mix <- an_elec_mix[,c("year","nerc","Source","Mix")]
colnames(out_elec_mix)[1] <- "Year"
colnames(out_elec_mix)[2] <- "Region"
#Calculate US mix
us_elec_mix <- aggregate(data=an_elec_gen,`generation (mwh)` ~ Source + year ,FUN=sum)
us_elec_mix[,"Mix"] <- sapply(1:nrow(us_elec_mix),function(x) us_elec_mix[x,"generation (mwh)"]/sum(subset(us_elec_mix,year==an_elec_mix[x,"year"])[,"generation (mwh)"]))
#Format
us_elec_mix <- us_elec_mix[,c("year","Source","Mix")]
colnames(us_elec_mix)[1] <- "Year"
us_elec_mix[,"Region"] <- "United States"
#Combine
out_elec_mix <- rbind(out_elec_mix,us_elec_mix)

#Get historical data for 2018 and 2019
###>Creates prospective electricity mixes from AEO projections.
source("inputs/scripts/api_eia_data.R")
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_data_raw <- NULL
aeo_case = "REF"
for (aeo_year in c(2019,2020)){
  elec_gen <- getAEOElecGen(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Remove some unnecessary sources and years
  new_elec_gen <- subset(elec_gen,!Source%in%c("Total Generation","Total","Sales to Customers","Generation for Own Use") & Year==aeo_year-1)
  
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
  new_elec_gen[,"Region"] <- get_matching_names(new_elec_gen[,"Region"],"geo","NERC_nems","NERC_schivley")
  agg.formula <- reformulate(termlabels = setdiff(colnames(new_elec_gen),c("Value")),response = "Value")
  new_elec_gen <- aggregate(data = new_elec_gen,agg.formula,FUN=sum)
  new_elec_gen[,"Mix"] <- sapply(1:nrow(new_elec_gen),function(x)new_elec_gen[x,"Value"]/sum(subset(new_elec_gen,Year==new_elec_gen[x,"Year"] & Region==new_elec_gen[x,"Region"])$Value))
  #Add national mix
  aeo_data_raw <- rbind(aeo_data_raw,rbind(new_elec_gen,elec_gen_us))
}
#Convert into sources
aeo_elec_mix <- subset(aeo_data_raw,Source!="Total")[,c("Year","Source","Region","Mix")]
#Format
aeo_elec_mix[,"Source"] <- get_matching_names(original_values=aeo_elec_mix[,"Source"],
                                              matching_type="electricity",
                                              original_source="AEO",
                                              matched_source="Own")
aeo_elec_mix <- aggregate(data=aeo_elec_mix,reformulate(termlabels = setdiff(colnames(aeo_elec_mix),c("Mix")),response = "Mix"),FUN=sum)

#Combine
out_elec_mix <- rbind(out_elec_mix,aeo_elec_mix)

write.csv(out_elec_mix,"inputs/model/us_elec_mix_hist.csv",row.names = FALSE)

