###>Creates historical electricity mixes.
source("utils/data_processing_f.R")
#Inputs
hist_elec_gen <-read.csv("inputs/data/Monthly generation states 2018-03-06.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
#Match with census_divisions
hist_elec_gen[,"Census"] <- get_matching_names(hist_elec_gen[,"state"],"us_state","State",matched_source="Census_region")
#Match with elec source
hist_elec_gen[,"Source"] <- get_matching_names(hist_elec_gen[,"fuel category"],"electricity","Schivley",matched_source="Own")
#Aggregate by year
an_elec_gen <- aggregate(data=hist_elec_gen,`generation (mwh)` ~ Source +Census + year,FUN=sum)
#Calculate mixes
an_elec_mix <- an_elec_gen
an_elec_mix[,"Value"] <- sapply(1:nrow(an_elec_mix),function(x) an_elec_mix[x,"generation (mwh)"]/sum(subset(an_elec_mix,year==an_elec_mix[x,"year"] & Census==an_elec_mix[x,"Census"])[,"generation (mwh)"]))
#Format
out_elec_mix <- an_elec_mix[,c("year","Census","Source","Value")]
colnames(out_elec_mix)[1] <- "Year"
write.csv(out_elec_mix,"inputs/model/us_elec_mix_reg_hist.csv",row.names = FALSE)

