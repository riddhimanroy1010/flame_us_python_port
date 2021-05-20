library(rgcam)
library(tidyr)
gcam_data <- loadProject('inputs/data/gcam_data.proj')
source("inputs/scripts/gcam_get_data_f.R")
#Outputs
ldv_on_road_stock_dt <- NULL
ldv_energy_consumption_dt <- NULL
ldv_energy_use_dt <- NULL
ef_electricity_dt <- NULL
electricity_mix_dt <- NULL
biomass_fraction_dt <- NULL
ldv_emission_intensity <- NULL
#Loop on all the scenarios and extract data
for (scen_name in names(gcam_data)){
  gcam_data_list <- gcam_data[[scen_name]]
  #Get data
  ldv_on_road_stock_dt <- rbind(ldv_on_road_stock_dt,get_on_road_stock(gcam_data_list))
  ldv_energy_consumption_dt <- rbind(ldv_energy_consumption_dt,get_ldv_energy_consumption(gcam_data_list))
  ldv_energy_use_dt <- rbind(ldv_energy_use_dt,get_ldv_energy_use(gcam_data_list))
  electricity_mix_dt <- rbind(electricity_mix_dt,get_mix_electricity(gcam_data_list))
  ef_electricity_dt <- rbind(ef_electricity_dt,get_ef_electricity(gcam_data_list)[["electricity_ef_dt"]])
  biomass_fraction_dt <- rbind(biomass_fraction_dt,get_emission_intensity_refining(gcam_data_list)[["refining_out_dt"]])
  ldv_emission_intensity <- rbind(ldv_emission_intensity,get_emission_intensity_ldv(gcam_data_list))
}
#
write.csv(ldv_on_road_stock_dt,"inputs/model/gcam_ldv_on_road_stock.csv",row.names = FALSE)
write.csv(ldv_emission_intensity,"inputs/model/gcam_ldv_emissions.csv",row.names = FALSE)
write.csv(ldv_energy_consumption_dt,"inputs/model/gcam_ldv_energy_consumption.csv",row.names = FALSE)
write.csv(ldv_energy_use_dt,"inputs/model/gcam_ldv_energy_use.csv",row.names = FALSE)
write.csv(ef_electricity_dt,"inputs/model/gcam_ef_electricity.csv",row.names = FALSE)
write.csv(electricity_mix_dt,"inputs/model/gcam_mix_electricity.csv",row.names = FALSE)
write.csv(biomass_fraction_dt,"inputs/model/gcam_biomass_fraction.csv",row.names = FALSE)

