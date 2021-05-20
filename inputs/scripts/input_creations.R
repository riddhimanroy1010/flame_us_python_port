#Script that generates the model inputs from data
#Generate the historical curb weight of light-duty vehicles from EPA data
source("inputs/scripts/init_epa_wgt.R")
remove(list=ls())
#Generate the adjusted component material composition file
source("inputs/scripts/init_adj_component_mat_composition.R")
remove(list=ls())
#Generate the historical material composition of the fleet
source("inputs/scripts/init_fleet_mat_composition_hist.R")
remove(list=ls())
#Generate the historical sales-weighted fuel consumption of light-duty vehicles from EPA data
source("inputs/scripts/init_epa_fc.R")
remove(list=ls())
#Generate the historical stock and sales data from AEO the AEO download
source("inputs/scripts/init_st&sl_aeo_hist_files.R")
remove(list=ls())
#Generate the historical stock and sales data from AEO the online AEO projections
source("inputs/scripts/init_st&sl_aeo_hist_api.R")
remove(list=ls())
#Generate the historical stock and sales data from AEO and vISION
source("inputs/scripts/init_st&sl_hist.R")
remove(list=ls())
#Generate the projected stock and sales data from AEO projections
source("inputs/scripts/init_st&sl_aeo_proj.R")
remove(list=ls())
#Generate the historical sales data by region from AEO
source("inputs/scripts/init_reg_sl_aeo_hist_api.R")
remove(list=ls())
#Generate the projected sales data by region from AEO
source("inputs/scripts/init_reg_sl_aeo_proj.R")
remove(list=ls())
#Generate the historicall electricity generation from AEO projections
source("inputs/scripts/init_elec_mix_aeo_hist.R")
remove(list=ls())
#Generate the projected electricity generation from AEO projections
source("inputs/scripts/init_elec_mix_aeo_proj.R")
remove(list=ls())
#Generate the historical electricity generation from AEO projections by region
source("inputs/scripts/init_elec_mix_reg_hist.R")
remove(list=ls())
#Generate the projected fuel prices from AEO projections
source("inputs/scripts/init_fuel_price_aeo_proj.R")
remove(list=ls())
