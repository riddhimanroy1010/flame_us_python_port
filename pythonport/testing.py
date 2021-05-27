''' py file for testing kek '''

import pandas as pd
import openpyxl

vh_techno                          = pd.read_csv("inputs/data_input_management.csv")

degra_fac                           = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fc_degra_factor_vision', 'File'].array[0])

technology = 'ICEV-G'
size = 'Car'
fuel_type = 'Gasoline'

degra_fc = (degra_fac.loc[(degra_fac["Technology"] == technology) & (degra_fac["Size"] == size), 'Degradation factor']).array[0]

fc_ev_hist_fc                       = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fc_ev_hist', 'File'].array[0])      
            #Get the historical values
tmp_mat_hist_fc                     = fc_ev_hist_fc[(fc_ev_hist_fc["Year"] > 2011) & (fc_ev_hist_fc["Size"] == size) & (fc_ev_hist_fc["Technology"] == "BEV100") & (fc_ev_hist_fc["Model"] == "Sales weighted")]

print(degra_fc)
print(fc_ev_hist_fc)
print(tmp_mat_hist_fc)