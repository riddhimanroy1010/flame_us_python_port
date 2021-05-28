''' py file for testing kek '''

import pandas as pd
import openpyxl
import re

vh_techno                          = pd.read_csv("inputs/data_input_management.csv")
conv                                = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'conversion_units', 'File'].array[0])

print(conv)
print(conv['Unnamed: 0'])
conv = conv.set_index(conv['Unnamed: 0'])
conv.index.names = [None]
del conv['Unnamed: 0']
print(conv)
print(conv.loc["mile", "1 km"])

'''

degra_fac                           = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fc_degra_factor_vision', 'File'].array[0])
fe_vision                           = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'vision_fe_hist', 'File'].array[0])

print(fe_vision)
vision_techno = ["Gasoline ICE", "EV100", "CNG", "SI PHEV 40"]
tmp_mat_hist_fc                 = fe_vision[(fe_vision["Size"] == "Car") & (fe_vision["Data"] == "Unadjusted Fuel Economy") & (fe_vision["Technology"].isin(vision_techno))]
print(tmp_mat_hist_fc)
fuel_type = ["Gasoline", "Electricity", "CNG & LPG"]
for col in tmp_mat_hist_fc.columns:
    print(re.findall('[0-9]+', col))
    if re.findall('[0-9]+', col) != []:
        if int(re.findall('[0-9]+', col)[0]) < 1984:
            del tmp_mat_hist_fc[col]
            

print(tmp_mat_hist_fc)
tmp_mat_hist_fc = tmp_mat_hist_fc * 2
'''
'''
technology = 'ICEV-G'
size = 'Car'
fuel_type = 'Gasoline'
list = ["BEV100", "PHEV20"]


degra_fc = (degra_fac.loc[(degra_fac["Technology"] == technology) & (degra_fac["Size"] == size), 'Degradation factor']).array[0]

fc_ev_hist_fc                       = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fc_ev_hist', 'File'].array[0])      
            #Get the historical values
tmp_mat_hist_fc                     = fc_ev_hist_fc[(fc_ev_hist_fc["Year"] > 2011) & (fc_ev_hist_fc["Size"] == size) & (fc_ev_hist_fc["Technology"].isin(list)) & (fc_ev_hist_fc["Model"] == "Sales weighted")]

temp = pd.DataFrame()

print(degra_fc)
print(fc_ev_hist_fc)
print(tmp_mat_hist_fc)
print(temp)
'''