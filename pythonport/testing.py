''' py file for testing kek '''

import pandas as pd
import openpyxl

vh_techno                          = pd.read_csv("inputs/data_input_management.csv")

degra_fac                           = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fc_degra_factor_vision', 'File'].array[0])

technology = 'ICEV-G'
size = 'Car'
fuel_type = 'Gasoline'

mask = degra_fac["Technology"] == technology, degra_fac["Size"] == size, degra_fac['Fuel type'] == fuel_type,
degra_fc = (degra_fac.loc[(degra_fac["Technology"] == technology) & (degra_fac["Size"] == size), 'Degradation factor']).array[0]

print(degra_fc)