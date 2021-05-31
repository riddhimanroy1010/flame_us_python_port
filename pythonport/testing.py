''' py file for testing kek '''
from dataclasses import dataclass, field
from numpy import NaN, mat, newaxis
import pandas as pd
import openpyxl
import re
import pythonport as FLAME


from pandas.core.indexes.range import RangeIndex

vh_techno                          = pd.read_csv("inputs/data_input_management.csv")


'''
conv                                = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'conversion_units', 'File'].array[0])

print(conv)
print(conv['Unnamed: 0'])
conv = conv.set_index(conv['Unnamed: 0'])
conv.index.names = [None]
del conv['Unnamed: 0']
print(conv)
print(conv.loc["mile", "1 km"])

@dataclass
class test:
     hello: str = field(default_factory=str)

     def __init__(self) -> None:
         print("bye")

A = test()
A.hello = "hello"
print(A.hello)
 
test = ["a", "b"]

test_dt = pd.DataFrame(index = test, columns= RangeIndex(1, 10))

print(test_dt)
hist_mc                                 = pd.read_csv(vh_techno.loc[vh_techno['Variable_name'] == 'fleet_mt_comp_hist', 'File'].array[0])
mat_hist_mc                             = hist_mc[(hist_mc["Technology"] == "ICEV-G") & (hist_mc["Size"] == "Car") & (hist_mc["Model_year"] >= 1975) & (hist_mc["Material"] != "Total")]
print("mat_hist_mc")
print(mat_hist_mc)
new = pd.DataFrame(index=pd.unique(mat_hist_mc["Material"]), columns=pd.unique(mat_hist_mc["Model_year"]))
print(vh_techno.loc[vh_techno["Variable_name"] == "model_matching_technology"]["File"])
material_dt                             = pd.read_excel((vh_techno.loc[(vh_techno["Variable_name"] == "model_matching_material")]["File"]).array[0], (vh_techno.loc[(vh_techno["Variable_name"] == "model_matching_material")]["Sheet_name"]).array[0], engine="openpyxl")
print("material_dt")
print(material_dt)

material_composition               = pd.DataFrame(index=pd.unique(material_dt["Own"]), columns=RangeIndex(1975, 2015))
print("material_composition")
print(material_composition)
print("new")
print(new)

for year in pd.unique(mat_hist_mc["Model_year"]):
    for material in pd.unique(mat_hist_mc["Material"]):
        new[year][material] = (mat_hist_mc.loc[(mat_hist_mc["Model_year"] == year) & (mat_hist_mc["Material"] == material)]["Value"]).array[0]

print(new)
    

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

if pd.isnull(tmp_mat_hist_fc).values.any():
    na_cols = [cols for cols in tmp_mat_hist_fc.columns if pd.isna(tmp_mat_hist_fc).values.any() if re.findall('[0-9]+', cols) != [] if int(re.findall('[0-9]+', cols)[0]) > 1999]
    print(na_cols)

df = pd.DataFrame([['a.1','b.1','c.1'],['a.2','b.2','c.2'],['a.3','b.3','c.3']],columns=['A','B','C'])

df['A'] = df['B']
print(df)
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