''' py file for testing kek '''
from dataclasses import dataclass, field
import numpy as np
import pandas as pd
import re
import time
import pyFLAME as FLAME
from pandas.core.indexes.range import RangeIndex

FLAME.utils.shelf_destroy()
print(FLAME.utils.get_input("vehicle_specifications"))
vehicle_specs_dt = FLAME.utils.get_input("vehicle_specifications")
tmp_techno = 'HEV'
size = 'Car'
tech_ok = []
for index, rows in vehicle_specs_dt.iterrows():
    try:
        if tmp_techno in vehicle_specs_dt.at[index, 'Technology'].split(','):
            print(vehicle_specs_dt.at[index, 'Technology'].split(','))
            print("found in row" + str(index))
            tech_ok.append(index)           
    except ValueError:
        print(rows)
        continue
vehicle_specs_tmp                       = vehicle_specs_dt.loc[((tmp_techno in vehicle_specs_dt['Technology'].str.split(',')) | (vehicle_specs_dt['Technology'] == 'Glo') & ((vehicle_specs_dt['Size'] == size)) | (vehicle_specs_dt['Size'] == 'Glo'))]
print(vehicle_specs_tmp)
size = ['Car', 'Glo']
print(FLAME.utils.get_input("vehicle_specifications_dyn"))
specs_list                         = []
for items in vehicle_specs_tmp['Parameter']:
    specs_list.append(items)
vehicle_specs_dyn                   = FLAME.utils.get_input('vehicle_specifications_dyn') 
specifications = pd.DataFrame(index= specs_list, columns=RangeIndex(2015, 2051))
vehicle_specs = vehicle_specs_tmp
years = [years for years in range(2015, 2051)]
for par in pd.unique(vehicle_specs['Parameter']):
    dyn_indices = vehicle_specs_dyn.index[vehicle_specs_dyn['Parameter'] == par].tolist()
    norm_indices = vehicle_specs.index[vehicle_specs['Parameter'] == par].tolist()
    print("********************")
    for index in norm_indices: 
        if vehicle_specs.loc[index, 'Constant'] == 'n' and vehicle_specs.loc[index, 'Size'] in size:
            print(par)
            if pd.isna(vehicle_specs.loc[index, 'Value']):
            #print(dyn_indices[norm_indices.index(index)])
            #specifications = specifications.append(vehicle_specs_dyn.loc[dyn_indices.index(norm_indices[index])].to_dict(), ignore_index=True)\
            # print("********************")
            # print(vehicle_specs.loc[index, 'Constant'], vehicle_specs.loc[index, 'Value'], np.nan)
            # print("dyn_indices", dyn_indices)
            # print("norm_indices", norm_indices)
            # print("dyn_index_correct", dyn_indices[norm_indices.index(index)])
                line = vehicle_specs_dyn.loc[dyn_indices[norm_indices.index(index)]]
                for key in line.index:
                    try:
                        if len(re.findall('[0-9]+',key)) > 0:
                            print(key, line.loc[key])
                            print(type(key))
                            specifications.loc[par, int(key)] = line.loc[key]
                    except ValueError:
                        continue
                    # if keys in years:
                    #     print(keys)
                print("********************")
            #specifications = specifications.append(other = {k: v for k, v in vehicle_specs_dyn.loc[dyn_indices[norm_indices.index(index)]].to_dict().items() if k in years}, ignore_index=True) 
print(specifications)
'''
# */ timing block */ #
start = time.time()
conversion = FLAME.utils.get_input("conversion_units")
vh_techno = FLAME.utils.get_input("fc_degra_factor_vision")
fleet_comp = FLAME.utils.get_input("fleet_mt_comp_hist")
fuel_conv = FLAME.utils.get_input("fuel_conversion")
vision = FLAME.utils.get_input("vision_fe_hist")
excel = FLAME.get_input("model_matching_technology")
excel2 = FLAME.get_input("model_matching_material")
end = time.time()

print("time taken:", end - start)
# */ timing block */ #

hist_stock_dt                           = FLAME.utils.get_input("fleet_stock_hist")
print(hist_stock_dt)
print(hist_stock_dt["Year"])
new = pd.DataFrame(columns= RangeIndex(min(hist_stock_dt["Year"]), max(hist_stock_dt["Year"]) + 1))
new.insert(0, "Technology", pd.unique(hist_stock_dt["Technology"]))
new2 = new.append(new[:], ignore_index=True)
new2.insert(0, 'Size', pd.unique(hist_stock_dt["Size"])[0])
for index, rows in new2.iterrows():
    if index >= len(new2.index)//2:
        new2['Size'][index] = 'Light truck' 

print(new2)  
for year in pd.unique(hist_stock_dt["Year"]):
    for tech in pd.unique(hist_stock_dt["Technology"]):
        for size in pd.unique(hist_stock_dt["Size"]):
            try:
                value = (hist_stock_dt.loc[(hist_stock_dt["Year"] == year) & (hist_stock_dt["Technology"] == tech) & (hist_stock_dt["Size"] == size)]["Value"]).array[0]
                index = (new2[(new2['Size'] == size) & (new2['Technology'] == tech)].index)
                new2.at[index, year] = value
                # (new2.loc[(new2['Size'] == size) & (new2['Technology'] == tech)][year]).array[0] = value
                # print(value)
                # print((new2.loc[(new2['Size'] == size) & (new2['Technology'] == tech)][year]).array[0])
                # print((new2.loc[(new2['Size'] == size) & (new2['Technology'] == tech)][year]).shape)
            except IndexError:
                print("error at ", year, tech, size)
                pass
        # try:
        #(hist_stock_dt.loc[(hist_stock_dt["Year"] == year) & (hist_stock_dt["Technology"] == tech) & (hist_stock_dt["Size"] == size)]["Value"]).array[0]
        #     print((hist_stock_dt.loc[(hist_stock_dt["Year"] == year) & (hist_stock_dt["Technology"] == tech)]["Value"]).array[0])
        #     new[year][tech] = (hist_stock_dt.loc[(hist_stock_dt["Year"] == year) & (hist_stock_dt["Technology"] == tech)]["Value"]).array[0]
        # except Exception:
        #     pass

print(new2)

new3 = pd.DataFrame(index=['Total'], columns=RangeIndex(1970, 2020 + 1))
print(new3)
for cols in new3.columns:
    new3[cols] = new2[cols].sum()
print(new3)
newx = new2
del newx['Size']
del newx['Technology']
new_mat1 = newx.to_numpy()
new_mat2 = new3.columns.to_numpy()
print("new_mat1")
print(new_mat1)
print("#############")
print("new_mat2")
print(1/new_mat2)
print("#############")
print("new_mat2 diag")
print(np.diag(1/new_mat2))
print("#############")
print("final prod")
print(np.matmul(new_mat1, np.diag(1/new_mat2)))
'''
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
'''
'''
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