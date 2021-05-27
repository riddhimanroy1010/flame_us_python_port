from dataclasses import dataclass, field
import numpy as np
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

class vehicleClass():
    '''
    Data fields:
        technology                          :            type str
        size                                :            type str
        fuel_type                           :            type str
        fuel_consumption                    :            type pandas.core.frame.DataFrame
        utility_factor                      :            type pandas.core.frame.DataFrame
        specifications                      :            type numpy.matrix
        battery_type                        :            type str
        material_composition                :            type numpy.matrix
        material_component_composition      :            type pandas.core.frame.DataFrame
    Functions:
        __init__: initializes the function and its fields
        get_data_frame: converts the existing fields into dataframes and returns dataframe
    '''
    technology:                                 str                         = field(default_factory=str)
    size:                                       str                         = field(default_factory=str)
    fuel_type:                                  list                        = field(default_factory=list)
    fuel_consumption:                           pd.DataFrame                = field(default_factory=pd.DataFrame)
    utility_factor:                             pd.DataFrame                = field(default_factory=pd.DataFrame)
    specifications:                             np.matrix                   = field(default_factory=np.matrix)
    battery_type:                               str                         = field(default_factory=str)
    material_composition:                       np.matrix                   = field(default=np.matrix)
    material_component_composition:             pd.DataFrame                = field(default_factory=pd.DataFrame)

    '''
    Initializing the vehicle class with get_input to get data from environment class.
    This is called when a vehicle object is made. 
    This __init__ function incorporated vehicle_initialize_f.r into the class itself, reflecting python best practices.
    '''
    def __init__(self, technology, size, first_yr = None, last_yr = None, BEV_bat_t = None, PHEV_bat_t = None, HEV_bat_t = None) -> None:
        self.vh_techno                          = pd.read_csv("inputs/data_input_management.csv")

        self.technology                         = technology                                                            
        self.size                               = size

        ''' bat_type dict
            Depending on the type of technology inputed, battery type is accordingly updated. 
            To add more battery types, update the dict mappings. 
            None case does not need to be included.
        '''
        bat_type                                = {"HEV": HEV_bat_t,                      
                                                   "BEV100" : BEV_bat_t,
                                                   "BEV300" : BEV_bat_t,
                                                   "PHEV20" : PHEV_bat_t,
                                                   "PHEV40" : PHEV_bat_t,}
        self.battery_type                       = bat_type.get(technology, None)
        
        #historical fuel consumption
        self.vehicle_hist_fc_f(self)

        #updating material composition and material component composition
        self.vehicle_hist_material_composition_f(self)
        
        #utility factor
        first_hist_yr                           = float(np.min(self.fuel_consumption[:, 'Year']))
        last_hist_yr                            = float(pd.max(self.material_component_composition(level = "Model_year")))
        self.utility_factor                     = pd.DataFrame(index = self.fuel_type, columns = self.fuel_consumption[0])
        self.utility_factor.fillna(None)
        self.vehicle_utility_factor_f(self, last_hist_yr)
           
    def vehicle_hist_fc_f(self, first_yr = None, last_yr = None, fc_ev_mdl = None, fc_conv_mdl = None):
        ## Configure environment ##

        age_tbc                                 = 30
        first__hist_yr                          = first_yr - age_tbc
        last_hist_yr                            = 2019
        self.fuel_consumption                   = pd.DataFrame(index = self.fuel_type, columns = RangeIndex(first__hist_yr, last_yr))
        tmp_mat_hist_fc                         = pd.DataFrame()
        if self.technology == "ICEV-G" or "ICEV-D":
            epa_fc                              = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'epa_fleet_fc_hist', 'File'].array[0])    
            tmp_mat_hist_fc                     = epa_fc[(epa_fc["Model_year"] > first__hist_yr) & (epa_fc["Size"] == self.size) & (epa_fc["Technology"] == self.technology) & (epa_fc["Fuel_type"] == self.fuel_type)] 
            #Add degradation factors
            def_fac_matr                        = {'def' : 1,
                                                   'low' : 0.9,
                                                   'high' : 1.1}
            def_fac                             = def_fac_matr.get(fc_conv_mdl)
            tmp_mat_hist_fc                     = tmp_mat_hist_fc * def_fac
        elif self.technology == "BEV100" or "BEV100" or "PHEV20" or "PHEV40":
            fc_ev_hist_fc                       = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'fc_ev_hist', 'File'].array[0])      
            #Get the historical values
            tmp_mat_hist_fc                     = fc_ev_hist_fc[(fc_ev_hist_fc["Year"] > first__hist_yr) & (fc_ev_hist_fc["Size"] == self.size) & (fc_ev_hist_fc["Technology"] == self.technology) & (fc_ev_hist_fc["Model"] == "Saled weighted") | (fc_ev_hist_fc["Model"] == fc_ev_mdl)]         
            #Add battery charging efficiency and transmission losses
            tmp_mat_hist_fc['Electricity']      = tmp_mat_hist_fc['Electricity'] / (0.90*0.95)
        else:
            #Inputs
            fe_vision                           = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'vision_fe_hist', 'File'].array[0])
            degra_fac                           = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'fc_degra_factor_vision', 'File'].array[0])
            vh_techno                           = pd.read_excel(self.vh_techno.loc['model_matching_technology', 'File'], self.vh_techno.loc['model_matching_technology', 'Sheet_name'], engine="openpyxl")
            fuel_conv                           = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'fuel_conversion', 'File'].array[0])
            conv                                = pd.read_csv(self.vh_techno.loc[self.vh_techno['Variable_name'] == 'conversion_units', 'File'].array[0])

            #vision_techno contains the list of equivalent technologies in vision data
            vision_techno                       = (vh_techno.loc(vh_techno['Own'] == self.technology, 'Vision')).array[0].split(';')

            #VISION data are unadjusted and combined. We need to consider the degradation factor provided by VISION.
            deg_fac                             = (degra_fac.loc[(degra_fac["Technology"] == self.technology) & (degra_fac["Size"] == self.size) & (degra_fac["Fuel type"] == self.fuel_type), 'Degradation factor']).array[0]
            
            #fuel_conv_fact is a conversion factor to convert from L equivalent gasoline to L of fuel (or kWh)
            fuel_conv_fact                      = (fuel_conv.loc[(fuel_conv["Fuel"] == 'Gasoline') & (fuel_conv["Data"] == "Conversion Factor")]).array[0] \
                                                  / (fuel_conv.loc[(fuel_conv["Fuel"] == self.fuel_type) & (fuel_conv["Data"] == "Conversion Factor")]).array[0]
            
            #Get data from VISION
            tmp_mat_hist_fc                     = fe_vision[(fe_vision["Size"] == self.size) & (fe_vision["Fuel type"] == self.fuel_type) & (fe_vision["Data"] == "Unadjusted Fuel Economy") & (fe_vision["Technology"].isin(vision_techno))]
            for col in tmp_mat_hist_fc.columns:
                if re.findall('[0-9]+', col) != []:
                    if int(re.findall('[0-9]+', col)[0]) < first__hist_yr:
                        del tmp_mat_hist_fc[col]
            
        self.fuel_consumption                   = tmp_mat_hist_fc
        
                    






        
    '''
    Returns the existing attributes of the vehicle as a dataframe
    '''
    def get_data_frame(self, field_name):

        out                                     = pd.DataFrame()

        if type(vars(self)[str(field_name)]) == 'numpy.matrix':
            temp                                = pd.DataFrame(vars(self)[field_name])
            # cbind(Data=rownames(field_values),stringsAsFactors = FALSE) What does this line do?
            temp.concat(temp, self.technology, self.size)
            out.concat(temp[['Technology', 'Size', 'Model_year', 'Data', 'Value']])
            replace                             = {"fuel_consumption": "Fuel", 
                                                   "utility_factor" : "Fuel", 
                                                   "specifications" : "Attribute", 
                                                   "material_composition" : "Material"}
            out.rename(columns = replace, inplace = True)

        elif type(vars(self)[str(field_name)]) == 'pandas.core.frame.DataFrame':
            out.concat(field_name, self.technology, self.size)
        return out

class fleetClass():
    '''
    Data fields:
        vint_stock: type list
        vint_scrap: type list
        technology_market_share: type numpy.matrix
        fuel_consumption: type numpy.matrix
        ldv_sales: type numpy.matrix
        ldv_on_road_stock: type numpy.matrix
        ldv_on_road_stock_tot: type numpy.matrix
    Functions:
        __init__: initializes the function and its fields
        get_data_frame: converts the existing fields into dataframes and returns pandas.core.frame.DataFrame
        get_list_dataframe: Return all fields into a list of pandas.core.frame.DataFrame
    '''
    vint_stock: list = field(default_factory=list)
    vint_scrap: list = field(default_factory=list)
    technology_market_share: np.matrix = field(default_factory=np.matrx)
    ldv_sales: np.matrix = field(default_factory=np.matrx)
    ldv_on_road_stock: np.matrix = field(default_factory=np.matrx)
    ldv_on_road_stock_tot: np.matrix = field(default_factory=np.matrx)

    '''
    Returns the fields of the class as a pandas.core.frame.DataFrame
    '''
    def get_data_frame(self, field_name):
        out = pd.DataFrame(vars(self)[field_name])
        if type(vars(self)[field_name]) == "numpy.matrix":
            temp = pd.DataFrame(vars(self)[field_name])
            ##same problem: what does the cbind line do?
            temp.concat(temp, self.technology, self.size)
            out.concat(temp[["Year", "Value"]])
            for row, data in out.iterrows():
                str = out['Type'][row]
                val = float(re.findall('_', out['Type'][row]))
                out['Size'][row] = str[:val - 1]
                out['Technology'][row] = str[:val + 1200]
                out['Type'][row] = None
        elif type(vars(self)[field_name]) == list:
            for i in range(len(vars(self)[field_name])):
                tmp_stock_dt = pd.DataFrame(vars(self)[field_name][i])
                tmp_stock_dt.concat(tmp_stock_dt, {'Year' : int(vars(self)[field_name]['year'][i])})
                str = tmp_stock_dt['Type'][i]
                val = float(re.findall('_', tmp_stock_dt['Type'][i]))
                tmp_stock_dt['Size'][i] = str[:val - 1]
                tmp_stock_dt['Technology'][i] = str[:val + 1200]
                tmp_stock_dt['Type'][i] = None

                out = pd.DataFrame()
                out.concat(tmp_stock_dt)
                return out
    '''
    Return all fields into a list of pandas.core.frame.DataFrame
    '''
    def get_list_dataframe(self):
        return [self.get_data_frame('vint_stock'), self.get_data_frame('vint_scrap')]




            



                

            
        

