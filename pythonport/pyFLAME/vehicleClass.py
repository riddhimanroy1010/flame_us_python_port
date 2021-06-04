from dataclasses import dataclass, field
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

import utils

@dataclass
class vehicleClass():
    '''
    Data fields:
        technology                          :            type str
        size                                :            type str
        fuel_type                           :            type list
        fuel_consumption                    :            type pandas.core.frame.DataFrame
        utility_factor                      :            type pandas.core.frame.DataFrame
        specifications                      :            type pandas.core.frame.DataFrame
        battery_type                        :            type str
        material_composition                :            type pandas.core.frame.DataFrame
        material_component_composition      :            type pandas.core.frame.DataFrame
    Functions:
        __init__                            :            initializes the function and its fields
        vehicle_hist_fc_f                   :            initializes historical values for fuel consumption for the vehicle
        vehicle_utility_factor_f            :            initializes the utility factor for the vehicle
        get_data_frame                      :            combines all possible dataframes into one dataframe
    '''
    technology:                                 str                         = field(default_factory=str)
    size:                                       str                         = field(default_factory=str)
    fuel_type:                                  list                        = field(default_factory=list)
    fuel_consumption:                           pd.DataFrame                = field(default_factory=pd.DataFrame)
    utility_factor:                             pd.DataFrame                = field(default_factory=pd.DataFrame)
    specifications:                             pd.DataFrame                = field(default_factory=pd.DataFrame)
    battery_type:                               str                         = field(default_factory=str)
    material_composition:                       pd.DataFrame                = field(default_factory=pd.DataFrame)
    material_component_composition:             pd.DataFrame                = field(default_factory=pd.DataFrame)

    '''
    __init__
        Initializing the vehicle class with get_input to get data from environment class.
        This is called when a vehicle object is made. 
        This __init__ function initializes other aspects of the vehicle class. 
        It also initializes the environment if there is none #TODO
        It stores the main data-management CSV in the environment such that it can be used for future 
        reference. This is done for processing optimization since indexing is faster then creation
        and storage.
    '''
    def __init__(self, technology, size, first_yr = None, last_yr = None, BEV_bat_t = None, PHEV_bat_t = None, HEV_bat_t = None) -> None:
        self.technology                         = technology                                                            
        self.size                               = size

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

    '''
    vehicle_hist_fc_f
        Sets a vehicle's historical fuel consumption.
        Initializes (if non-existent in the inputs environment) multiple CSVs for 
        future indexing. 
    '''
           
    def vehicle_hist_fc_f(self, first_yr = None, last_yr = None, fc_ev_mdl = None, fc_conv_mdl = None):
        ## Configure environment ##

        age_tbc                                 = 30
        first__hist_yr                          = first_yr - age_tbc
        last_hist_yr                            = 2019
        self.fuel_consumption                   = pd.DataFrame(index = self.fuel_type, columns = RangeIndex(first__hist_yr, last_yr + 1))
        tmp_mat_hist_fc                         = pd.DataFrame()
        if self.technology == "ICEV-G" or "ICEV-D":
            epa_fc                              = utils.get_input("epa_fleet_fc_hist")   
            tmp_mat_hist_fc                     = epa_fc[(epa_fc["Model_year"] > first__hist_yr) & (epa_fc["Size"] == self.size) & (epa_fc["Technology"] == self.technology) & (epa_fc["Fuel_type"] == self.fuel_type)] 
            #Add degradation factors
            def_fac_matr                        = {'def' : 1,
                                                   'low' : 0.9,
                                                   'high' : 1.1}
            def_fac                             = def_fac_matr.get(fc_conv_mdl)
            tmp_mat_hist_fc                     = tmp_mat_hist_fc * def_fac
        elif self.technology == "BEV100" or "BEV100" or "PHEV20" or "PHEV40":
            fc_ev_hist_fc                       = utils.get_input("fc_ev_hist")     
            #Get the historical values
            tmp_mat_hist_fc                     = fc_ev_hist_fc[(fc_ev_hist_fc["Year"] > first__hist_yr) & (fc_ev_hist_fc["Size"] == self.size) & (fc_ev_hist_fc["Technology"] == self.technology) & (fc_ev_hist_fc["Model"] == "Saled weighted") | (fc_ev_hist_fc["Model"] == fc_ev_mdl)]         
            #Add battery charging efficiency and transmission losses
            tmp_mat_hist_fc['Electricity']      = tmp_mat_hist_fc['Electricity'] / (0.90*0.95)
        else:
            #Inputs
            fe_vision                           = utils.get_input("vision_fe_hist")
            degra_fac                           = utils.get_input("fc_degra_factor_vision'")
            vh_techno                           = utils.get_input("model_matching_technology")
            fuel_conv                           = utils.get_input("fuel_conversion")
            conv                                = utils.get_input("conversion_units")
            conv                                = conv.set_index(conv['Unnamed: 0'])
            conv.index.names                    = [None]
            del conv['Unnamed: 0']

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
                    else:
                        tmp_mat_hist_fc[col]    = 1 / ((tmp_mat_hist_fc[col]  * deg_fac) * conv.loc["L", "1 gal"] * conv.loc["mile", "1 km"] * 100 * fuel_conv_fact)  
            
        self.fuel_consumption                   = tmp_mat_hist_fc
        if self.fuel_consumption.isnull().values.any():
            na_cols                             = [cols for cols in self.fuel_consumption.columns if pd.isnull(self.fuel_consumption).values.any() if re.findall('[0-9]+', cols) != []]
            if max(na_cols) < first_yr:
                col_to_cpy                      = max(na_cols) + 1
                for col in na_cols:
                    self.fuel_consumption[col]  = self.fuel_consumption[col_to_cpy]
            elif max(na_cols) < last_hist_yr:
                col_to_cpy                      = min(na_cols) - 1
                for col in na_cols:
                    self.fuel_consumption[col]  = self.fuel_consumption[col_to_cpy]

        
    '''
    vehicle_utility_factor_f
        Initializes and sets a vehicle's utility factor based on fuel
    '''
    def vehicle_utility_factor_f(self, model_year):
        conv                                    = utils.get_input("conversion_units")
        conv                                    = conv.set_index(conv['Unnamed: 0'])
        conv.index.names                        = [None]
        del conv['Unnamed: 0']

        def uf_f(range):
            range_miles                         = conv.loc["mile", "1 km"]
            uf                                  = (7.73 * math.pow(10, -9) * math.pow(range_miles, 4)) + (2.63 * math.pow(10, -6) * math.pow(range_miles, 3)) -   \
                                                  (3.7 * math.pow(10, -4) * math.pow(range_miles, 2)) + (2.66 * math.pow(10, -2) * range_miles)

            return uf 
        
        if len(self.fuel_type) == 1:
            self.utility_factor                 = 1

        else:
            vehicle_range                       = self.specifications.loc["range", str(model_year)]
            for fuel in self.fuel_type:
                if fuel == "Electricity":
                    self.utility_factor.loc["Electricity", str(model_year)]\
                                                = uf_f(vehicle_range)
                else:
                    self.utility_factor.loc[fuel, str(model_year)]\
                                                = 1 - uf_f(vehicle_range)

    def vehicle_hist_material_composition_f(self, first_yr = None, last_yr = None):
        #Parameter setup
        age_tbc                                 = 30
        first_hist_yr                           = first_yr - age_tbc
        last_hist_yr                            = max(float(self.fuel_consumption.columns))

        #Inputs
        material_dt                             = utils.get_input("model_matching_material")
        hist_mc                                 = utils.get_input("fleet_mt_comp_hist")

        #Create dt of material composition with accurate fields
        self.material_composition               = pd.DataFrame(index=pd.unique(material_dt["Own"]), columns=RangeIndex(first_hist_yr, last_yr + 1))
        self.material_composition.fillna(None)

        #Update historical material composition
        mat_hist_mc                             = hist_mc[(hist_mc["Technology"] == self.technology) & (hist_mc["Size"] == self.size) & (hist_mc["Model_year"] >= first_hist_yr) & (hist_mc["Material"] != "Total")]
        for year in pd.unique(mat_hist_mc["Model_year"]):
            for material in pd.unique(mat_hist_mc["Material"]):
                self.material_composition[year][material] \
                                                = (mat_hist_mc.loc[(mat_hist_mc["Model_year"] == year) & (mat_hist_mc["Material"] == material)]["Value"]).array[0]
        
        #Functions: Calculate material composition by subcomponent -> to be implemented in fleet class *discuss with prof*

        #The rest of this function needs to be dealt with in fleetClass.

      
    '''
    Returns the existing attributes of the vehicle as a dataframe
    '''
    def get_data_frame(self, field_name):

        out                                     = pd.DataFrame()

        if type(vars(self)[str(field_name)]) == 'pandas.core.frame.DataFrame':
            out.concat(field_name, self.technology, self.size)

        return out


            



                

            
        
