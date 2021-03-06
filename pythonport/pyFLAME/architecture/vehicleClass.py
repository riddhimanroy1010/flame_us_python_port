from dataclasses import dataclass, field
from typing import Sized
import numpy as np
import math
import pandas as pd
import re
from pandas.core.indexes.range import RangeIndex

from . import utils
from ..functions import fleet_i_mat_cont_f

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
    '''                                                                                                                         # Initialized?
    technology:                                 str                         = field(default_factory=str)                        # Yes
    size:                                       str                         = field(default_factory=str)                        # Yes
    fuel_type:                                  list                        = field(default_factory=list)                       # Yes                    
    fuel_consumption:                           pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    utility_factor:                             pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    specifications:                             pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    battery_type:                               str                         = field(default_factory=str)                        # Yes
    material_composition:                       pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    material_component_composition:             pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes

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
        
        vh_techno                               = utils.get_input("model_matching_technology")
        self.fuel_type                          = (vh_techno.loc[vh_techno["Own"] == technology]["Fuel type"]).str.split(';').tolist()[0][0]

        self.technology                         = technology                                                            
        self.size                               = size

        bat_type                                = {"HEV": HEV_bat_t,                      
                                                   "BEV100" : BEV_bat_t,
                                                   "BEV300" : BEV_bat_t,
                                                   "PHEV20" : PHEV_bat_t,
                                                   "PHEV40" : PHEV_bat_t,}
        self.battery_type                       = bat_type.get(technology, None)
        
        #historical fuel consumption
        self.vehicle_hist_fc_f(first_yr=2015, last_yr=2050)

        #updating material composition and material component composition\
        self.vehicle_hist_material_composition_f(first_yr=2015, last_yr=2050)
        
        #utility factor
        first_hist_yr                           = float(np.min(self.fuel_consumption[:, 'Year']))
        last_hist_yr                            = float(pd.max(self.material_component_composition(level = "Model_year")))
        self.utility_factor                     = pd.DataFrame(index = self.fuel_type, columns = self.fuel_consumption[0])
        self.utility_factor.fillna(None)
        self.vehicle_utility_factor_f(self, last_hist_yr)

        utils.add_attributes(self)

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
        self.fuel_consumption                   = pd.DataFrame(index = [self.fuel_type], columns = RangeIndex(first__hist_yr, last_yr + 1))
        #tmp_mat_hist_fc                         = pd.DataFrame()
        if self.technology in ["ICEV-G", "ICEV-D"]:
            epa_fc                              = utils.get_input("epa_fleet_fc_hist")   
            tmp_mat_hist_fc                     = epa_fc.loc[(epa_fc["Model_year"] > first__hist_yr) & (epa_fc["Size"] == self.size) & (epa_fc["Technology"] == self.technology) & (epa_fc["Fuel_type"] == self.fuel_type)] 
            #Add degradation factors
            def_fac_matr                        = {'def' : 1,
                                                   'low' : 0.9,
                                                   'high' : 1.1}
            def_fac                             = def_fac_matr.get(fc_conv_mdl)
            tmp_mat_hist_fc                     = tmp_mat_hist_fc * def_fac
        elif self.technology in ["BEV100", "BEV100", "PHEV20", "PHEV40"]:
            fc_ev_hist_fc                       = utils.get_input("fc_ev_hist")     
            #Get the historical values
            tmp_mat_hist_fc                     = fc_ev_hist_fc.loc[(fc_ev_hist_fc["Year"] > first__hist_yr) & (fc_ev_hist_fc["Size"] == self.size) & (fc_ev_hist_fc["Technology"] == self.technology) & (fc_ev_hist_fc["Model"] == "Sales weighted") | (fc_ev_hist_fc["Model"] == fc_ev_mdl)]         
            #Add battery charging efficiency and transmission losses
            tmp_mat_hist_fc.loc[tmp_mat_hist_fc["Fuel"] =='Electricity']["Value"]      \
                                                = tmp_mat_hist_fc.loc[tmp_mat_hist_fc["Fuel"] =='Electricity']["Value"]  / (0.90*0.95)
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
        print(self.fuel_consumption)
        last_hist_yr                            = int(max(self.fuel_consumption["Year"]))

        #Inputs
        material_dt                             = utils.get_input("model_matching_material")
        hist_mc                                 = utils.get_input("fleet_mt_comp_hist")

        #Create dt of material composition with accurate fields
        self.material_composition               = pd.DataFrame(index=pd.unique(material_dt["Own"]), columns=RangeIndex(first_hist_yr, last_yr + 1))


        #Update historical material composition
        mat_hist_mc                             = hist_mc.loc[(hist_mc["Technology"] == self.technology) & (hist_mc["Size"] == self.size) & (hist_mc["Model_year"] >= first_hist_yr) & (hist_mc["Material"] != "Total")]
        fleet_i_mat_cont_f_res                  = fleet_i_mat_cont_f()
        veh_mat_cont                            = fleet_i_mat_cont_f_res.loc[(fleet_i_mat_cont_f_res["Size"] == self.size) & (fleet_i_mat_cont_f_res["Technology"] == self.technology)]
        del(veh_mat_cont["Size"])
        del(veh_mat_cont["Technology"])

        for year in pd.unique(mat_hist_mc["Model_year"]):
            self.material_composition           = pd.concat[self.material_composition, (mat_hist_mc.loc[(mat_hist_mc["Model_year"] == year) & (mat_hist_mc["Material"] != "Total" )]["Value"])]
            self.material_component_composition = pd.concat[self.material_component_composition, veh_mat_cont.loc[year]]

    def vehicle_specifications_f(self, fc_impro = None, first_yr = None, last_yr = None):
        tmp_techno                              = ''

        if 'BEV' or 'PHEV' in self.technology:
            tmp_techno                          = re.findall('[0-9]+', self.technology)
        else:
            tmp_techno                          = self.technology
        
        vehicle_specs_dt                        = utils.get_input('vehicle_specifications')
        bat_fc_dt                               = utils.get_input('greet_battery')

        vehicle_specs                           = vehicle_specs_dt.loc[((tmp_techno in vehicle_specs_dt['Technology'].str.split(',')) | (vehicle_specs_dt['Technology'] == 'Glo') & ((vehicle_specs_dt['Size'] == self.size)) | (vehicle_specs_dt['Size'] == 'Glo'))]

        vehicle_specs_dyn_dt                    = utils.get_input('vehicle_specifications_dyn')
        vehicle_specs_dyn                       = vehicle_specs_dyn_dt.loc[((tmp_techno in vehicle_specs_dyn_dt['Technology'].str.split(',')) | (vehicle_specs_dyn_dt['Technology'] == 'Glo') & ((vehicle_specs_dyn_dt['Size'] == self.size)) | (vehicle_specs_dyn_dt['Size'] == 'Glo')) & (fc_impro in vehicle_specs_dyn_dt['Improvement Scenario'].str.split(','))]

        last_hist_yr                            = 2050                        
        for cols in self.fuel_consumption.columns:
            if len(re.findall('[0-9]+', cols)) != 0 and re.findall('[0-9]+', cols)[0] < last_hist_yr and not pd.isna(self.fuel_consumption[cols]).values.any:
                last_hist_yr                    = re.findall('[0-9]+', cols)[0]
        
        if "Electricity" in self.fuel_type:
            specs_list                          = [vehicle_specs['Parameter'], 'range', 'battery_density', 'peak_power']
        else:
            specs_list                          = [vehicle_specs['Parameter'], 'peak_power']
        
        self.specifications                     = pd.DataFrame(index = specs_list.index, columns= RangeIndex(first_yr, last_yr + 1)) 

        size                                    = [self.size, 'Glo']      

        for par in pd.unique(vehicle_specs['Parameter']):
            dyn_indices                         = vehicle_specs_dyn.index[vehicle_specs_dyn['Parameter'] == par].tolist()
            norm_indices                        = vehicle_specs.index[vehicle_specs['Parameter'] == par].tolist()
            for index in norm_indices:
                if vehicle_specs.loc[index, 'Constant'] == 'n' and vehicle_specs.loc[index, 'Size'] in size:
                    if pd.isna(vehicle_specs.loc[index, 'Value']):
                        line                    = vehicle_specs_dyn.loc[dyn_indices[norm_indices.index(index)]]
                        for key in line.index:
                            try:
                                if len(re.findall('[0-9]+', key)) > 0:
                                    self.specifications.loc[par, int(key)]\
                                                = line.loc[key]
                            except ValueError:
                                continue
                    else:
                        line                    = vehicle_specs_dyn.loc[dyn_indices[norm_indices.index(index)]] * vehicle_specs.loc[(vehicle_specs["Parameter"] == par), "Value"]
                        for key in line.index:
                            try:
                                if len(re.findall('[0-9]+', key)) > 0:
                                    self.specifications.loc[par, int(key)]\
                                                = line.loc[key]
                            except ValueError:
                                continue

        first_cpt_composition_yr                = min(self.material_component_composition["Model Year"])   

        #Update peak power from first material composition. Year=2016\
        cpt_dt                                  = self.vehicle_peak_power_f(self, model_year=first_cpt_composition_yr)  

        for year in self.specifications.columns:
            self.specifications["peak_power", year]\
                                                = sum(cpt_dt["peak_power", year])

        if 'Electricity' in self.fuel_type:
            
            wgt_bat                             = self.material_component_composition.sum(self.material_component_composition.loc['EV Battery', first_cpt_composition_yr])

            for year in range(first_cpt_composition_yr, last_yr + 1):
                self.specifications["battery_density", year]\
                                                = self.battery_density_f("peak_power", year)
                
            
            tmp_techno                          = " ".join(re.findall("[a-zA-Z]+", self.technology)) 
            
            usable_en                           = bat_fc_dt.loc[((tmp_techno in bat_fc_dt['Technology'].str.split(',')) & ((bat_fc_dt['Subcomponent'] == 'EV Battery')) & (bat_fc_dt['Data'] == 'Usable Energy')), '2015']

            for year in range(first_cpt_composition_yr, last_hist_yr + 1):
                self.specifications["range", year]\
                                                = self.specifications["battery_density", year] * wgt_bat/self.fuel_consumption["Electricity", year]* 100*usable_en         
    

    def vehicle_peak_power_f(self, model_year):
        bat_fc_dt                               = utils.get_input('greet_battery')
        pwt_compoments_dt                       = utils.get_input("mackenzie_pwt_components")

        if 'BEV' or 'PHEV' in self.technology:
            tmp_techno                          = " ".join(re.findall("[a-zA-Z]+", self.technology))
        else:
            tmp_techno                          = self.technology

        pwt_compoments_dt                       = pwt_compoments_dt.loc[(tmp_techno in pwt_compoments_dt['Technology'].str.split(',')) & (pwt_compoments_dt['Size'] == self.size), 'Technology']
        cpt_l                                   = pwt_compoments_dt.tolist()
        cpt_dt                                  = pd.DataFrame(index = cpt_l)

        for cpt in cpt_dt.index:
            cpt_dt['wgt']                       = []  
            cpt_dt[cpt, 'wgt']                  = self.material_component_composition.loc[(self.material_component_composition['Model_year'] == model_year) & (self.material_component_composition['Subcomponent'] == cpt), 'Weight']

            cpt_dt['fixed_mass']                = []

            try:
                cpt_dt[cpt, 'fixed_mass']       = bat_fc_dt.loc[(bat_fc_dt['Subcomponent'] == cpt) & (tmp_techno in bat_fc_dt['Technology'].str.split(',')) & (bat_fc_dt['Data'] == 'Fixed Mass'), '2015']
            except KeyError or ValueError:
                cpt_dt[cpt, 'fixed_mass']       = 0

            cpt_dt['density']                   = []
            cpt_dt['peak_power']                = []

            if (cpt in ("Engine","Traction Motor") and tmp_techno != 'HEV') or (cpt in ("Engine","EV Battery") and tmp_techno == 'HEV'):
                cpt_dt[cpt, 'density']          = bat_fc_dt.loc[(bat_fc_dt['Subcomponent'] == cpt) & (tmp_techno in bat_fc_dt['Technology'].str.split(',')) & (bat_fc_dt['Data'] == 'Energy density'), '2015']
                cpt_dt[cpt, 'peak_power']       = cpt_dt[cpt, 'wgt'] - cpt_dt[cpt, 'fixed_mass'] * cpt_dt[cpt, 'density']
            else:
                cpt_dt[cpt, 'density']          = 0
                cpt_dt[cpt, 'peak_power']       = 0

            cpt_dt['usable_e']                  = []
            
            try:
                cpt_dt[cpt, 'usable_e']         = bat_fc_dt.loc[(bat_fc_dt['Subcomponent'] == cpt) & (tmp_techno in bat_fc_dt['Technology'].str.split(',')) & (bat_fc_dt['Data'] == 'Usable Energy'), '2015']
            except KeyError or ValueError:
                cpt_dt[cpt, 'usable_e']         = 1
        
        return cpt_dt    

    def battery_density_f(self, model_year, bat_impro=None):
        bat_fc_dt                               = utils.get_input('greet_battery')

        techno                                  = self.technology
         
        if 'BEV' or 'PHEV' in techno:
            tmp_techno                          = " ".join(re.findall("[a-zA-Z]+", self.technology))
            bat_type                            = self.battery_type
            i_year                              = 2015
            i_dens                              = bat_fc_dt.loc[((tmp_techno in bat_fc_dt['Technology'].str.split(',')) & ((bat_fc_dt['Subcomponent'] == 'EV Battery')) & (bat_fc_dt['Data'] == 'Energy density') & (bat_type in bat_fc_dt['Battery Type'].str.split(','))), i_year] 

            f_year                              = 2030
            f_dens_dict                         = { 'n': i_dens,
                                                    'y': bat_fc_dt.loc[((tmp_techno in bat_fc_dt['Technology'].str.split(',')) & ((bat_fc_dt['Subcomponent'] == 'EV Battery')) & (bat_fc_dt['Data'] == 'Energy density') & (bat_type in bat_fc_dt['Battery Type'].str.split(','))), f_year]
                                                  }                              
            f_dens                              = f_dens_dict.get(bat_impro, None)

            if model_year <= i_year:
                dens                            = i_dens
            elif model_year < f_year:
                dens                            = i_dens+(model_year-i_year)*(f_dens-i_dens)/(f_year-i_year)
            else:
                dens                            = f_dens
            
        else:
            dens                                = np.nan

        return dens

    '''
    Returns the existing attributes of the vehicle as a dataframe
    '''
    def get_data_frame(self, field_name):

        out                                     = pd.DataFrame()

        if type(vars(self)[str(field_name)]) == 'pandas.core.frame.DataFrame':
            out.concat(field_name, self.technology, self.size)

        return out


            



                

            
        

