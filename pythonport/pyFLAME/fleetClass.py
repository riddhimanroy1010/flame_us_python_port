from dataclasses import dataclass, field
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from . import utils

@dataclass
class fleetClass():
    '''
    Data fields:
        vint_stock                          :            type list
        vint_scrap                          :            type list
        technology_market_share             :            type pandas.core.frame.DataFrame
        fuel_consumption                    :            type pandas.core.frame.DataFrame
        ldv_sales                           :            type pandas.core.frame.DataFrame
        ldv_on_road_stock                   :            type pandas.core.frame.DataFrame
        ldv_on_road_stock_tot               :            type pandas.core.frame.DataFrame
    Functions:
        __init__: initializes the function and its fields
        get_data_frame: converts the existing fields into dataframes and returns pandas.core.frame.DataFrame
        get_list_dataframe: Return all fields into a list of pandas.core.frame.DataFrame
    '''
    vint_stock:                                 list                        = field(default_factory=list)
    vint_scrap:                                 list                        = field(default_factory=list)
    fuel_consumption:                           pd.DataFrame                = field(default_factory=pd.DataFrame)
    technology_market_share:                    pd.DataFrame                = field(default_factory=pd.DataFrame)
    ldv_sales:                                  pd.DataFrame                = field(default_factory=pd.DataFrame)
    ldv_on_road_stock:                          pd.DataFrame                = field(default_factory=pd.DataFrame)
    ldv_on_road_stock_tot:                      pd.DataFrame                = field(default_factory=pd.DataFrame)


    def __init__(self, first_yr = None, last_yr = None) -> None:
        #Inputs
        hist_stock_dt                           = utils.get_input("fleet_stock_hist")
        hist_sales_dt                           = utils.get_input("ffleet_sales_hist")

        #Create historical matrix stock and sales
        i_year                                  = 1979
        first_proj_yr                           = max(hist_stock_dt["Year"]) + 1
        dataframe_stock_hist                    = pd.DataFrame(index = pd.Series(data = hist_stock_dt["Size"] & hist_stock_dt["Technology"]))



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

