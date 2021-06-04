from dataclasses import dataclass, field
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

import utils

@dataclass
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
    technology_market_share: np.matrix = field(default_factory=np.matrix)
    ldv_sales: np.matrix = field(default_factory=np.matrix)
    ldv_on_road_stock: np.matrix = field(default_factory=np.matrix)
    ldv_on_road_stock_tot: np.matrix = field(default_factory=np.matrix)

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

