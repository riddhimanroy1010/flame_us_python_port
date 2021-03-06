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
    '''                                                                                                                         # Initialized?
    vint_stock:                                 list                        = field(default_factory=list)                       # No
    vint_scrap:                                 list                        = field(default_factory=list)                       # No
    technology_market_share:                    pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    ldv_sales:                                  pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    ldv_on_road_stock:                          pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes
    ldv_on_road_stock_tot:                      pd.DataFrame                = field(default_factory=pd.DataFrame)               # Yes


    def __init__(self, first_yr = None, last_yr = None) -> None:
        #Inputs
        hist_stock_dt                           = utils.get_input("fleet_stock_hist")
        hist_sales_dt                           = utils.get_input("ffleet_sales_hist")

        #Create historical matrix stock and sales
        i_year                                  = 1979
        first_proj_yr                           = max(hist_stock_dt["Year"]) + 1

        dataframe_stock_hist_temp               = pd.DataFrame(columns= RangeIndex(min(hist_stock_dt["Year"]), max(hist_stock_dt["Year"]) + 1))
        dataframe_stock_hist_temp.insert(0, "Technology", pd.unique(hist_stock_dt["Technology"]))
        dataframe_stock_hist                    = dataframe_stock_hist_temp.append(dataframe_stock_hist_temp[:], ignore_index=True)
        dataframe_stock_hist.insert(0, 'Size', pd.unique(hist_stock_dt["Size"])[0])

        for index, rows in dataframe_stock_hist.iterrows:
            if index >= len(dataframe_stock_hist.index)//2:
                dataframe_stock_hist['Size'][index]\
                                                = 'Light truck'
        for year in pd.unique(hist_stock_dt["Year"]):
            for tech in pd.unique(hist_stock_dt["Technology"]):
                for size in pd.unique(hist_stock_dt["Size"]):
                    try:
                        value                   = (hist_stock_dt.loc[(hist_stock_dt["Year"] == year) & (hist_stock_dt["Technology"] == tech) & (hist_stock_dt["Size"] == size)]["Value"]).array[0]
                        index                   = (dataframe_stock_hist[(dataframe_stock_hist['Size'] == size) & (dataframe_stock_hist['Technology'] == tech)].index)
                        dataframe_stock_hist.at[index, year] \
                                                = value
                    except IndexError:
                        pass
        
        dataframe_sales_hist_temp               = pd.DataFrame(columns= RangeIndex(min(hist_sales_dt["Year"]), max(hist_sales_dt["Year"]) + 1))
        dataframe_sales_hist_temp.insert(0, "Technology", pd.unique(hist_sales_dt["Technology"]))
        dataframe_sales_hist                    = dataframe_sales_hist_temp.append(dataframe_sales_hist_temp[:], ignore_index=True)
        dataframe_sales_hist.insert(0, 'Size', pd.unique(hist_sales_dt["Size"])[0])

        for index, rows in dataframe_sales_hist.iterrows:
            if index >= len(dataframe_sales_hist.index)//2:
                dataframe_sales_hist['Size'][index]\
                                                = 'Light truck'
        for year in pd.unique(hist_sales_dt["Year"]):
            for tech in pd.unique(hist_sales_dt["Technology"]):
                for size in pd.unique(hist_sales_dt["Size"]):
                    try:
                        value                   = (hist_sales_dt.loc[(hist_sales_dt["Year"] == year) & (hist_sales_dt["Technology"] == tech) & (hist_sales_dt["Size"] == size)]["Value"]).array[0]
                        index                   = (dataframe_sales_hist[(dataframe_sales_hist['Size'] == size) & (dataframe_sales_hist['Technology'] == tech)].index)
                        dataframe_sales_hist.at[index, year] \
                                                = value
                    except IndexError:
                        pass

        self.ldv_on_road_stock                  = dataframe_stock_hist
        self.ldv_sales                          = dataframe_sales_hist

        self.ldv_on_road_stock_tot              = pd.DataFrame(index=['Total'], columns= RangeIndex(i_year, last_yr + 1))
        for cols in self.ldv_on_road_stock.columns:
            self.ldv_on_road_stock_tot[cols]    = self.ldv_on_road_stock[cols].sum()

        ldv_temp_stock                          = self.ldv_on_road_stock
        del ldv_temp_stock['Size']
        del ldv_temp_stock['Technology']
        ldv_temp_stock_mat                      = ldv_temp_stock.to_numpy()
        ldv_temp_stock_tot_mat                  = self.ldv_on_road_stock_tot.columns.to_numpy()
        self.technology_market_share            = np.matmul(ldv_temp_stock_mat, np.diag(1/ldv_temp_stock_tot_mat))

        utils.add_attributes(self)
    '''
    Returns the fields of the class as a pandas.core.frame.DataFrame
    '''
    def get_data_frame(self, field_name):
        out                                     = pd.DataFrame(vars(self)[field_name])
        if type(vars(self)[field_name]) == "numpy.matrix":
            temp                                = pd.DataFrame(vars(self)[field_name])
            ##same problem: what does the cbind line do?
            temp.concat(temp, self.technology, self.size)
            out.concat(temp[["Year", "Value"]])
            for row, data in out.iterrows():
                str                             = out['Type'][row]
                val                             = float(re.findall('_', out['Type'][row]))
                out['Size'][row]                = str[:val - 1]
                out['Technology'][row]          = str[:val + 1200]
                out['Type'][row]                = None
        elif type(vars(self)[field_name]) == list:
            for i in range(len(vars(self)[field_name])):
                tmp_stock_dt                    = pd.DataFrame(vars(self)[field_name][i])
                tmp_stock_dt.concat(tmp_stock_dt, {'Year' : int(vars(self)[field_name]['year'][i])})
                str                             = tmp_stock_dt['Type'][i]
                val                             = float(re.findall('_', tmp_stock_dt['Type'][i]))
                tmp_stock_dt['Size'][i]         = str[:val - 1]
                tmp_stock_dt['Technology'][i]   = str[:val + 1200]
                tmp_stock_dt['Type'][i]         = None

                out                             = pd.DataFrame()
                out.concat(tmp_stock_dt)
        return out
    '''
    Return all fields into a list of pandas.core.frame.DataFrame
    '''
    def get_list_dataframe(self):
        return [self.get_data_frame('vint_stock'), self.get_data_frame('vint_scrap')]

