import csv
import numpy as np
import pandas as pd
from pandas.io import formats
import pythonport as FLAME


def get_input(inputvar = None, sheet = None):
    input_mgnt                          = pd.read_csv("inputs/data_input_management.csv")

    file_type                           = input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File_format'].array[0]
    
    if file_type == '.csv':
        return pd.read_csv(input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File'].array[0]) 
    elif file_type == '.xlsx':
        return pd.read_excel((input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["File"]).array[0], (input_mgnt.loc[(input_mgnt["Variable_name"] == "model_matching_material")]["Sheet_name"]).array[0], engine="openpyxl")