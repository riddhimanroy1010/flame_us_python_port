import pandas as pd
import __init__ as FLAME

def get_input(inputvar = None):
    input_mgnt                          = pd.read_csv("inputs/data_input_management.csv")

    file_type                           = input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File_format'].array[0]
    
    if file_type == '.csv':
        return pd.read_csv(input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File'].array[0]) 
    elif file_type == '.xlsx':
        return pd.read_excel((input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["File"]).array[0], (input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["Sheet_name"]).array[0], engine="openpyxl")
    
