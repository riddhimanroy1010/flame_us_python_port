from typing import Type
import pandas as pd
import numpy as np
import shelve
import os

import env

def shelf_make():

    if 'input_env' not in locals():
        env_store = shelve.open('pythonport/pyFLAME/shelves/shelve.db', flag='n')
        try:
            input_env                       = env_store['input_env']
        except KeyError:
            input_env = env.environment()
            input_env.objects["input_mgnt"] = pd.read_csv("inputs/data_input_management.csv")
            input_env.isNone                = False
            env_store['input_env']          = input_env   
        finally:
            env_store.close()
            return input_env


def shelf_retrieve():
    if len(os.listdir('pythonport/pyFLAME/shelves')) > 0:
        env_store                               = shelve.open('pythonport/pyFLAME/shelves/shelve.db')
        try:
            return env_store['input_env']
        except KeyError:
            raise Exception("An unknown error occured in utils/shelf_retrieve")


def shelf_update(key, value):
    if len(os.listdir('pythonport/pyFLAME/shelves')) > 0:
        env_store                               = shelve.open('pythonport/pyFLAME/shelves/shelve.db')
        try:
            input_env                           = env_store['input_env']
            input_env.objects[key]              = value
            env_store['input_env']              = input_env
        finally:
            env_store.close()
            return input_env
        

def get_input(inputvar = None, sheet = None):

    if len(os.listdir('pythonport/pyFLAME/shelves')) == 0:
        input_env                           = shelf_make()
    
    else:
        input_env = shelf_retrieve()
      
    input_mgnt                              = input_env.objects['input_mgnt']

    file_type                               = input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File_format'].array[0]
    
    if str(inputvar) not in input_env.objects:
        if file_type == '.csv':
            value                               = pd.read_csv(input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File'].array[0])
            input_env                           = shelf_update(inputvar, value)
        elif file_type == '.xlsx':
            value                               = pd.read_excel((input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["File"]).array[0], (input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["Sheet_name"]).array[0], engine="openpyxl")
            input_env                           = shelf_update(inputvar, value)
    
    return input_env.objects[inputvar]

if __name__ == '__main__':
    print(get_input('model_matching_material'))
    

