from typing import Type
import pandas as pd
import numpy as np
import shelve
import os
import sys

sys.path.insert(1, '/pythonport/pyFLAME')

from . import env

def shelf_make(env_input):

    if 'input_env' not in locals():
        env_store = shelve.open('pythonport/pyFLAME/shelves/shelve.db', flag='n')
        try:
            cur_env                         = env_store[env_input]
        except KeyError:
            if env_input == 'input_env':
                cur_env                         = env.environment()
                cur_env.objects["input_mgnt"]   = pd.read_csv("inputs/data_input_management.csv")
                cur_env.isNone                  = False
                env_store['input_env']          = cur_env
            else:
                cur_env                         = env.environment()
                cur_env.isNone                  = False   
        finally:
            env_store.close()
            return cur_env

def shelf_retrieve(env_input):
    if len(os.listdir('pythonport/pyFLAME/shelves')) > 0:
        env_store                               = shelve.open('pythonport/pyFLAME/shelves/shelve.db')
        try:
            return env_store[env_input]
        except KeyError:
            raise Exception("An unknown error occured in utils/shelf_retrieve")

def shelf_update(env_input, key, value):
    if len(os.listdir('pythonport/pyFLAME/shelves')) > 0:
        env_store                               = shelve.open('pythonport/pyFLAME/shelves/shelve.db')
        try:
            cur_env                             = env_store[env_input]
            cur_env.objects[key]                = value
            env_store[env_input]                = cur_env
        finally:
            env_store.close()
            return cur_env
        
def shelf_destroy():
    if len(os.listdir('pythonport/pyFLAME/shelves')) > 0:
        shelve.close()
        os.remove('pythonport/pyFLAME/shelves/shelve.db.bak')
        os.remove('pythonport/pyFLAME/shelves/shelve.db.dat')
        os.remove('pythonport/pyFLAME/shelves/shelve.db.dir')
  

def get_input(inputvar = None):

    if len(os.listdir('pythonport/pyFLAME/shelves')) == 0:
        input_env                               = shelf_make("input_env")
    
    else:
        input_env = shelf_retrieve("input_env")
      
    input_mgnt                                  = input_env.objects['input_mgnt']

    file_type                                   = input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File_format'].array[0]
    
    if str(inputvar) not in input_env.objects:
        if file_type == '.csv':
            value                               = pd.read_csv(input_mgnt.loc[input_mgnt['Variable_name'] == inputvar, 'File'].array[0])
            input_env                           = shelf_update("input_env", inputvar, value)
        elif file_type == '.xlsx':
            value                               = pd.read_excel((input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["File"]).array[0], (input_mgnt.loc[(input_mgnt["Variable_name"] == inputvar)]["Sheet_name"]).array[0], engine="openpyxl")
            input_env                           = shelf_update("input_env", inputvar, value)
    
    return input_env.objects[inputvar]
    

