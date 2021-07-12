import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from rpy2.robjects import r
import rpy2.robjects.pandas2ri as pandas2ri
import rpy2
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
#Import the SignatureTranslatedAnonymousPackage 
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage as STAP
# import rpy2's package module
import rpy2.robjects.packages as rpackages


def rinit():

    # import R's utility package
    utils = rpackages.importr('utils')

    # select a mirror for R packages
    utils.chooseCRANmirror(ind=1) # select the first mirror in the list

    packnames = ('readxl', 'reshape2', 'tidyr', 'stringr')

    # R vector of strings
    from rpy2.robjects.vectors import StrVector

    # Selectively install what needs to be install.
    # We are fancy, just because we can.
    names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
    if len(names_to_install) > 0:
        utils.install_packages(StrVector(names_to_install))

def model_init():
  robjects.r.source("model_setup.R")
  robjects.r.source("load_functions.R")

def FLAME_run_function(fun_name, index=0):
  filepath = 'functions/' + fun_name + '.R'
  var_to_open_as = fun_name.replace("_f", "")
  py_func_name = var_to_open_as + '_func'
    
  with open(filepath, 'r') as globals()[var_to_open_as]:
    globals()[py_func_name] = globals()[var_to_open_as].read()

  fun_res = STAP(globals()[py_func_name], fun_name + '.R') 
  cmd='fun_res.' + fun_name + '()'
  output = eval(cmd)
  if index:
    return(output[index])
  else:
    return output

if __name__ == '__main__':
    model_init()
    print(FLAME_run_function(fun_name="fleet_demand_matrix_f", index = None))