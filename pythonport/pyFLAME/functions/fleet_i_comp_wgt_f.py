from pythonport.pyFLAME.functions.fleet_i_ev_bat_f import fleet_i_ev_bat_f
from typing import Sized
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from architecture import utils

def fleet_i_comp_wgt_f(wgt_scen_GREET = None, mod_scen_GREET = None):
    #input files
    vh_techno                                   = utils.get_input("model_matching_technology")
    comp_dt                                     = utils.get_input('model_matching_component')
    vh_comp_wgt_dist                            = utils.get_input('greet_rel_cpt_wgt_2017')
    wgt_greet_dt                                = utils.get_input('greet_wgt_2017')
    wgt_dt                                      = utils.get_input('epa_fleet_wgt_hist')
    conv                                        = utils.get_input('conversion_units')
    wt_subcomp                                  = utils.get_input('c2g_rel_subcpt_wgt')
    bat_fc_dt                                   = utils.get_input('greet_battery')

    fleet_i_ev_bat                              = fleet_i_ev_bat_f()

    #Creation output
    #fleet_compo_wgt contains the weight of the components of the technology techno at size size
    dt_col                                      = ["Size", "Technology", "Component", "Weight"]
    fleet_compo_wgt                             = pd.DataFrame(columns=dt_col)

    #fleet_subcompo_wgt cotnains the weight of the subcomponent of the technology techno at size size
    dt_col                                      = ["Size", "Technology", "Subcomponent", "Weight"]
    fleet_subcompo_wgt                          = pd.DataFrame(columns=dt_col)
    
    for size in ["Car", "Light truck"]:

        #Size_GREET:Model size to use in GREET
        if size == 'Light truck' and wgt_scen_GREET in [1, 4] or size == 'Car' and wgt_scen_GREET in [3, 4]:
            size_greet                          = 'SUV'
        
        elif size == 'Light truck' and wgt_scen_GREET in [2, 3]:
            size_greet                          = 'PUT'
        else:
            size_greet                          = 'Car'
        



