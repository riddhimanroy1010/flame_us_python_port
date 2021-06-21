from typing import Sized
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from FLAMEstructure import utils

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