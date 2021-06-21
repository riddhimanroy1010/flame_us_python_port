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
        
        #Loop technology
        for techno in pd.unique(vh_techno["Own"]):
            if 'BEV' in techno or "PHEV" in techno:
                range_i                         = vh_techno.loc[vh_techno["Own"] == techno]["Range"]
                tmp_techno                      = " ".join(re.findall("[a-zA-Z]+", techno))

            else:
                range_i                         = np.nan
                tmp_techno                      = techno
            
            techno_greet                        = vh_techno.loc[vh_techno["Own"] == techno]["GREET1"]

            if 'BEV' in techno or "PHEV" in techno:
                range_i                         = vh_techno.loc[vh_techno["Own"] == techno]["Range"]
                tmp_techno                      = " ".join(re.findall("[a-zA-Z]+", techno))

            else:
                range                           = np.nan

            comp_wgt_dt                         = wgt_greet_dt.loc[(mod_scen_GREET in wgt_greet_dt["Model"].str.split(',')) & (wgt_greet_dt['Size'] == size_greet) & (pd.isna(wgt_greet_dt.loc[wgt_greet_dt["Data"] == 'Range'])) | (wgt_greet_dt["Data"] == "Range")][techno_greet]

            vh_comp                             = vh_comp_wgt_dist.loc[(vh_comp_wgt_dist["Model"] == mod_scen_GREET) & (vh_comp_wgt_dist["Size"] == size_greet)][techno_greet]
            vh_comp                             = pd.concat([pd.DataFrame(vh_comp_wgt_dist.loc["Range"][techno_greet]), vh_comp].reset_index(drop = True))

            components                          = pd.unique(wt_subcomp.loc[techno in wt_subcomp["Technology"]["Component"].str.split(',')]).tolist()
            subcomponents                       = pd.unique(wt_subcomp.loc[techno in wt_subcomp["Technology"]["Subcomponent"].str.split(',')]).tolist()

            tmp_compo_wgt                       = pd.DataFrame(index = components, columns=["Component", "Weight"])
            tmp_subcompo_wgt                    = pd.DataFrame(index = subcomponents, columns=["Components", "Subcomponent", "Weight"])

            






