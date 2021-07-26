from typing import Sized
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from ..architecture import utils


def fleet_i_ev_bat_f(FCV_bat_t = "Li_ion LMO", BEV_bat_t = "Li_ion LMO", PHEV_bat_t = "Li_ion LMO", HEV_bat_t = "Ni_MH", wgt_scen_GREET = "def", ev_bat_size_mdl = "def"):
    vh_techno                                   = utils.get_input("model_matching_technology") 
    wt_subcomp                                  = utils.get_input("c2g_rel_subcpt_wgt")
    ev_bat_size_dt                              = utils.get_input("ev_bat_size")  
    greet_battery_size                          = utils.get_input("greet_battery_size")
    bat_fc_dt                                   = utils.get_input("greet_battery")
    conv                                        = utils.get_input("conversion_units")

    dt_col                                      = ["Size", "Technology", "Bat_type", "Capacity", "Weight"]
    ev_bat_dt                                   = pd.DataFrame(columns=dt_col)

    for size in ("Car", "Light truck"):
        init_size                               = 0
        for techno in pd.unique(vh_techno["Own"]):
            techno_greet                        = vh_techno.loc[vh_techno["Own"] == techno]["GREET1"][init_size]
            init_size                           += 1
            component                           = wt_subcomp.loc[wt_subcomp["Technology"].str.contains(techno_greet, regex = True)]["Component"]

            if component.str.contains("EV Battery").any():
                if "BEV" in techno:
                    bat_type                    = BEV_bat_t
                elif "PHEV" in techno:
                    bat_type                    = PHEV_bat_t
                elif techno == 'HEV':
                    bat_type                    = HEV_bat_t
                elif techno == 'FCV':
                    bat_type                    = FCV_bat_t
                
                if 'BEV' in techno or 'PHEV' in techno:
                    bat_cap                     = ev_bat_size_dt.loc[(ev_bat_size_dt["Year"] == 2020) & (ev_bat_size_dt["Size"] == size) & (ev_bat_size_dt["Technology"] == techno) & (ev_bat_size_dt["Model"] == ev_bat_size_mdl)]["Value"]
                    tmp_techno                  = " ".join(re.findall("[a-zA-Z]+", techno))
                else:
                    if size == 'Light truck' and wgt_scen_GREET in [1, 4] or size == "Car" and wgt_scen_GREET in [3, 4]:
                        size_greet              = "SUV"
                    elif size == "Light truck" and wgt_scen_GREET in [2, 3]:
                        size_greet              = "PUT"
                    else:
                        size_greet              = "Car"

                    bat_cap                     = greet_battery_size.loc[(greet_battery_size["Data"] == "Conventional") & (greet_battery_size["Size"] == size_greet) & (greet_battery_size["Technology"] == techno)]["value"].values[0]
                    tmp_techno                  = techno

                bat_wgt                     = bat_cap / (bat_fc_dt.loc[(bat_fc_dt["Subcomponent"] == "EV Battery") & (bat_fc_dt["Technology"].str.contains(tmp_techno)) & (bat_fc_dt["Battery type"] == bat_type)]["2015"] * conv["1 kg"][3]).values[0]

                ev_bat_dt                   = ev_bat_dt.append({"Size" : size, "Technology" : techno, "Bat_type" : bat_type, "Capacity" : bat_cap, "Weight" : bat_wgt}, ignore_index=True)
                #ev_bat_dt                   = ev_bat_dt.append(size).append(techno).append(bat_type).append(float(bat_cap)).append(float(bat_wgt))
    
    return ev_bat_dt