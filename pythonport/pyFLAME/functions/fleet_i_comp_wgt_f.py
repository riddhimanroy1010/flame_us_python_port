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

            tmp_compo_wgt["Component"]          = components
            tmp_subcompo_wgt["Subcomponent"]    = subcomponents

            if len(wgt_dt.loc[(wgt_dt["Technology"] == tmp_techno) & (wgt_dt["Size"] == size) & (wgt_dt["Source"] == 'epa') & (wgt_dt["Model_year"] == '2016')].index) == 1:
                CW_in_kg                        = wgt_dt.loc[(wgt_dt["Technology"] == tmp_techno) & (wgt_dt["Size"] == size) & (wgt_dt["Source"] == 'epa') & (wgt_dt["Model_year"] == '2016')]["Value"]
                CW                              = CW_in_kg*conv.loc["lb", "1 kg"]
                source_CW                       = 'EPA'

            else:
                CW                              = comp_wgt_dt.loc[comp_wgt_dt["Data"] == "Curb weight (w/o bat)"][techno_greet]
                source_CW                       = "GREET"
            
            for comp in ["Battery Lead-Acid","Fluids","Wheels"]:
                tmp_compo_wgt                   = tmp_compo_wgt.append({"Component" : comp, "Weight": comp_wgt_dt.loc[(comp_wgt_dt["Data"] == comp)][techno_greet]})
            
            if len(fleet_i_ev_bat.loc[(fleet_i_ev_bat["Size"] == size) & (fleet_i_ev_bat["Technology"] == techno)].index) != 0:
                tmp_compo_wgt["EV Battery", "Weight"]\
                                                = fleet_i_ev_bat.loc[(fleet_i_ev_bat["Size"] == size) & (fleet_i_ev_bat["Technology"] == techno)]["Weight"]
            
            if techno == "FCV":
                fuel_cell_stack_weight          = (comp_wgt_dt.loc["Fuel cell stack", techno] / bat_fc_dt.loc["Fuel cell stack", "2015"]) * conv.loc["lb", "1kg"]
                tmp_subcompo_wgt.loc["Fuel cell", "Weight"]\
                                                = fuel_cell_stack_weight
                
                tmp_subcompo_wgt.loc["Fuel cell auxiliaries", "Weight"]\
                                                = comp_wgt_dt.loc["Fuel cell auxiliaries", techno_greet]
                tmp_compo_wgt.loc["Powertrain", "Weight"]\
                                                = tmp_subcompo_wgt.loc["Powertrain", "Weight"]

            if source_CW == "GREET":
                for component in ["Battery Lead-Acid","EV Battery"]:
                    CW                          += float(tmp_compo_wgt[component, "Weight"])

            tmp_compo_wgt                       = tmp_compo_wgt.append({"Component" : "Total", "Weight" : CW})  
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.append({"Component" : "Total", "Weight" : CW})  

            CW_rem                              = CW
            for compo in ["Battery Lead-Acid","EV Battery", "Fluid", "Wheels"]:
                CW_rem                          -= float(tmp_compo_wgt.loc[compo, "Weight"])
            
            for comp in tmp_compo_wgt["Component"]:
                if pd.isna(tmp_compo_wgt[comp, "Weight"]) and comp != "Glider":
                    tmp_compo_wgt[comp, "Weight"]\
                                                = vh_comp.loc[(vh_comp["Vehicle Component"] in pd.unique(comp_dt.loc[comp, "GREET"]).tolist())][techno_greet] * CW_rem
            
            tmp_compo_wgt.loc["Glider", "Weight"]\
                                                = CW - vh_comp.sum(tmp_compo_wgt["Component"] != "Total", skipna=True)
            
            for subcomp in tmp_subcompo_wgt["Subcomponent"]:
                if pd.isna(tmp_compo_wgt.loc[subcomp, "Weight"]):
                    tmp_subcompo_wgt.loc[subcomp, "Weight"]\
                                                = tmp_compo_wgt.loc[(tmp_compo_wgt["Own subcomponent"] == subcomp)]["Weight"] * wt_subcomp.loc[(techno_greet in wt_subcomp["Technology"].str.split(",")) & (wt_subcomp["Subcomponent"] == subcomp)]["Subcomponent weight distribution"]

            tmp_compo_wgt                       = tmp_compo_wgt.assign(Relative = pd.series(tmp_compo_wgt["Weight"]/CW).values)
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Relative = pd.series(tmp_subcompo_wgt["Weight"]/CW).values)
            tmp_compo_wgt                       = tmp_compo_wgt.assign(Weight = pd.series(tmp_compo_wgt["Weight"] * conv.loc["kg", "1 lb"]).values)
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Weight = pd.series(tmp_subcompo_wgt["Weight"] * conv.loc["kg", "1 lb"]).values)
            tmp_compo_wgt                       = tmp_compo_wgt.assign(Size = size)
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Size = size)
            tmp_compo_wgt                       = tmp_compo_wgt.assign(Technology= techno)
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Technology = techno)

            fleet_compo_wgt                     = pd.concat([fleet_compo_wgt, tmp_compo_wgt], ignore_index=True)
            fleet_subcompo_wgt                  = pd.concat([fleet_subcompo_wgt, tmp_subcompo_wgt], ignore_index=True)

    fleet_compo_wgt                             = fleet_compo_wgt.assign(Unit = "kg")
    fleet_subcompo_wgt                          = fleet_subcompo_wgt.assign(Unit = "kg")
    return [fleet_compo_wgt, fleet_subcompo_wgt]     




            






