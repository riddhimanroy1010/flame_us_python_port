from typing import Sized
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from ..architecture import utils
from . import fleet_i_ev_bat_f

def fleet_i_comp_wgt_f(wgt_scen_GREET = None, mod_scen_GREET = None):

    if wgt_scen_GREET or mod_scen_GREET == None:
        wgt_scen_GREET                          = utils.get_attributes('wgt_scen_GREET')
        mod_scen_GREET                          = utils.get_attributes('mod_scen_GREET')

    #input files
    vh_techno                                   = utils.get_input("model_matching_technology")
    comp_dt                                     = utils.get_input('model_matching_component')
    vh_comp_wgt_dist                            = utils.get_input('greet_rel_cpt_wgt_2017')
    wgt_greet_dt                                = utils.get_input('greet_wgt_2017')
    wgt_dt                                      = utils.get_input('epa_fleet_wgt_hist')
    conv                                        = utils.get_input('conversion_units')
    wt_subcomp                                  = utils.get_input('c2g_rel_subcpt_wgt')
    bat_fc_dt                                   = utils.get_input('greet_battery')


    fleet_i_ev_bat                              = fleet_i_ev_bat_f.fleet_i_ev_bat_f()

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
                range_i                         = np.nan

            comp_wgt_dt                         = wgt_greet_dt.loc[(wgt_greet_dt["Model"].str.contains(mod_scen_GREET)) & (wgt_greet_dt['Size'] == size_greet)][techno_greet.values[0]]
            comp_wgt_dt.index                   = wgt_greet_dt.loc[(wgt_greet_dt["Model"].str.contains(mod_scen_GREET)) & (wgt_greet_dt['Size'] == size_greet)]['Data']

            vh_comp                             = vh_comp_wgt_dist.loc[(vh_comp_wgt_dist["Model"] == float(mod_scen_GREET)) & (vh_comp_wgt_dist["Size"] == size_greet)][techno_greet.values[0]]
            vh_comp.index                       = vh_comp_wgt_dist.loc[(vh_comp_wgt_dist["Model"] == float(mod_scen_GREET)) & (vh_comp_wgt_dist["Size"] == size_greet)]["Vehicle component"]
            vh_comp                             = pd.concat([(vh_comp_wgt_dist.loc[(vh_comp_wgt_dist["Vehicle component"] == "Range")][techno_greet.values[0]]).reset_index(drop = True), vh_comp])

            # tmp_compo_wgt                       = pd.DataFrame(index = [techno_greet.values[0]], columns=["Component", "Weight"])
            # tmp_subcompo_wgt                    = pd.DataFrame(index = [techno_greet.values[0]], columns=["Components", "Subcomponent", "Weight"])
            tmp_compo_wgt                       = pd.DataFrame()
            tmp_subcompo_wgt                    = pd.DataFrame()

            tmp_compo_wgt                       = tmp_compo_wgt.assign(Component=pd.unique(wt_subcomp.loc[wt_subcomp["Technology"].str.contains(techno_greet.values[0])]["Component"]))
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Component=wt_subcomp.loc[wt_subcomp["Technology"].str.contains(techno_greet.values[0])]["Component"])
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.assign(Subcomponent=wt_subcomp.loc[wt_subcomp["Technology"].str.contains(techno_greet.values[0])]["Subcomponent"])
            tmp_compo_wgt["Weight"]             = np.nan
            tmp_subcompo_wgt["Weight"]          = np.nan


            if len(wgt_dt.loc[(wgt_dt["Technology"] == tmp_techno) & (wgt_dt["Size"] == size) & (wgt_dt["Source"] == 'epa') & (wgt_dt["Model_year"] == '2016')].index) == 1:
                CW_in_kg                        = wgt_dt.loc[(wgt_dt["Technology"] == tmp_techno) & (wgt_dt["Size"] == size) & (wgt_dt["Source"] == 'epa') & (wgt_dt["Model_year"] == '2016')]["Value"]
                CW                              = CW_in_kg*conv.loc["lb", "1 kg"]
                source_CW                       = 'EPA'

            else:
                CW                              = comp_wgt_dt.loc["Curb weight (w/o bat)"]
                source_CW                       = "GREET"
            
            for comp in ["Battery Lead-Acid","Fluids","Wheels"]:
                tmp_compo_wgt.at[list(np.where(tmp_compo_wgt["Component"] == comp))[0], "Weight"]\
                                                = comp_wgt_dt.loc[comp]
            
            if len(fleet_i_ev_bat.loc[(fleet_i_ev_bat["Size"] == size) & (fleet_i_ev_bat["Technology"] == techno)].index) != 0:
                tmp_compo_wgt.at[list(np.where(tmp_compo_wgt["Component"] == "EV Battery"))[0], "Weight"]\
                                                = fleet_i_ev_bat.loc[(fleet_i_ev_bat["Size"] == size) & (fleet_i_ev_bat["Technology"] == techno)]["Weight"]
            
            if techno == "FCV":
                fuel_cell_stack_weight          = (comp_wgt_dt.loc["Fuel cell stack", techno] / bat_fc_dt.loc["Fuel cell stack", "2015"]) * conv.loc["lb", "1kg"]
                tmp_subcompo_wgt.at[list(np.where(tmp_subcompo_wgt["Component"] == "Fuel cell"))[0], "Weight"]\
                                                = fuel_cell_stack_weight
                
                tmp_subcompo_wgt.at[list(np.where(tmp_subcompo_wgt["Component"] == "Fuel cell auxiliaries"))[0], "Weight"]\
                                                = comp_wgt_dt.loc["Fuel cell auxiliaries", techno_greet]
                                                
                tmp_subcompo_wgt.at[list(np.where(tmp_subcompo_wgt["Component"] == "Powertrain"))[0], "Weight"]\
                                                = tmp_subcompo_wgt.loc["Powertrain", "Weight"]

            if source_CW == "GREET" and techno != "ICEV-G" and techno != "FCV" and techno != "ICEV-D":
                for component in ["Battery Lead-Acid","EV Battery"]:
                    CW                          += float(tmp_compo_wgt.loc[tmp_compo_wgt["Component"] == component]["Weight"].values[0])
            else:
                CW                          += float(tmp_compo_wgt.loc[tmp_compo_wgt["Component"] == "Battery Lead-Acid"]["Weight"].values[0])

            tmp_compo_wgt                       = tmp_compo_wgt.append({"Component" : "Total", "Weight" : CW}, ignore_index=True)  
            tmp_subcompo_wgt                    = tmp_subcompo_wgt.append({"Component" : "Total", "Weight" : CW}, ignore_index=True)  

            CW_rem                              = CW
            if techno != "ICEV-G" and techno != "FCV" and techno != "ICEV-D":         
                for compo in ["Battery Lead-Acid","EV Battery", "Fluids", "Wheels"]:
                    CW_rem                      -= float(tmp_compo_wgt.loc[tmp_compo_wgt["Component"] == compo]["Weight"].values[0])
            else:
                for compo in ["Battery Lead-Acid","Fluids", "Wheels"]:
                        CW_rem                  -= float(tmp_compo_wgt.loc[tmp_compo_wgt["Component"] == compo]["Weight"].values[0])

            
            for comp in tmp_compo_wgt["Component"]:
                if pd.isna(tmp_compo_wgt.loc[tmp_compo_wgt["Component"] == comp]["Weight"]).values[0] and comp != "Glider":
                    for item in vh_comp.index: 
                        if item in pd.unique(comp_dt.loc[comp_dt["Own component"] == comp]["GREET"]):
                            tmp_compo_wgt.at[list(np.where(tmp_compo_wgt["Component"] == comp))[0], "Weight"]\
                                                = vh_comp.loc[item] * CW_rem
            
            tmp_compo_wgt.at[list(np.where(tmp_compo_wgt["Component"] == "Glider"))[0], "Weight"]\
                                                = CW - tmp_compo_wgt.loc[tmp_compo_wgt["Component"] != "Total"]["Weight"].sum(skipna=True)
            
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




            






