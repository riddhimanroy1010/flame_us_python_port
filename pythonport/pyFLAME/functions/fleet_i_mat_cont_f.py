from typing import Sized
import numpy as np
import math
import pandas as pd
import re
import openpyxl
from pandas.core.indexes.range import RangeIndex

from ..architecture import utils
from . import fleet_i_comp_wgt_f

def fleet_i_mat_cont_f(mat_cont_adj = None, rel_mat_cont = "n"):

    ori_mat_comp                                = utils.get_input('component_material_comp_acsm')
    adj_mat_comp                                = utils.get_input('component_material_comp_adj')
    material                                    = utils.get_input('model_matching_material')
    vh_techno                                   = utils.get_input('model_matching_technology')
    comp_dt                                     = utils.get_input('model_matching_component')

    fleet_subcompo_wgt                          = fleet_i_comp_wgt_f()["fleet_subcompo_wgt"]

    if mat_cont_adj == "y":
        mat_comp                                = adj_mat_comp
    else:
        mat_comp                                = ori_mat_comp

    dt_col                                      = ["Size","Technology","Component","Subcomponent","Material","Weight"]
    fleet_mat_cont                              = pd.DataFrame(columns=dt_col)

    for size in ["Car", "Light truck"]:
        for techno in pd.unique(vh_techno["Own"]):
            if 'BEV' in techno or "PHEV" in techno:
                range_i                         = vh_techno.loc[vh_techno["Own"] == techno]["Range"]
                tmp_techno                      = " ".join(re.findall("[a-zA-Z]+", techno))

            else:
                range_i                         = np.nan
                tmp_techno                      = techno
            
            subcomponent                        = pd.unique(fleet_subcompo_wgt.loc[(fleet_subcompo_wgt["Size"] == size) & (fleet_subcompo_wgt["Technology"] == techno) & (fleet_subcompo_wgt["Subcomponent"] != "Total")]["Subcomponent"])
            for subcomp in subcomponent:
                component                       = comp_dt.loc[(comp_dt["Own subcomponent"] == subcomp)]["Own component"]
                row_tbc                         = mat_comp.loc[(tmp_techno in mat_comp["Technology"].str.split(",")) | (mat_comp["Technology"] == "Glo") & (mat_comp["Subcomponent"] == subcomp)]

                for mat in pd.unique(material["Own"]):
                    if rel_mat_cont == 'y':
                        mat_compo                   = fleet_subcompo_wgt.loc[(fleet_subcompo_wgt["Size"] == size) & (fleet_subcompo_wgt["Subcomponent"] == subcomp)]["Relative"] * mat_comp.loc[row_tbc, mat]
                    elif rel_mat_cont == 'n':
                        mat_compo                   = fleet_subcompo_wgt.loc[(fleet_subcompo_wgt["Size"] == size) & (fleet_subcompo_wgt["Subcomponent"] == subcomp)]["Weight"] * mat_comp.loc[row_tbc, mat]
                    fleet_mat_cont                  = fleet_mat_cont.append({"Size" : size, "Technology": techno, "Component": component, "Subcomponent" : subcomp, "Material" : mat, "Material Composition" : mat_compo})

    fleet_mat_cont["Weight"]                        = float(fleet_mat_cont["Weight"])
    
