from dataclasses import dataclass, field
import pandas as pd
import numpy as np

@dataclass
class env:
    isNone: bool = field(default = True, repr = False)
    objects: dict = field(default_factory=dict)

    def isObject(self, objName):
        if objName in self.objects:
            return self.objects[objName]
        else:
            if "_res" in objName:
                f_to_call = objName[:-4] + "(self)"
                self.objects[objName] = eval(f_to_call)

        
def res_env_blank_check(env):
    if env.isNone == True:
        return True
    return False

def res_env_new_attr(env):
    if not env.isNone:
        env.objects["new"] = "happy"
        print(env.objects["new"])

def res_env_new_dt(env):
    if not env.isNone:
        env.objects["elec_source_ev_f_res"] = pd.DataFrame(np.array([1, 2, 3]), columns=["col1"])

def elec_source_ev_f(env):
    return pd.DataFrame(np.array([1, 2, 3]), columns=["col1"])




