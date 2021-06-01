from dataclasses import dataclass, field
import pandas as pd
import numpy as np

@dataclass
class environment:
    isNone: bool = field(default = True, repr = False)
    objects: dict = field(default_factory=dict)

    def isObject(self, objName):
        if objName in self.objects:
            return self.objects[objName]
        else:
            if "_res" in objName:
                f_to_call = objName[:-4] + "(self)"
                self.objects[objName] = eval(f_to_call)





