from dataclasses import dataclass, field
import numpy as np
import pandas as pd
import re

class vehicleClass():
    '''
    Data fields:
        technology: type str
        size: type str
        fuel_type: type str
        fuel_consumption: type numpy matrix
        utility_factor: type numpy matrix
        specifications: type numpy matrix
        battery_type: type str
        material_composition: type numpy matrix
        material_component_composition: type pandas dataframe
    Functions:
        __init__: initializes the function and it's fields
        get_data_frame: converts the existig fields into dataframes and returns dataframe
    '''
    technology: str = field(default_factory=str)
    size: str = field(default_factory=str)
    fuel_type: str = field(default_factory=str)
    fuel_consumption: np.matrix = field(default_factory=np.matrix)
    utility_factor: np.matrix = field(default_factory=np.matrix)
    specifications: np.matrix = field(default_factory=np.matrix)
    battery_type: str = field(default_factory=str)
    material_composition: np.matrix = field(default=np.matrix)
    material_component_composition: pd.DataFrame = field(default_factory=pd.DataFrame)

    '''
    Initializing the vehicle class with get_input to get data from environment class.
    '''
    def __init__(self) -> None:
        vh_techno = get_input("model_matching_technology") 
        #implement get_input later

        '''.self$technology and .self$size from the R code need not be manually changed; they can be assigned at creation of the vehicle class.'''
        self.fuel_type = 0

    '''
        Returns the existing attributes of the vehicle as a dataframe
    '''
    def get_data_frame(self):
        out = pd.DataFrame()
        for attribute in vars(self):
            if type(attribute) == 'numpy.matrix':
                temp = pd.DataFrame(vars(self)[attribute])
                # cbind(Data=rownames(field_values),stringsAsFactors = FALSE) What does this line do?
                temp.concat(temp, self.technology, self.size)
                out.concat(temp[['Technology', 'Size', 'Model_year', 'Data', 'Value']])
                replace = {"fuel_consumption": "Fuel", "utility_factor" : "Fuel", "specifications" : "Attribute", "material_composition" : "Material"}
                out.rename(columns = replace, inplace = True)
            elif type(attribute) == 'pandas.core.frame.DataFrame':
                out.concat(attribute, self.technology, self.size)

        return out

class fleetClass():
    vint_stock: list = field(default_factory=list)
    vint_scrap: list = field(default_factory=list)
    technology_market_share: np.matrix = field(default_factory=np.matrx)
    ldv_sales: np.matrix = field(default_factory=np.matrx)
    ldv_on_road_stock: np.matrix = field(default_factory=np.matrx)
    ldv_on_road_stock_tot: np.matrix = field(default_factory=np.matrx)

    def get_data_frame(self):
        out = pd.DataFrame()
        for attribute in vars(self):
            if type(attribute) == "numpy.matrix":
                temp = pd.DataFrame(vars(self)[attribute])

                

            
        

