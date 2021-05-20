source("model_setup.R")
outputs_path <- "outputs"
# Scenarios -----------------------------------

do.call(write_scenario_f,list(outputs_path=outputs_path,function_tbc= ,scen_tbc = ))

# Simulations -------------------------------------------------------------

do.call(write_simulation_f,list(outputs_path=outputs_path,function_tbc = ,scen_tbc = ,sim_tbc = ,sim_type = ))

# Sensitivity analysis ----------------------------------------------------

do.call(write_simulation_f,list(outputs_path=outputs_path,function_tbc = ,scen_tbc = ,sens_tbc = ,get_default_attr_val = ))

