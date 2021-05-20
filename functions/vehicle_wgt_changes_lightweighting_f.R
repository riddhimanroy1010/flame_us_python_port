#' vehicle_wgt_changes_lightweighting_f
#' Function: Apply material substitution due to lightweighting to a vehicle material and component compositions
#' @export
vehicle_wgt_changes_lightweighting_f = function(vehicle,model_year,mat_mc_component,lw_scen=NA,lw_first_yr=NA,lw_period=NA){
  attribute_f("vehicle_wgt_changes_lightweighting_f")
  #Inputs
  lw_sc_dt  <- get_input_f(input_name = 'lw_subsitution_scen')
  #Other parameters. Update the first year for lightweighting if last_hist_yr is higher
  lw_start_yr <- max(max(as.numeric(colnames(vehicle$fuel_consumption)[!is.na(vehicle$fuel_consumption[1,])])),lw_first_yr)
  lw_last_yr <- lw_start_yr+lw_period
  #mat_i_mc_component: Material composition before any lightweighting
  mat_i_mc_component <- acast(data=subset(vehicle$material_component_composition, Model_year==(lw_start_yr-1)), Material ~ Subcomponent, value.var='Weight',fun.aggregate=sum, margins=FALSE)
  mat_f_mc_component <- mat_mc_component
  #Apply lightweighting
  if (model_year%in%(lw_start_yr:lw_last_yr) & any(lw_sc_dt$LW_scenario==lw_scen & (sapply(1:nrow(lw_sc_dt),function(x)vehicle$technology %in% unlist(strsplit(lw_sc_dt$Technology[x],",")))|lw_sc_dt$Technology=="Glo"))){
    #rpl_rates it the data frame with the replacement rates of the components
    rpl_rates <- lw_sc_dt[lw_sc_dt$LW_scenario==lw_scen&(sapply(1:nrow(lw_sc_dt),function(x)vehicle$technology %in% unlist(strsplit(lw_sc_dt$Technology[x],",")))|lw_sc_dt$Technology=="Glo"),]
    #mat_comp_obj contains the relative material content in the subcomponent of the different replaced material to achieve by lw_last_yr
    dt_col <- c("Subcomponent","Replaced material","Objective")
    mat_comp_obj <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
    #rpl_subs_l contains the list of subcomponents to be edited
    rpl_subc_l <- unique(rpl_rates$Subcomponent[rpl_rates$Subcomponent %in% colnames(mat_mc_component)])
    #Fill mat_comp_obj
    for (subc in rpl_subc_l){
      for (rpd_mat in unique(rpl_rates$`Replaced material`[rpl_rates$Subcomponent==subc])){
        mat_comp_obj[nrow(mat_comp_obj)+1,c("Subcomponent","Replaced material")] <- c(subc,rpd_mat)
        #The objective is the initial material content minus the percent of this content to be replaced
        mat_comp_obj[nrow(mat_comp_obj),"Objective"] <- mat_i_mc_component[rpd_mat,subc]/sum(mat_i_mc_component[,subc])*(1-sum(subset(rpl_rates,Subcomponent==subc & `Replaced material`==rpd_mat)["Replacement rates"]))
      }
    }
    for (i in 1:nrow(mat_comp_obj)){
      #subcomponent: Subcomponent to be edited
      subcomponent <- mat_comp_obj[i,"Subcomponent"]
      #rpd_mat: Replaced material in the subcomponent
      rpd_mat <- mat_comp_obj[i,"Replaced material"]
      #rpl_obj: is the final material content objective of the replaced material in subcomponent
      rpl_obj <- mat_comp_obj[i,"Objective"]
      #rpd_wgt is the weight of replaced material to substitute at model_year y
      #It is based on curent relative material content of rpd in subcomponent and the replacing objective.
      rpd_wgt <- (mat_f_mc_component[rpd_mat, subcomponent]/sum(mat_f_mc_component[, subcomponent])-rpl_obj)/(lw_last_yr-model_year+1)*sum(mat_f_mc_component[, subcomponent])
      #rpg_mat_l is the list of replacing material
      rpg_mat_l <- subset(rpl_rates,Subcomponent==subcomponent & `Replaced material`==rpd_mat)$`Replacing material`
      for (rpg_mat in rpg_mat_l){
        #sub_fac: Substitution factor of rpd_mat by rpg_mat in subcomponent
        sub_fac <- do.call(sub_fac_f,list(rpd_mat = rpd_mat, rpg_mat = rpg_mat, component = subcomponent))
        #rpg_mat_ratio: rpg_mat_ratio is the ratio of rpg_mat to substitute rpd_mat compared to the other replacing materials
        rpg_mat_ratio <- subset(rpl_rates,Subcomponent==subcomponent & `Replaced material`==rpd_mat & `Replacing material`==rpg_mat)$`Replacement rates`/
          sum(subset(rpl_rates,Subcomponent==subcomponent & `Replaced material`==rpd_mat)$`Replacement rates`)
        #rpd_wgt_rpg is the weight of rpd subsitued by rpg
        rpd_wgt_rpg <- rpd_wgt*rpg_mat_ratio
        #Replacement: rpl_ratio is a ratio against the total subcomponent weight
        mat_f_mc_component[rpd_mat, subcomponent] <- mat_f_mc_component[rpd_mat, subcomponent] - rpd_wgt_rpg
        #Increases the replacing material content
        mat_f_mc_component[rpg_mat, subcomponent] <- mat_f_mc_component[rpg_mat, subcomponent] + rpd_wgt_rpg*sub_fac
      }
    }
  }
  return(mat_f_mc_component)
}
