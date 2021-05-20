#' vehicle_weight_changes_powertrain_f
#' Function: Resize the powertrain components according to other resizments and MacKenzie equation
#' @export
vehicle_weight_changes_powertrain_f<-function(vehicle,model_year,mat_mc_component,pwtr_resz_ratio=NA,mat_i_mc_component=0,weight_iteration="n"){
  attribute_f("vehicle_weight_changes_powertrain_f")
  #input files
  mackenzie_coef_dt  <- get_input_f(input_name = 'coef_mackenzie_equation')
  #Other parameters
  #mat_i_mc_component: Material composition of previous model_year vehicle
  if (weight_iteration=="n"){
    mat_i_mc_component <- acast(data=subset(vehicle$material_component_composition, Model_year==model_year-1), Material ~ Subcomponent, value.var='Weight',fun.aggregate=sum, margins=FALSE)
  }
  mat_f_mc_component <- mat_mc_component
  if (grepl("BEV",vehicle$technology) | grepl("PHEV",vehicle$technology)){
    tmp_techno <- substring(vehicle$technology,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",vehicle$technology))-1)
  }else{
    tmp_techno <- vehicle$technology
  }
  #Format input files
  mackenzie_coef_dt <- mackenzie_coef_dt[mackenzie_coef_dt$Size==vehicle$size&(sapply(1:nrow(mackenzie_coef_dt),function(x)tmp_techno %in% unlist(strsplit(mackenzie_coef_dt$Technology[x],",")))),]
  #Assign the parameters estimates
  for (col in colnames(mackenzie_coef_dt[,-(1:4)])){
    assign(col,mackenzie_coef_dt[1,col])
  }
  cpt_dt <- do.call(vehicle_peak_power_f,list(vehicle=vehicle,model_year=model_year-1))
  #Total previous year peak power
  i_ppw <- switch(weight_iteration,
                  "n"=vehicle$specifications["peak_power",as.character(model_year-1)],
                  "y"=vehicle$specifications["peak_power",as.character(model_year)])
  i_cw <- sum(mat_i_mc_component)
  f_cw <- sum(mat_mc_component)
  if ("disp"%in%rownames(vehicle$specifications)){
    i_displ <- switch(weight_iteration,
                      "n"=vehicle$specifications["disp",as.character(model_year-1)],
                      "y"=vehicle$specifications["disp",as.character(model_year)])
    f_displ <- vehicle$specifications["disp",as.character(model_year)]
  } else {
    f_displ <- 1
    i_displ <- 1
  }
  i_z97 <- switch(weight_iteration,
                  "n"=vehicle$specifications["z97",as.character(model_year-1)],
                  "y"=vehicle$specifications["z97",as.character(model_year)])
  f_z97 <- vehicle$specifications["z97",as.character(model_year)]
  #Minimum achieved peak power with MacKenzie relation if full resizing
  min_f_ppw <- min(exp(Re(polyroot(c(beta2*log(f_cw/i_cw)+beta3*log(f_displ/i_displ)-beta1*log(i_ppw)-beta4*log(i_ppw)^2-log(f_z97/i_z97),beta1,beta4)))))
  f_ppw <- i_ppw+pwtr_resz_ratio*(min_f_ppw-i_ppw)
  #Adapt output file with weight ratio and powertrain resizing ratio
  for (cpt in cpt_dt$Component){
    f_cpt_wgt <- subset(cpt_dt,Component==cpt)[,"fixed_mass"]+(sum(mat_mc_component[,cpt])-subset(cpt_dt,Component==cpt)[,"fixed_mass"])*(f_ppw/i_ppw)
    mat_f_mc_component[,cpt] <- mat_mc_component[,cpt]*f_cpt_wgt/sum(mat_mc_component[,cpt])
  }
  return(list(mat_f_mc_component=mat_f_mc_component,final_peak_power=f_ppw))
}
