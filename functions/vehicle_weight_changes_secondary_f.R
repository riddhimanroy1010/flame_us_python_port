#' vehicle_weight_changes_secondary_f
#' Function: Returns the component material composition of a vehicle after secondary weight changes
#' @export
vehicle_weight_changes_secondary_f <- function(vehicle,model_year,mat_mc_component,sms_coef_rand=NA,sms_ratio=NA,mat_i_mc_component=0,weight_iteration="n"){
  attribute_f("vehicle_weight_changes_secondary_f")
  #input files
  sms_coef  <- get_input_f(input_name = 'coef_alonso_equation')
  comp_dt <- get_input_f(input_name = 'model_matching_component')
  #Other parameters
  if (weight_iteration=="n"){
    mat_i_mc_component <- acast(data=subset(vehicle$material_component_composition, Model_year==model_year-1), Material ~ Subcomponent, value.var='Weight',fun.aggregate=sum, margins=FALSE)
  }
  prim_sav <- sum(mat_i_mc_component) - sum(mat_mc_component)
  #Output files
  mat_f_mc_component <- mat_mc_component
  #subcomp_tbc is the list of the subcomponents to consider
  subcomp_tbc <- subset(sms_coef,Subcomponent%in%colnames(mat_f_mc_component) & !Subcomponent%in%subset(comp_dt,`Own component`=="Powertrain")$`Own subcomponent`)$Subcomponent
  #Random generation of sms_coef
  if (sms_coef_rand=="y"){
    sms_coef[sms_coef$Subcomponent %in% subcomp_tbc,"Own"]<-
      rnorm(n=length(which(sms_coef$Subcomponent %in% subcomp_tbc)),
            mean=sms_coef$`Mass influence coef - mean`[sms_coef$Subcomponent %in% subcomp_tbc],
            sd=sms_coef$`Mass influence coef - std error`[sms_coef$Subcomponent %in% subcomp_tbc])*
      sms_ratio
  } else if (sms_coef_rand=="n") {
    sms_coef[sms_coef$Subcomponent%in%subcomp_tbc,"Own"] <- subset(sms_coef,Subcomponent%in%subcomp_tbc)[,"Mass influence coef - mean"]*sms_ratio
  }
  #mass_decomp is the mass decompounding coefficient associated with the considered subcomponents
  mass_decomp <- sum(subset(sms_coef,Subcomponent%in%subcomp_tbc)$Own)/(1-sum(subset(sms_coef,Subcomponent%in%subcomp_tbc)$Own))
  for (subcomp in subcomp_tbc) {
    mass_dec_comp <- sum(subset(sms_coef,Subcomponent==subcomp)$Own)/(1-sum(subset(sms_coef,Subcomponent%in%subcomp_tbc)$Own))
    mat_f_mc_component[,subcomp] <- mat_mc_component[,subcomp]*ifelse(sum(mat_mc_component[,subcomp])==0,0,(1-prim_sav*mass_dec_comp/sum(mat_mc_component[,subcomp])))
  }
  #Check secondry savings
  #(sum(mat_mc_component)-sum(mat_f_mc_component))/prim_sav-mass_decomp
  return(mat_f_mc_component)
}
