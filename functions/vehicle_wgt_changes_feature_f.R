#' vehicle_wgt_changes_feature_f
#' Function: Apply weight changes due to feature content to vehicle material and component compositions
#' @export
vehicle_wgt_changes_feature_f = function(vehicle,model_year,mat_mc_component,feature_wgt=NA){
 attribute_f("vehicle_wgt_changes_feature_f")
  mat_f_mc_component <- mat_mc_component
  #Scenarios built from MacKenzie et al. 2014
  if (feature_wgt=="def"){
    feat_ann_wgt_inc <- 3.1
  } else if(feature_wgt=="n"){
    feat_ann_wgt_inc <- 0
  } else if(feature_wgt=="high"){
    feat_ann_wgt_inc <- 6.4
  }
  #Assumption: Weight increase due to features only changes "Other" in "Interior"
  mat_f_mc_component["Other","Interior"] <- mat_mc_component["Other","Interior"]+feat_ann_wgt_inc
  return(mat_f_mc_component)
}
