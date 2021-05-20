#' sub_fac_f
#' Function: Returns subsitution factors by component, replaced and replacing materials
#' @export
sub_fac_f <- function (rpd_mat, 
                       rpg_mat, 
                       component,
                       sub_fac_fun=NA){
  attribute_f(fun_name = "sub_fac_f")
  #Extract the substitution factor for the specified component, replaced and replacing materials and model of extraction (mean, min, max)
  sub_fac_dt  <- get_input_f(input_name = 'substitution_factor')
  comp_dt <- get_input_f(input_name = 'model_matching_component')
  #Change character in function
  fun <- get(sub_fac_fun)
  #Changes "HSS" in "HSS/AHSS". Not AHSS.
  sub_fac_dt$`Replaced material`[which(sub_fac_dt$`Replaced material`=="HSS")]<-"HSS/AHSS"
  sub_fac_dt$`Replacing material`[which(sub_fac_dt$`Replacing material`=="HSS")]<-"HSS/AHSS"
  sub_fac_dt$`Replacing material`[which(sub_fac_dt$`Replacing material`=="AHSS")]<-"HSS/AHSS"
  sub_fac<-aggregate(x=sub_fac_dt$`Substitution ratio`, 
                     by = sub_fac_dt[,c("Replaced material", "Replacing material", "Component")], 
                     FUN = fun)
  if (any(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])
          &sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])
          &sub_fac$Component==component)){
    sub_ratio<-sub_fac$x[which(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                                 sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])&
                                 sub_fac$Component==component)]
  } else if (any(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                                    sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])&
                                    sub_fac$Component==comp_dt$`Own component`[which(comp_dt$`Own subcomponent`==component)])){
    sub_ratio<-sub_fac$x[which(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                                 sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])&
                                 sub_fac$Component==comp_dt$`Own component`[which(comp_dt$`Own subcomponent`==component)])]
  } else if (any(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                 sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])&
                 sub_fac$Component=="General")){
    sub_ratio<-sub_fac$x[which(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                                 sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))])&
                                 sub_fac$Component=="General")]
  } else {
    sub_ratio<-fun(sub_fac$x[which(sub_fac$`Replaced material`==unique(sub_fac$`Replaced material`[which(sapply(sub_fac$`Replaced material`, function(x) grepl(x, rpd_mat, ignore.case = TRUE)))])&
                                 sub_fac$`Replacing material`==unique(sub_fac$`Replacing material`[which(sapply(sub_fac$`Replacing material`, function(x) grepl(x, rpg_mat, ignore.case = TRUE)))]))])
  }
return(sub_ratio)
  }
