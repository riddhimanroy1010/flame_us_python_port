#' elec_source_reg_ev_f
#' Function: DEPRECIATED. Calculate electricity sources mixes that are weighted by regional deployment of EVs
#' @importFrom reshape2 acast
#' @export
elec_source_reg_ev_f <- function(aeo_scen=NA,first_yr=NA,last_yr=NA){
  attribute_f("elec_source_reg_ev_f")
  #Function's outputs - Electricity use by region
  fleet_fuel_u_reg_f_res <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_reg_f"))
  reg_elec_use <- fleet_fuel_u_reg_f_res[["reg_elec_use"]]
  #Matrix of electricity use by region
  matrix_reg_elec_use <- acast(data=subset(reg_elec_use), Region ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  matrix_reg_elec_mix <- matrix_reg_elec_use %*% diag(x=1/colSums(matrix_reg_elec_use),nrow = ncol(matrix_reg_elec_use))
  colnames(matrix_reg_elec_mix) <- colnames(matrix_reg_elec_use)
  #Inputs - Electricity source by region
  us_elec_mix_hist  <- get_input_f(input_name = 'us_elec_mix_reg_hist')
  us_elec_mix_proj  <- get_input_f(input_name = 'us_elec_mix_reg_proj')
  last_hist_yr <- max(us_elec_mix_hist$Year)
  #Get regional sources matrix
  matrix_mix_hist <- acast(data=subset(us_elec_mix_hist, Year>=first_yr), paste(Census,"-",Source) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  matrix_mix_proj <- acast(data=subset(us_elec_mix_proj,Aeo_case==aeo_scen & Year > last_hist_yr), paste(Census,"-",Source) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  matrix_mix_reg <- cbind(matrix_mix_hist,matrix_mix_proj[rownames(matrix_mix_hist),])
  #Converting matrix
  conv_reg <- as.matrix(sapply(rownames(matrix_reg_elec_mix),function(x)as.numeric(grepl(x,rownames(matrix_mix_reg)))))
  rownames(conv_reg) <- rownames(matrix_mix_reg)
  #Obtain mix by region and source
  matrix_mix_reg_source <- matrix_mix_reg * (conv_reg %*% matrix_reg_elec_mix[colnames(conv_reg),])[rownames(matrix_mix_reg),colnames(matrix_mix_reg)]
  #Converting matrix
  conv_source <- t(as.matrix(sapply(unique(us_elec_mix_proj$Source),function(x)as.numeric(grepl(x,rownames(matrix_mix_reg_source))))))
  colnames(conv_source) <- rownames(matrix_mix_reg_source)
  #Calculate matrix by source
  matrix_mix <- conv_source %*% matrix_mix_reg_source[colnames(conv_source),]
  return(list(matrix_mix))
}
