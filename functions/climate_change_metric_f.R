#' climate_change_metric_f
#' Function: Calculate alternative climate change metrics of an input matrix of GHG emissions
#' @import tidyr
#' @import reshape2
#' @export
climate_change_metric_f <- function(emission_matrix,
                                    ghg=NA,
                                    gwp_th=NA,
                                    crf_last_yr=NA,
                                    agtp_last_yr=NA,
                                    discount_rate=NA,
                                    ssp_scen=NA,
                                    ssp_mitigation_scen=NA,
                                    ghg_concentration_scen=NA){
  attribute_f("climate_change_metric_f")
  #Extract first year from the matrix
  first_yr <- min(as.numeric(colnames(emission_matrix)))
  #Define parameters to be used across methods.
  #First radiative efficiency of CO2 in [W.m-2.kg-1] based on background concentration
  mat_concentration <- matrix(NA,nrow=1,ncol=length(2010:2100),dimnames = list('ppb',2010:2100))
  if (ghg_concentration_scen=="constant"){
    mat_concentration[1,] <- 413*10^3
    #Fixed at the a_c02 provided by 
  } else if(grepl("ssp",ghg_concentration_scen)){
    ssp_diagnostic = get_input_f(input_name = "ssp_diagnostic_dt")
    #Get data
    concentration_dt <- ssp_diagnostic %>%
      subset(.,grepl(paste0("SSP",ssp_scen),SCENARIO) & grepl(ssp_mitigation_scen,SCENARIO,ignore.case = TRUE) & VARIABLE=="Diagnostics|MAGICC6|Concentration|CO2")
    unit_conv_factor <- switch(unique(concentration_dt$UNIT),"ppm"=10^3,"ppb"=1)
    #Select the appropriate scenario.
    #If default, then consider the scenario of concentration that is the closest to the median of the distribution in 2050
    if(grepl("def",ghg_concentration_scen)){
      mat_concentration[1,as.character(seq(2010,2100,10))] <- as.numeric(concentration_dt[which.min(abs(concentration_dt$`2050`-mean(concentration_dt$`2050`))),as.character(seq(2010,2100,10))])
    #If low, then consider the minimum concentration in 2050
    } else if (grepl("low",ghg_concentration_scen)){
      mat_concentration[1,as.character(seq(2010,2100,10))] <- as.numeric(concentration_dt[which.min(concentration_dt$`2050`),as.character(seq(2010,2100,10))])
    #If high, then consider the maximum concentration in 2050
    } else if (grepl("high",ghg_concentration_scen)){
      mat_concentration[1,as.character(seq(2010,2100,10))] <- as.numeric(concentration_dt[which.max(concentration_dt$`2050`),as.character(seq(2010,2100,10))])
    }
    #Perform linear regression
    mat_concentration[,as.character(2010:2100)] <- approx(x=seq(2010,2100,10),y=mat_concentration[1,as.character(seq(2010,2100,10))],method="linear",xout=2010:2100)$y*unit_conv_factor
  } else if(grepl("RCP",ghg_concentration_scen)){
    rcp_conc_dt = get_input_f(input_name = "rcp_concentration_dt")
    tmp_mat <- acast(data=subset(rcp_conc_dt,Year%in%c(2010:2100) & Background==ghg_concentration_scen), Unit ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    mat_concentration[,colnames(tmp_mat)] <- tmp_mat*10^3
  }
  #Convert concentrations into radiative efficiency
  mat_re_co2 <- matrix(0, ncol = max(crf_last_yr,agtp_last_yr) - first_yr + 1 , nrow = 1, dimnames = list("RE",first_yr:max(crf_last_yr,agtp_last_yr)))
  #Equation derived from IPCC AR5 page 8-SM-16.
  mat_re_co2[1,colnames(mat_re_co2)] <- 5.35/mat_concentration["ppb",colnames(mat_re_co2)]*28.97/44.01*(10^9)/(5.1352*10^18)
  #a_co2 = 1.7517*10^(-15) #Radiative efficiency of CO2 according to IPCC
  #CO2 atmospheric load
  a0 = 0.2173 #First term coefficient [unitless]
  a1 = 0.2240 #Second term coefficient [unitless]
  a2 = 0.2824 #Third term coefficient [unitless]
  a3 = 0.2763 #Fourth term coefficient [unitless]
  tau1 = 394.4 #Second term time scale [years]
  tau2 = 36.54 #Third term time scale [years]
  tau3 = 4.304 #Fourth term time scale [years]
  if(ghg!="CO2"){
    a_ghg <- switch(ghg,
               "CH4"=1.2767e-13,
               "N2O"=3.8453e-13)
    tau_ghg <- switch(ghg,
                    "CH4"=12.4,
                    "N2O"=121)
    f_ghg <- switch(ghg,
                "CH4"=0.65,
                "N2O"=-0.071874)
  }
  #Calculate GWP
  #
  if(ghg=="CO2"){
    mat_gwp <- emission_matrix
  } else {
    #Calculate AGWP of CO2
    agwp_co2_f <- function(t,year){
      exp_term <- function(t,ai,taui){return(ai*taui*(1-exp(-t/taui)))}
      agwp_co2 =  mat_re_co2[1,as.character(year)] * (a0*t + exp_term(t,a1,tau1) + exp_term(t,a2,tau2) + exp_term(t,a3,tau3))
      return(agwp_co2)
    }
    #Calculate AGWP of the GHG
    agwp_ghg_f <- function(t){
      return((1+f_ghg)*a_ghg*tau_ghg*(1-exp(-t/tau_ghg)))
    }
    #Calculate GWP
    gwp_ghg_f <- function(t,year){
      return(agwp_ghg_f(t)/agwp_co2_f(t,year))
    }
    #Create matrix of GWP (unit = kg CO2 eq.)
    mat_gwp_coef <- matrix(0, ncol = ncol(emission_matrix) , nrow = ncol(emission_matrix), dimnames = list(colnames(emission_matrix),colnames(emission_matrix)))
    diag(mat_gwp_coef) <- sapply(colnames(mat_gwp_coef),function(x)gwp_ghg_f(t=gwp_th,year=as.numeric(x)))
    #Calculate matrix of annual radiative forcing (unit = W.m-2.yr)
    mat_gwp <- emission_matrix %*% mat_gwp_coef
  }
  #Calculate Cumulative Radiative Forcing
  #Create a function that calculate the Marginal Radiative Forcing of an emission pulse of CO2 (also called Dynamic Characterization factors by Levasseur et al)
  if (ghg=="CO2"){
    marginal_rf_f <- function(t,year){
      exp_term <- function(t,ai,taui){return(ai*taui*(exp(-(t-1)/taui)-exp(-t/taui)))}
      if (t<=0){
        marginal_rf = 0
      } else{
        marginal_rf = mat_re_co2[1,as.character(year)] * (a0 + exp_term(t,a1,tau1) + exp_term(t,a2,tau2) + exp_term(t,a3,tau3))
      }
      return(marginal_rf)
    }
  } else {
    marginal_rf_f <- function(t,year){
      if (t<=0){
        marginal_rf = 0
      } else{
        marginal_rf = (1+f_ghg)*a_ghg*tau_ghg*(exp(-(t-1)/tau_ghg)-exp(-t/tau_ghg))
      }
      return(marginal_rf)
    }
  }
  #Create matrix of Marginal Radiative Forcing (unit = W.m-2.yr.kg-1)
  mat_marginal_rf <- matrix(0, ncol = crf_last_yr - first_yr + 1 , nrow = ncol(emission_matrix), dimnames = list(colnames(emission_matrix),first_yr:crf_last_yr))
  for (col in colnames(mat_marginal_rf)){
    mat_marginal_rf[,col] <- vapply(as.numeric(rownames(mat_marginal_rf)),function(x)marginal_rf_f(t=as.numeric(col)-x+1,year=col),numeric(1))
  }
  #Calculate matrix of annual radiative forcing (unit = W.m-2.yr)
  mat_crf <- emission_matrix %*% mat_marginal_rf
  
  #Calculate Global Temperature Change
  #Define parameters of the climate response function
  c1 = 0.631 #First term component of the climate sensitivity[K.(W.m-2)-1]
  c2 = 0.429 #Second term component of the climate sensitivity[K.(W.m-2)-1]
  d1 = 8.4 #First term response time [years]
  d2 = 409.5 #Second term response time [year]
  #Create functions that calculates the Absolute Global Temperature change potential of a CO2 pulse emission at time t (in K.kg-1)
  if (ghg=="CO2"){
    agtp_f <- function(t,year){
      term1 <- function(t,ai,taui,dj){
        return((ai*taui)/(taui-dj)*(exp(-t/taui)-exp(-t/dj)))
      }
      term2 <- function(t,cj,dj){
        return(a0*cj*(1-exp(-t/dj))+cj*(term1(t,a1,tau1,dj)+term1(t,a2,tau2,dj)+term1(t,a3,tau3,dj)))
      }
      if (t<=0){
        agtp = 0
      } else{
        agtp = mat_re_co2[1,as.character(year)]*(term2(t,c1,d1)+term2(t,c2,d2))
      }
      return(agtp)
    }
  } else {
    agtp_f <- function(t,year){
      term1 <- function(t,tau_ghg,ci,di){
        return((tau_ghg*ci)/(tau_ghg-di)*(exp(-t/tau_ghg)-exp(-t/di)))
      }
      if (t<=0){
        agtp = 0
      } else{
        agtp = (1+f_ghg)*a_ghg*(term1(t,tau_ghg,c1,d1)+term1(t,tau_ghg,c2,d2))
      }
      return(agtp)
    }
  }
  
  #Create matrix of AGTP (unit = K.kg-1)
  mat_agtp <- matrix(0, ncol = agtp_last_yr - first_yr + 1 , nrow = ncol(emission_matrix), dimnames = list(colnames(emission_matrix),first_yr:agtp_last_yr))
  for (col in colnames(mat_agtp)){
    mat_agtp[,col] <- vapply(as.numeric(rownames(mat_agtp)),function(x)agtp_f(t=as.numeric(col)-x+1,year=col),numeric(1))
  }
  #Calculate matrix of annual global temperature change (unit = W.m-2.yr)
  mat_agt <- emission_matrix %*% mat_agtp
  
  #Calculate Dynamic Global Warming Impact (Sproul et al. 2019)
  dgwi_dt <- get_input_f(input_name = "dgwi_dt")
  dgwi_dt <- subset(dgwi_dt, Discount_rate==discount_rate & GHG==ghg)
  #Create matrix of DGWI
  mat_dgwi_coef <- matrix(0, ncol = ncol(emission_matrix) , nrow = ncol(emission_matrix), dimnames = list(colnames(emission_matrix),colnames(emission_matrix)))
  diag(mat_dgwi_coef) <- sapply(colnames(emission_matrix),function(x)ifelse(as.numeric(x) < min(dgwi_dt$Year),1,subset(dgwi_dt,Year==x)$Value))
  #Calculate Mean Present Value of Emissions using DGWI
  mat_dgwi <- emission_matrix %*% mat_dgwi_coef
  
  return(list(gwp=mat_gwp,crf=mat_crf,agt=mat_agt,dgwi=mat_dgwi,units=list(gwp="kg CO2 eq.",crf="W.m-2.yr",agt="K",dgwi="kg CO2 eq. (present value)")))
}
