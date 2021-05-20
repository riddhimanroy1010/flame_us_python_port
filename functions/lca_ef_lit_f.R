#' lca_ef_lit_f
#' Function: Returns GHG emission factors from literature
#' @export
lca_ef_lit_f<-function (techno="ns",
                        ghg = NA,
                        FCV_bat_t=NA,
                        HEV_bat_t=NA,
                        BEV_bat_t = NA,
                        PHEV_bat_t = NA,
                        ef_bat_mdl = NA){
  attribute_f("lca_ef_lit_f")
  #Input files
  lca_process <- get_input_f(input_name = 'lca_process')
  conv <- get_input_f(input_name = 'conversion_units')
  fuel_specs <- get_input_f(input_name = 'fuel_specs')
  EF_lit <- get_input_f(input_name = 'EF_lit_review')
  lca_process <- subset(lca_process,Source=="lit")
  #Get emission factors for battery production and assembly
  techno_tbc <- c("BEV100","BEV300","PHEV20","PHEV40","HEV","FCV")
  for (techno in techno_tbc){
    if (grepl(pattern="[[:digit:]]{1}",techno)){
      tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
    } else{
      tmp_techno <- techno
    }
    bat_t_l <- grep("bat_t",ls(),value=TRUE)
    if (any(grepl(tmp_techno,bat_t_l))){
      bat_name <- get(grep(tmp_techno,bat_t_l,value=TRUE))
      if (any(grepl(pattern=" ",bat_name))){
        bat_type <- substring(bat_name,1,as.numeric(regexpr(pattern=" ",bat_name))-1)
        anode_type <- substring(bat_name,as.numeric(regexpr(pattern=" ",bat_name))+1,nchar(bat_name))
      } else {
        bat_type<-bat_name
        anode_type<-""
      }
    } else {
      bat_name=NA
    }
    temp_dt <- lca_process[grepl("Battery",lca_process$Phase,ignore.case = TRUE),c("Unit","Phase","Process","Source")]
    temp_dt[,"Value"] <- 0
    temp_dt[,"Technology"]<-techno
    #Fill EF for battery production and assembly
    if (!is.na(bat_name)){
      #EF for Battery production
      #The default model uses Kim et al values data
      if (ef_bat_mdl=="def"){
        temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production","Value"] <- as.numeric(subset(EF_lit,Modele=="def"&Process=="Battery production",select=LCI))
        temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly","Value"] <- as.numeric(subset(EF_lit,Modele=="def"&Process=="Battery Assembly",select=LCI))
        #If GREET values:
        } else if (ef_bat_mdl=="greet"){
        #EF for Battery Assembly
        temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly","Value"] <- as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery Assembly"&grepl(bat_type,`Battery type`),select=LCI))
        #EF for battery production
        #Check if the exact battery name is available
        if (bat_name%in%EF_lit$`Battery type`[EF_lit$Reference=="GREET"]){
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production","Value"] <- as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery production"&`Functional Unit`=="kg"&Technology%in%c(NA,techno)&`Battery type`==bat_name,LCI))
        } else { #If not exact battery name, consider battery type
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production","Value"] <- as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery production"&`Functional Unit`=="kg"&Technology%in%c(NA,techno)&grepl(bat_type,`Battery type`),LCI))
        }
        #Extreme values for battery production. Assume assembly is included
        } else {
        #EF for Battery Assembly
        temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly","Value"] <- 0
        if (ef_bat_mdl=="low"){ef_bat_fun=min
        } else if (ef_bat_mdl=="high"){ef_bat_fun=max}
        temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production","Value"] <- ef_bat_fun(subset(EF_lit,Process=="Battery production"&`Functional Unit`=="kg"&grepl(bat_type,`Battery type`)&Technology%in%c(NA,techno),LCI))
        }
      }
    if (exists("lca_ef_lit",inherits = FALSE)){
      lca_ef_lit <- rbind(lca_ef_lit,temp_dt)
    } else {
      lca_ef_lit<-temp_dt
    }
  }
  #Format
  temp_dt[,"Technology"] <- NA
  #Aggregate EF
  lca_ef_lit <- rbind(lca_ef_lit,temp_dt)
  if(ghg!="CO2"){
    lca_ef_lit$Value <- 0
  }
  return(list(lca_ef_lit=lca_ef_lit))
}
