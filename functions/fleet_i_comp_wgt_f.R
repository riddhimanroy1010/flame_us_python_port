#' fleet_i_comp_wgt_f
#' Function: Create the initial weights of components and subscomponents from GREET and EPA assumptions for all vehicle technologies
#' @import modelframework
#' @export
##Attributes: size=LDV size, 
##techno=LDV technology,
##bat_type= Type of EV battery
##li_cathode= Type of cathode material use for Li-ion battery
##wgt_scen_GREET=The Car size to consider in GREET. If 1=Car and SUV,if 2=Car and PUT, if 3=SUV and PUT, if 4= SUV and SUV
##mod_scen_GREET=The passenger vehicle model to consider in GREET
fleet_i_comp_wgt_f <- function(wgt_scen_GREET = NA,mod_scen_GREET = NA){
  attribute_f("fleet_i_comp_wgt_f")
  #input files
  vh_techno  <- get_input_f(input_name = 'model_matching_technology')
  comp_dt  <- get_input_f(input_name = 'model_matching_component')
  vh_comp_wgt_dist  <- get_input_f(input_name = 'greet_rel_cpt_wgt_2017')
  wgt_greet_dt  <- get_input_f(input_name = 'greet_wgt_2017')
  wgt_dt  <- get_input_f(input_name = 'epa_fleet_wgt_hist')
  conv  <- get_input_f(input_name = 'conversion_units')
  wt_subcomp  <- get_input_f(input_name = 'c2g_rel_subcpt_wgt')
  bat_fc_dt  <- get_input_f(input_name = 'greet_battery')
  #Called functions
  fleet_i_ev_bat_f_res <- do.call(fun_res_f,list(fun_name="fleet_i_ev_bat_f"))
  fleet_i_ev_bat <- fleet_i_ev_bat_f_res[["fleet_i_ev_bat"]]
  #Creation output
  #fleet_compo_wgt contains the weight of the components of the technology techno at size size
  dt_col <- c("Size","Technology","Component","Weight")
  fleet_compo_wgt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #fleet_subcompo_wgt cotnains the weight of the subcomponent of the technology techno at size size
  dt_col <- c("Size","Technology","Component","Subcomponent","Weight")
  fleet_subcompo_wgt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (size in c("Car","Light truck")){
    #Size_GREET:Model size to use in GREET
    if (size=="Light truck" & wgt_scen_GREET%in%c(1,4)|size=="Car" & wgt_scen_GREET%in%c(3,4)){ size_greet = "SUV"
    }else if (size=="Light truck" & wgt_scen_GREET%in%c(2,3)) { size_greet = "PUT"
    }else { size_greet="Car"}
    #Loop technology
    for (techno in unique(vh_techno$Own)){
      if (grepl("BEV",techno) | grepl("PHEV",techno)){
        range <- subset(vh_techno,Own==techno)$Range
        tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
      } else {
        range<-NA
        tmp_techno <- techno
      }
      techno_greet <- subset(vh_techno,Own==techno)[,"GREET1"]
      #range
      if (grepl("BEV",techno) | grepl("PHEV",techno)){
        range <- subset(vh_techno,Own==techno)$Range
      } else {
        range <- NA
      }
      #comp_wgt_dt contains the curb weight of the vehicle taken from GREET (wo battery) and other components
      comp_wgt_dt <- wgt_greet_dt[sapply(1:nrow(wgt_greet_dt),function(x)mod_scen_GREET %in% unlist(strsplit(wgt_greet_dt$Model[x],","))) & wgt_greet_dt$Size==size_greet,
                       c(which(colnames(wgt_greet_dt)%in%c("Unit","Data")),which(colnames(wgt_greet_dt)==techno_greet &(is.na(wgt_greet_dt[which(wgt_greet_dt$Data=="Range"),])|wgt_greet_dt[which(wgt_greet_dt$Data=="Range"),]%in%range)))]
      #vh_comp contains the component weight distribution for technology
      vh_comp<-vh_comp_wgt_dist[vh_comp_wgt_dist$Model==mod_scen_GREET & vh_comp_wgt_dist$Size==size_greet,
                     c(which(colnames(vh_comp_wgt_dist)%in%c("Vehicle component")),which(colnames(vh_comp_wgt_dist)==techno_greet & (is.na(vh_comp_wgt_dist[which(vh_comp_wgt_dist$`Vehicle component`=="Range"),])|vh_comp_wgt_dist[which(vh_comp_wgt_dist$`Vehicle component`=="Range"),]%in%range)))]
      #Extract own components
      component <- unique(subset(wt_subcomp,sapply(1:nrow(wt_subcomp),function(x)techno_greet%in%unlist(strsplit(Technology,",")[x])))[,"Component"])
      #Extract own subcomponents
      subcomponent <- unique(subset(wt_subcomp,sapply(1:nrow(wt_subcomp),function(x)techno_greet%in%unlist(strsplit(Technology,",")[x])))[,"Subcomponent"])
      #Create temporary dataframe
      tmp_compo_wgt <- data.frame("Component"=0, "Weight"=NA, stringsAsFactors = FALSE)
      tmp_subcompo_wgt <- data.frame("Component"=0,"Subcomponent"=0, "Weight"=NA, stringsAsFactors = FALSE)
      tmp_compo_wgt[1:(length(component)+1),"Component"] <- c(component,"Total")
      tmp_subcompo_wgt[1:(length(subcomponent)+1),"Subcomponent"] <- c(subcomponent,"Total")
      tmp_subcompo_wgt$Component <- c(wt_subcomp[which(sapply(1:nrow(wt_subcomp),function(x)techno_greet %in% unlist(strsplit(wt_subcomp$Technology[x],",")))), "Component"],"Total")
      #For the curb weight, if EPA data available for technology. Consider EPA data
      if (nrow(subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="epa" & Model_year==2016))==1) {
        CW_in_kg <- subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="epa" & Model_year==2016)$Value
        CW <- CW_in_kg*conv["lb","1 kg"]
        source_CW<-"EPA"
        #If EPA not available, consider Greet Data
      } else {
        CW <- subset(comp_wgt_dt,Data=="Curb weight (w/o bat)")[,techno_greet]
        source_CW<-"GREET"
      }
      #Compile Battery Lead-acid, fluids and wheels weight (GREET data)
      for (comp in c("Battery Lead-Acid","Fluids","Wheels")){
        tmp_compo_wgt[tmp_compo_wgt$Component==comp,"Weight"] <- sum(subset(comp_wgt_dt,Data == comp)[,techno_greet])
      }
      #Compile EV Battery weight (GREET data)
      if (nrow(subset(fleet_i_ev_bat,Size==size & Technology==techno))!=0){
        #Battery weight is already in lb
        tmp_compo_wgt[tmp_compo_wgt$Component=="EV Battery","Weight"] <- subset(fleet_i_ev_bat,Size==size & Technology==techno)[,"Weight"]
      }
      #If FCV, powertrain system is the fuel cell stack and onboard storage
      if (techno=="FCV"){
        #fuel_cell_stack_weight is weight of fuel cell stack in FCV in lb
        fuel_cell_stack_weight <- subset(comp_wgt_dt,Data == "Fuel cell stack")[,techno_greet]/subset(bat_fc_dt,Subcomponent=="Fuel cell stack")[,"2015"]*conv["lb","1 kg"]
        tmp_subcompo_wgt[tmp_subcompo_wgt$Subcomponent=="Fuel cell stack","Weight"] <- fuel_cell_stack_weight
        #Fill fuel cell auxiliaries weight
        tmp_subcompo_wgt[tmp_subcompo_wgt$Subcomponent=="Fuel cell auxiliaries","Weight"] <- subset(comp_wgt_dt,Data == "Fuel cell auxiliaries")[,techno_greet]
        #Fill compo wgt
        tmp_compo_wgt[tmp_compo_wgt$Component=="Powertrain","Weight"] <- sum(as.numeric(subset(tmp_subcompo_wgt,Component=="Powertrain")[,"Weight"]))
      }
      #Update curb weight if GREET sources
      if (source_CW=="GREET"){
        CW <- CW + sum(as.numeric(subset(tmp_compo_wgt,Component%in%c("Battery Lead-Acid","EV Battery"))[,"Weight"]))
      }
      tmp_compo_wgt[tmp_compo_wgt$Component=="Total","Weight"] <- CW
      tmp_subcompo_wgt[tmp_subcompo_wgt$Subcomponent=="Total","Weight"] <- CW
      #CW_rem is the remaining curb weight to distribute
      CW_rem <- CW - sum(as.numeric(subset(tmp_compo_wgt,Component%in%c("Battery Lead-Acid","EV Battery","Fluids","Wheels"))[,"Weight"]))
      #Fill the other components (except Glider)
      for (comp in tmp_compo_wgt$Component[is.na(tmp_compo_wgt$Weight)&tmp_compo_wgt$Component!="Glider"]) {
        tmp_compo_wgt[tmp_compo_wgt$Component == comp, "Weight"] <- sum(subset(vh_comp,`Vehicle component`%in%unique(subset(comp_dt,`Own component`==comp)[,"GREET"]))[,techno_greet])*CW_rem
      }
      #Fill the Glider weight
      tmp_compo_wgt[tmp_compo_wgt$Component=="Glider","Weight"] <- CW-sum(subset(tmp_compo_wgt,Component!="Total")[,"Weight"],na.rm=TRUE)
      #subcomponent weight for subcomponent not already filled (Only case: FCV for fuel cell)
      for (subcomp in tmp_subcompo_wgt$Subcomponent[is.na(tmp_subcompo_wgt$Weight)]) {
        #Calculate subcomponent from total component weight and subcomponent distirbution
        tmp_subcompo_wgt$Weight[tmp_subcompo_wgt$Subcomponent==subcomp] <- subset(tmp_compo_wgt,Component==subset(comp_dt,`Own subcomponent` == subcomp)[,"Own component"])[,"Weight"]*
          subset(wt_subcomp,sapply(1:nrow(wt_subcomp), function(x)techno_greet%in%unlist(strsplit(Technology,",")[x]))& `Subcomponent` == subcomp)[,"Subcomponent weight distribution"]
      }
      #Compute relative weight of  components
      tmp_compo_wgt[, "Relative"] <- tmp_compo_wgt[,"Weight"]/CW
      #Compute relative weight of  subcomponents
      tmp_subcompo_wgt[, "Relative"] <- tmp_subcompo_wgt[,"Weight"]/CW
      #Format output file:Conversion in kg
      tmp_compo_wgt[,"Weight"]<-tmp_compo_wgt[,"Weight"]*conv["kg","1 lb"]
      tmp_subcompo_wgt[,"Weight"]<-tmp_subcompo_wgt[,"Weight"]*conv["kg","1 lb"]
      #Format: Add techno and size
      tmp_compo_wgt[,"Size"] <- size
      tmp_subcompo_wgt[,"Size"] <- size
      tmp_compo_wgt[,"Technology"] <- techno
      tmp_subcompo_wgt[,"Technology"] <- techno
      #Fill final output
      fleet_compo_wgt <- rbind(fleet_compo_wgt,tmp_compo_wgt)
      fleet_subcompo_wgt <- rbind(fleet_subcompo_wgt,tmp_subcompo_wgt)
    }
  }
  fleet_compo_wgt[,"Unit"] <- "kg"
  fleet_subcompo_wgt[,"Unit"] <- "kg"
  results<-list(fleet_compo_wgt=fleet_compo_wgt,fleet_subcompo_wgt=fleet_subcompo_wgt)
  return(results)
}

