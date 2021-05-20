#' fleet_mfa_f
#' Function: Calculates material flow associated with LDVs
fleet_mfa_f <- function(rec_scen=NA,first_yr = NA,last_yr = NA) {
  #Assign default value
  attribute_f("fleet_mfa_f")
  #Input files
  material_dt <- get_input_f(input_name = 'model_matching_material')
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  nw_scrap_rt  <- get_input_f(input_name = 'new_scrap_rate')
  prim_sec_share  <- get_input_f(input_name = 'prim_sec_share')
  recovery_rt <- do.call(rec_rate_f,list(rec_scen=rec_scen))
  #Functions' Outputs
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet_vint_stock_f_res[["fleet_vint_scrap"]]
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_mc_dt <- vehicle_module_f_res[["fleet_mc_dt"]]
  fleet_mc_dt_year_list <- unique(fleet_mc_dt$Model_year)
  #Other parameters
  size_l <- c("Car", "Light truck")
  #Scrap input per secondary material: Represent the losses between the recovered materials and the secondary material supply
  scrap_input = 1
  #Output files
  #mat_emb is the matrix of material embodied in the new vehicles
  mat_emb <- matrix(0,ncol = length(first_yr:last_yr),nrow = length(material_dt$Own),dimnames = list(material_dt$Own,first_yr:last_yr))
  #mat_dem is the matrix of material demanded in the manufacturing phase of the new vehicles (include new scrap loss)
  mat_dem <- mat_emb[-which(rownames(mat_emb)=="Other"),]
  #mat_scrap is the matrix of scrapped materials
  mat_scrap <- matrix(0,ncol = length(first_yr:last_yr),nrow = length(material_dt$Own),dimnames = list(material_dt$Own,first_yr:last_yr))
  #mat_rec is the matrix of recovered material (e.g. recovered from scrapped materials)
  rec_mat_l <- union(material_dt$Own,unique(recovery_rt$`Recovered material`))
  mat_rec <- matrix(0, ncol = length(first_yr:last_yr), nrow = length(rec_mat_l),dimnames = list(rec_mat_l,first_yr:last_yr))
  #mat_int_sec is the matrix of internal secondary material acquired from the recovered materials vehicles
  mat_int_sec <- mat_rec[which(rownames(mat_rec)%in%material_dt$Own),]
  #mat_prim is the matrix of primary material production
  mat_prim <- mat_dem
  #mat_sec is the matrix of secondary material supply (internal and external)
  mat_sec <- mat_dem
  #Loop for size
  for (size in size_l) {
    #Loop for technology
    for (techno in unique(vh_techno$Own)) {
      #Extract material composition
      tmp_fleet_mc_dt <- subset(fleet_mc_dt,Size==size & Technology == techno,c(Model_year,Material,Value))
      matrix_mt_comp <- acast(tmp_fleet_mc_dt, Material ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
      #Extract the technology-specific vitanging stock
      new_sales <- subset(fleet_vint_stock, Size==size & Technology == techno & Year%in%c(first_yr:last_yr) & Age==0)
      matrix_new <- diag(x=new_sales$Value,nrow=length(new_sales$Year),ncol=length(new_sales$Year))
      dimnames(matrix_new) <- list(new_sales$Year,new_sales$Year)
      #Calculate matrix of embodied material
      tmp_matrix_emb <- matrix_mt_comp[rownames(mat_emb),as.character(first_yr:last_yr)] %*% matrix_new[as.character(first_yr:last_yr),as.character(first_yr:last_yr)]
      mat_emb <- mat_emb + tmp_matrix_emb[rownames(mat_emb),colnames(mat_emb)]
      #Extract the stock of scrapped vehicles
      scrap <- subset(fleet_vint_scrap, Size==size & Technology == techno & Year%in%c(first_yr:last_yr))
      #Create model_year. Assumption: Because no material composition later than historical, we assume all older vehicles to have this limit age.
      scrap[,"Model_year"] <- sapply(scrap[,"Year"] - scrap[,"Age"],function(x) ifelse(x < min(fleet_mc_dt_year_list),min(fleet_mc_dt_year_list),x))
      #Aggregate value. Add values for last_yr (no scrap)
      scrap <- scrap %>% subset(select=c(Year,Model_year,Value)) %>% aggregate(formula = Value ~ Year + Model_year,data=.,FUN = sum)
      #Loop for year
      matrix_scrap <- acast(scrap, Model_year ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
      tmp_matrix_scrap <- matrix_mt_comp[,rownames(matrix_scrap)] %*% matrix_scrap
      mat_scrap[,colnames(tmp_matrix_scrap)] <- mat_scrap[,colnames(tmp_matrix_scrap)] + tmp_matrix_scrap[rownames(mat_scrap),]
    }
  }
  #Calculate the material demand from embodied materials.
  #mat_demand_nw_scrap is a diagonal matrix. Each value represents the quantity of inputs required to get 1 output of material
  mat_demand_nw_scrap <- diag(x=1/(1-nw_scrap_rt$Rate),nrow=length(nw_scrap_rt$Material),ncol=length(nw_scrap_rt$Material))
  dimnames(mat_demand_nw_scrap) <- list(nw_scrap_rt$Material,nw_scrap_rt$Material)
  mat_dem <- mat_demand_nw_scrap[rownames(mat_dem),rownames(mat_dem)] %*% mat_emb[rownames(mat_dem),]
  #Calculate the quantity of recovered material from new scrap material
  #mat_new_scrap_rate is a diagonal matrix. Each value represents the quantity of new scrap generated from 1 kg of input
  mat_new_scrap_rate <- diag(x=nw_scrap_rt$Rate,nrow=length(nw_scrap_rt$Material),ncol=length(nw_scrap_rt$Material))
  dimnames(mat_new_scrap_rate) <- list(nw_scrap_rt$Material,nw_scrap_rt$Material)
  mat_rec[rownames(mat_dem),] <- mat_rec[rownames(mat_dem),] + mat_new_scrap_rate[rownames(mat_dem),rownames(mat_dem)] %*% mat_dem
  #Calculate the quantify of recovered material from scrapped material. Do not forget to addition on top of existing values
  for (y in first_yr:last_yr) {
    mat_recovery_rate <- acast(data=subset(recovery_rt,Year==y), `Recovered material` ~ `Scrapped material` , value.var='Value',fun.aggregate=sum, margins=FALSE)
    mat_rec[rownames(mat_recovery_rate), as.character(y)] <- mat_rec[rownames(mat_recovery_rate), as.character(y)] + mat_recovery_rate %*% mat_scrap[colnames(mat_recovery_rate),as.character(y)]
  }
  #Calculate the internal secondary material recovery matrix from recovered materials
  mat_int_sec <- mat_rec[rownames(mat_int_sec),] / scrap_input
  #Limit the interal secondary material to the material demand for all materials except aluminum related ones.
  tmp_mat_list <- grep("Aluminum",rownames(mat_dem),value =TRUE, invert = TRUE)
  mat_int_sec[tmp_mat_list,][(mat_dem[tmp_mat_list,] - mat_int_sec[tmp_mat_list,])<0] <- mat_dem[tmp_mat_list,][(mat_dem[tmp_mat_list,] - mat_int_sec[tmp_mat_list,])<0]
  #For aluminum, if more internal recovery of wrought than demand of wrought, we assume it becomes cast
  wr_alu_name <- "Wrought Aluminum"
  cast_alu_name <- "Cast Aluminum"
  #If surplus of wrought aluminum of secondary materials compared to demand. This surplus goes to cast aluminum and adjust wrought aluminum internal
  mat_int_sec[cast_alu_name,][(mat_dem[wr_alu_name,] - mat_int_sec[wr_alu_name,])<0] <- mat_int_sec[cast_alu_name,][(mat_dem[wr_alu_name,] - mat_int_sec[wr_alu_name,])<0] + (mat_dem[wr_alu_name,]-mat_int_sec[wr_alu_name,])[(mat_dem[wr_alu_name,] - mat_int_sec[wr_alu_name,])<0]
  mat_int_sec[wr_alu_name,][(mat_dem[wr_alu_name,] - mat_int_sec[wr_alu_name,])<0] <- mat_dem[wr_alu_name,][(mat_dem[wr_alu_name,] - mat_int_sec[wr_alu_name,])<0]
  #Adjust cast aluminum. If more internal secondary cast than deman, adj with demand
  mat_int_sec[cast_alu_name,][(mat_dem[cast_alu_name,] - mat_int_sec[cast_alu_name,])<0] <- mat_dem[cast_alu_name,][(mat_dem[cast_alu_name,] - mat_int_sec[cast_alu_name,])<0]
  #Calculate the external material input matrix. Could be either from primary material or from external secondary
  mat_ext_input <- (mat_dem - mat_int_sec[rownames(mat_dem),])
  #Calculate the primary material input matrix
  mat_prim_rate <- diag(x=prim_sec_share$Primary,nrow=length(prim_sec_share$Material),ncol=length(prim_sec_share$Material))
  dimnames(mat_prim_rate) <- list(prim_sec_share$Material,prim_sec_share$Material)
  mat_prim <- mat_prim_rate[rownames(mat_prim),rownames(mat_ext_input)] %*% mat_ext_input
  #Calcualte the external secondary material input matrix.
  #Calculate the secondary material input matrix
  mat_ext_sec <- mat_ext_input - mat_prim[rownames(mat_ext_input),]
  mat_sec <- mat_ext_sec[rownames(mat_sec),] + mat_int_sec[rownames(mat_sec),]
  #Create matrix with primary and secondary shares
  mat_prim_share <- mat_prim / mat_dem[rownames(mat_prim),]
  mat_sec_share <- mat_sec / mat_dem[rownames(mat_sec),]
  #Output file
  dt_col <- c("Name","Description","Unit","Material","Year","Value")
  fleet_mfa_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #mat_l is the list of all matrices to combine
  mat_l<-list(mat_emb=list(Name="emb",Description="Material embodied in new vehicle sales",Unit="kg"),
              mat_dem=list(Name="dem",Description="Demand in material for manufacturing of new vehicle",Unit="kg"),
              mat_scrap=list(Name="scrap",Description="Scrapped material from scrapped vehicles",Unit="kg"),
              mat_rec=list(Name="rec",Description="Recovered material from scrapped vehicles",Unit="kg"),
              mat_int_sec=list(Name="sec_int",Description="Internal secondary material recovered from scrapped vehicles",Unit="kg"),
              mat_ext_sec=list(Name="sec_ext",Description="External secondary material recovered from other industries",Unit="kg"),
              mat_prim=list(Name="prim",Description="Primary material demand",Unit="kg"),
              mat_prim_share=list(Name="prim_share",Description="Primary material share in material demand",Unit="Dimensionless"),
              mat_sec_share=list(Name="sec_share",Description="Secondary material share in material demand",Unit="Dimensionless"))
  for (mat_name in names(mat_l)){
    tmp_stock_dt <- as.data.frame(get(mat_name)) %>% cbind(Material=rownames(get(mat_name)),stringsAsFactors = FALSE) %>% gather("Year","Value",-Material,convert=TRUE) %>% cbind(Name=mat_l[[mat_name]]$Name,Description=mat_l[[mat_name]]$Description,Unit=mat_l[[mat_name]]$Unit,stringsAsFactors = FALSE)
    fleet_mfa_dt <- rbind(fleet_mfa_dt,tmp_stock_dt)
  }
  results<-list(fleet_mfa_dt=fleet_mfa_dt)
  return(results)
}
