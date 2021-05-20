#' fleet_technology_market_share_proj_f
#' Function: Creates prospective technology market shares and returns a matrix.
#' @import reshape2
#' @export
fleet_technology_market_share_proj_f <- function(market_share_source=NA,
                                                 market_share_adj_size=NA,
                                                 market_share_adj_size_an_grw=NA,
                                                 market_share_adj_size_mdl=NA,
                                                 market_share_adj_techno=NA,
                                                 market_share_adj_start_yr=NA,
                                                 aeo_scen=NA,
                                                 last_yr=NA){
  attribute_f("fleet_technology_market_share_proj_f")
  #Attribute
  if (market_share_adj_size_mdl=="lt_high"){
    market_share_adj_size_an_grw <- 0.005
    market_share_adj_size <- "Light truck"
  } else if (market_share_adj_size_mdl=="car_high"){
    market_share_adj_size_an_grw <- 0.005
    market_share_adj_size <- "Car"
  }
  #Inputs
  proj_sales_aeo  <- get_input_f(input_name = 'fleet_sales_proj_aeo')
  #last_hist_yr is the last year of historical data
  last_hist_yr <- 2020
  sales_dts <- subset(proj_sales_aeo,Aeo_case==aeo_scen & Year%in%c(last_hist_yr:last_yr))
  matrix_sales_proj <- acast(sales_dts, Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Create the original technological market share matrix from AEO data
  matrix_market_share <- matrix_sales_proj %*% diag(x=1/colSums(matrix_sales_proj),nrow=ncol(matrix_sales_proj),ncol=ncol(matrix_sales_proj))
  colnames(matrix_market_share) <- colnames(matrix_sales_proj)
  adj_mkt_share_mat <- matrix_market_share
  #Constant market share if specified
  if (market_share_source=="constant"){
    #Constant technological market share from last historical data onward
    adj_mkt_share_mat[,as.character((last_hist_yr+1):last_yr)] <- sapply((last_hist_yr+1):last_yr,function(year)matrix_market_share[,as.character(last_hist_yr)])
  }
  #Adjust the size market share if specified. market_share_adj_size should specify the size the adjust: "Car" or "Light truck"
  if (market_share_adj_size!="n"){
    type_name <- grep(market_share_adj_size,rownames(adj_mkt_share_mat),value=TRUE)
    #Get the annual relative growth in market_share_adj_size increase 
    ann_grw_rt <- market_share_adj_size_an_grw
    #Assumption: Keep the same internal ratio of technology market share
    adj_mkt_share_mat[type_name,] <- (adj_mkt_share_mat[type_name,,drop=FALSE] %*% 
                                        diag(x=1/colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
      diag(x=sapply(sum(adj_mkt_share_mat[type_name,as.character(last_hist_yr)])+ann_grw_rt*0:(last_yr-last_hist_yr),function(x)ifelse(x<=1,x,1)),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
    #Adjust the market shares of the other size
    #Assumption: Keep the same internal ratio of vehicles
    adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name,] <- (adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE] %*% 
                                                                        diag(x=1/colSums(adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
      diag(x=round(1-colSums(adj_mkt_share_mat[type_name,,drop=FALSE]),digits=7),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
  }
  #Adjust the technology market share if specified
  if (market_share_adj_techno=="bnef"){
    #Input
    bnef_ev  <- get_input_f(input_name = 'bnef_ev')
    #
    techno <- "BEV300"
    size <- c("Car","Light truck")
    type_name <- unlist(lapply(size,function(x)paste0(x,"_",techno)))
    #Assumption: Keep the same ratio of untouched vehicles for each size
    adj_mkt_share_mat[type_name,] <- (adj_mkt_share_mat[type_name,,drop=FALSE] %*% 
                                        diag(x=1/colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
      diag(x=sapply(last_hist_yr:last_yr,function(x)subset(bnef_ev,US==ifelse(x<=max(bnef_ev$US),x,max(bnef_ev$US)))[,"EV % of new car sales"]/100),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
    #Adjust the market shares of the other vehicles
    #Assumption: Reduce ICEV-G proportionally to their size
    adj_techno <- c("Car_ICEV-G","Light truck_ICEV-G")
    adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%adj_techno,] <- (adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%adj_techno,,drop=FALSE] %*% 
                                                                        diag(x=1/colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%adj_techno,,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
      diag(x=round(1-colSums(adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%adj_techno,,drop=FALSE]),digits=7),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
  
  } else if (grepl("2035",market_share_adj_techno)){
    tmp_adj_mkt_share_mat <- adj_mkt_share_mat
    #
    techno <- strsplit(market_share_adj_techno,"_")[[1]][2]
    size_to_adjust <- c("Car","Light truck")
    for (size in size_to_adjust){
      #Get the name of types to adjust
      type_name <- unlist(lapply(size,function(x)paste0(x,"_",techno)))
      #Get the sum of the market shares of the types, relatively to the size market shares
      i_value <- sum(adj_mkt_share_mat[type_name,as.character(last_hist_yr)])/sum(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),as.character(last_hist_yr)])
      proj_values <- approx(x=c(market_share_adj_start_yr,2035), y=c(i_value,1), xout=seq(last_hist_yr+1,last_yr,1),yleft=i_value,yright=1, method = "linear")$y
      #Assumption: Keep the same internal ratio of the adjusted technologies, only adjust the technology market share into the fixed size market share.
      tmp_adj_mkt_share_mat[type_name,] <- (adj_mkt_share_mat[type_name,,drop=FALSE] %*% 
                                              diag(x=sapply(colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE]),function(x)ifelse(x>0,1/x,0)),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
        diag(x=proj_values,nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat)) %*%
        diag(x=colSums(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
      #Adjust the other technologies of same size proportionally
      tmp_adj_mkt_share_mat[!rownames(tmp_adj_mkt_share_mat)%in%type_name & grepl(size,rownames(tmp_adj_mkt_share_mat)),] <- (adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name & grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE] %*% 
                                                                                                                                diag(x=sapply(colSums(adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name & grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE]),function(x)ifelse(x>0,1/x,0)),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
        diag(x=round(colSums(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE])-colSums(tmp_adj_mkt_share_mat[type_name,,drop=FALSE]),digits=7),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
    }
    adj_mkt_share_mat <- tmp_adj_mkt_share_mat
  } else if (market_share_adj_techno=="wsj"){
    #Input
    wsj_ihs_ms  <- get_input_f(input_name = 'wsj_ihs_ms')
    size <- c("Car","Light truck")
    for (techno in c("PHEV","BEV","HEV","FCV")){
      if (techno =="PHEV"){
        techno_list = c("PHEV20","PHEV40")
        } else if (techno =="BEV"){
          techno_list = c("BEV100","BEV300")
        } else {
          techno_list = techno 
      }
      type_name <- unlist(lapply(size,function(x)paste0(x,"_",techno_list)))
      #Assumption: Keep the same ratio of untouched vehicles for each size
      adj_mkt_share_mat[type_name,as.character((last_hist_yr+1):last_yr)] <- (adj_mkt_share_mat[type_name,as.character((last_hist_yr+1):last_yr),drop=FALSE] %*% 
                                          diag(x=1/colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%type_name,as.character((last_hist_yr+1):last_yr),drop=FALSE]),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)) %*%
        diag(x=sapply((last_hist_yr+1):last_yr,function(x)subset(wsj_ihs_ms,Year==x)[,techno]),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)
    }
    icev_type_name <- unlist(lapply(size,function(x)paste0(x,"_",c("ICEV-G","CNG","FFV","ICEV-D"))))
    #Adjust the market shares of the other vehicles
    adj_mkt_share_mat[icev_type_name,as.character((last_hist_yr+1):last_yr)] <- (adj_mkt_share_mat[icev_type_name,as.character((last_hist_yr+1):last_yr),drop=FALSE] %*% 
                                                                        diag(x=1/colSums(adj_mkt_share_mat[icev_type_name,as.character((last_hist_yr+1):last_yr),drop=FALSE]),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)) %*%
      diag(x=round(1-colSums(adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%icev_type_name,as.character((last_hist_yr+1):last_yr),drop=FALSE]),digits=7),nrow=last_yr-last_hist_yr,ncol=last_yr-last_hist_yr)
    
  } else if (market_share_adj_techno!="n"){
    tmp_adj_mkt_share_mat <- adj_mkt_share_mat
    techno <- unlist(strsplit(market_share_adj_techno,split=","))
    size_to_adjust <- c("Car","Light truck")
    for (size in size_to_adjust){
      #Get the name of types to adjust
      type_name <- unlist(lapply(size,function(x)paste0(x,"_",techno)))
      #Get the sum of the market shares of the types to adjust at the last historical year
      i_value <- sum(adj_mkt_share_mat[type_name,as.character(last_hist_yr)])/sum(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),as.character(last_hist_yr)])
      proj_values <- c(rep(i_value,times=market_share_adj_start_yr-last_hist_yr),do.call(technology_market_share_diffusion_f,list(i_value=i_value,i_year=market_share_adj_start_yr,last_yr=last_yr)))
      #Assumption: Keep the same internal ratio of the adjusted technologies, only adjust the technology market share into the fixed size market share.
      tmp_adj_mkt_share_mat[type_name,] <- (adj_mkt_share_mat[type_name,,drop=FALSE] %*% 
                                          diag(x=sapply(colSums(adj_mkt_share_mat[rownames(adj_mkt_share_mat)%in%type_name,,drop=FALSE]),function(x)ifelse(x>0,1/x,0)),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
        diag(x=proj_values,nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat)) %*%
        diag(x=colSums(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE]),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
      #Adjust the other technologies of same size proportionally
      tmp_adj_mkt_share_mat[!rownames(tmp_adj_mkt_share_mat)%in%type_name & grepl(size,rownames(tmp_adj_mkt_share_mat)),] <- (adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name & grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE] %*% 
                                                                          diag(x=sapply(colSums(adj_mkt_share_mat[!rownames(adj_mkt_share_mat)%in%type_name & grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE]),function(x)ifelse(x>0,1/x,0)),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))) %*%
        diag(x=round(colSums(adj_mkt_share_mat[grepl(size,rownames(adj_mkt_share_mat)),,drop=FALSE])-colSums(tmp_adj_mkt_share_mat[type_name,,drop=FALSE]),digits=7),nrow=ncol(adj_mkt_share_mat),ncol=ncol(adj_mkt_share_mat))
    }
    adj_mkt_share_mat <- tmp_adj_mkt_share_mat
  }
  return(list(fleet_technology_market_share=adj_mkt_share_mat))
}
