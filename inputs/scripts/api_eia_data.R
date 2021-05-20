###>Creates functions to obtain AEO data from EIA API.

library(XML)
library(reshape2)
library(tidyr)

# Fuel prices -------------------------------------------------------------

getAEOFuelPrices <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  #Output
  dt_col <- c("tmp","Year","Value","Unit")
  aeo_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Names
  aeo_cat_name <- "Energy Prices"
  aeo_data_name <- "Energy Prices by Sector and Source (2018 dollars per million Btu, unless otherwise noted)"
  
  gen_cat_list = getCatEIA(key=key)
  
  aeo_cat = as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list = getCatEIA(key=key,cat=aeo_cat)
  
  aeo_yr_cat = as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list = getCatEIA(key=key,cat=aeo_yr_cat)
  
  aeo_case_id = as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list = getCatEIA(key=key,cat=aeo_case_id)
  
  aeo_cat_id = as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name==aeo_cat_name]))
  aeo_cat_list = getCatEIA(key=key,cat=aeo_cat_id)
  aeo_cat_series_row <- grep(aeo_data_name,as.character(aeo_cat_list$Sub_Categories$name),fixed = TRUE,invert=FALSE)
  #Loop over regions
  for (j in aeo_cat_series_row){
    aeo_data_type_id = as.numeric(as.character(aeo_cat_list$Sub_Categories[j,"category_id"]))
    aeo_data_series = getCatEIA(key=key,cat=aeo_data_type_id)
    #Consider series with "Transportation"
    aeo_series_row <- grep("Energy Prices : Transportation :",as.character(aeo_data_series$Series_IDs$name),fixed = TRUE,invert=FALSE)
    for (i in aeo_series_row){
      serie_name <- as.character(aeo_data_series$Series_IDs[i,"name"])
      serie_id <- as.character(aeo_data_series$Series_IDs[i,"series_id"])
      unit <- as.character(aeo_data_series$Series_IDs[i,"units"])
      tmp_dt <- getEIA(ID=serie_id,key=key)
      tmp_dt[,"tmp"] <- serie_name
      tmp_dt[,"Unit"] <- unit
      #Combine
      aeo_dt<-rbind(aeo_dt,tmp_dt)
    }
  }
  #Format table
  format_aeo_dt <- subset(cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("Data_type","Sector","tmp_name"))),select=-tmp)
  format_aeo_dt <- subset(cbind(format_aeo_dt,colsplit(format_aeo_dt$tmp_name,", ",names=c("Fuel","Region","Aeo_case","Aeo_year"))),select=-tmp_name)
  return(format_aeo_dt)
}

# Vehicle fuel consumption ---------------------------------------------------------


getAEOVehicleFC <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  
  gen_cat_list=getCatEIA(key=key)
  aeo_cat=as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list=getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat=as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list=getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id=as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list=getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id=as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Transportation Sector"]))
  aeo_trans_list=getCatEIA(key=key,cat=aeo_trans_id)
  aeo_data_type_id=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[aeo_trans_list$Sub_Categories$name=="New Light-Duty Vehicle Fuel Economy (miles per gallon)"]))
  aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
  #Keep all rowas
  aeo_series_row <- 1:length(aeo_data_series$Series_IDs$name)
  #Output
  dt_col<-c("tmp","Year","Value")
  aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (i in aeo_series_row){
    serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
    serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
    tmp_dt<-getEIA(ID=serie_id,key=key)
    tmp_dt[,"tmp"]<-serie_name
    #Combine
    aeo_dt<-rbind(aeo_dt,tmp_dt)
  }
  #Format table
  aeo_dt <- cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("Data","Technology","Size")))
  aeo_dt[aeo_dt$Size=="",c("Size","Technology")] <- colsplit(subset(aeo_dt,Size=="")$Technology,": ",names=c("Technology","Size"))[,c("Size","Technology")]
  aeo_dt[,"Class"]<-colsplit(aeo_dt$Size,", ",names=c("Size","Aeo_case","Aeo_year"))$Size
  aeo_dt[,"Size"] <- NULL
  aeo_dt[,"tmp"] <- NULL
  #Enter unit
  aeo_dt[,"Unit"] <- "MPG"
  aeo_dt[,"Aeo_year"] <- aeo_year
  aeo_dt[,"Aeo_case"] <- aeo_case
  return(aeo_dt)
}

# Electricity generation --------------------------------------------------


getAEOElecGen <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  gen_cat_list = getCatEIA(key=key)
  aeo_cat = as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list = getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat = as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list = getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id = as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list = getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id = as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Electric Power Sector"]))
  aeo_trans_list = getCatEIA(key=key,cat=aeo_trans_id)
  if (any(grepl("Electricity Generation by Electricity Market Module Region and Source",aeo_trans_list$Sub_Categories$name,ignore.case = TRUE))){
    aeo_data_type_id = as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[grep("Electricity Generation by Electricity Market Module Region and Source",aeo_trans_list$Sub_Categories$name,ignore.case = TRUE)]))
    aeo_data_series = getCatEIA(key=key,cat=aeo_data_type_id)$Series_IDs
    #Output
    aeo_dt <- NULL
    for (i in 1:nrow(aeo_data_series)){
      tmp_dt <- getEIA(ID=aeo_data_series[i,"series_id"],key=key)
      tmp_dt[,"Unit"] <- aeo_data_series[i,"units"]
      tmp_dt[,"tmp"] <- aeo_data_series[i,"name"]
      #Combine
      aeo_dt <- rbind(aeo_dt,tmp_dt)
    }
    #Format table
    new_aeo_dt <- subset(cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("td1","Region","Source"))),select=-c(tmp,td1))
    new_aeo_dt$Source <- colsplit(new_aeo_dt$Source,", ",names=c("Source","Aeo_case","Aeo_year"))$Source
    #Enter unit
    new_aeo_dt[,"Aeo_year"] <- aeo_year
    new_aeo_dt[,"Aeo_case"] <- aeo_case
    #
    if ("" %in% new_aeo_dt$Source){
      new_aeo_dt$Region[new_aeo_dt$Source==""] <- colsplit(new_aeo_dt$Region[new_aeo_dt$Source == ""],", ",names=c("Region","Aeo_case","Aeo_year"))$Region
      new_aeo_dt$Source[new_aeo_dt$Source==""] <- "Total"
    }
  } else if(any(grepl("Electric Power Projections by Electricity Market Module Region",aeo_trans_list$Sub_Categories$name,ignore.case = TRUE))){
    aeo_data_type_id <- as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[grep("Electric Power Projections by Electricity Market Module Region",aeo_trans_list$Sub_Categories$name,ignore.case = TRUE)]))
    aeo_dt <- NULL
    for (i in aeo_data_type_id){
      aeo_serie_list <- getCatEIA(key=key,cat=i)
      aeo_data_series <- aeo_serie_list$Series_IDs[grep("Electricity : Electric Power Sector : Generation",aeo_serie_list$Series_IDs$name,ignore.case = TRUE),]
      for (j in 1:nrow(aeo_data_series)){
        tmp_dt <- getEIA(ID=aeo_data_series[j,"series_id"],key=key)
        tmp_dt[,"Unit"] <- aeo_data_series[j,"units"]
        tmp_dt[,"tmp"] <- aeo_data_series[j,"name"]
        #Combine
        aeo_dt <- rbind(aeo_dt,tmp_dt)
      }
    }
    #Format table
    new_aeo_dt <- subset(cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("td1","td2","td3","new_tmp"))),select=-c(tmp,td1,td2,td3))
    new_aeo_dt <- subset(cbind(new_aeo_dt,colsplit(new_aeo_dt$new_tmp,", ",names=c("Source","Region","td1","td2"))),select=-c(new_tmp,td1,td2))
    #Enter unit
    new_aeo_dt[,"Aeo_year"] <- aeo_year
    new_aeo_dt[,"Aeo_case"] <- aeo_case
    
  }
  return(new_aeo_dt)
}

# Stock and sales ---------------------------------------------------------


getAEOTransportation <- function(aeo_year,aeo_case,aeo_data,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  
  aeo_data_name1 <- switch (aeo_data,
                           "sales"="Light-Duty Vehicle Sales by Technology Type",
                           "stock"="Light-Duty Vehicle Stock by Technology Type",
                           print("ERROR: aeo_data not recognized")
  )
  
  aeo_data_name2 <- switch (aeo_data,
                            "sales"="United States",
                            "stock"="",
                            print("ERROR: aeo_data not recognized")
  )
  
  aeo_data_unit <- switch (aeo_data,
                           "sales"=10^3,
                           "stock"=10^6,
                           print("ERROR: aeo_data not recognized")
  )
  
  gen_cat_list=getCatEIA(key=key)
  aeo_cat=as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list=getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat=as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[sapply(aeo_cat_list$Sub_Categories$name,function(x)grepl("Annual Energy Outlook",x) & grepl(aeo_year,x))]))
  aeo_yr_list=getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id=as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list=getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id=as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Transportation Sector"]))
  aeo_trans_list=getCatEIA(key=key,cat=aeo_trans_id)
  aeo_data_type_id=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[sapply(aeo_trans_list$Sub_Categories$name,function(x) grepl(aeo_data_name1,x) & grepl(aeo_data_name2,x))]))
  aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
  #Eliminate series with "percent" or "total"
  aeo_series_row1<-intersect(grep("Percent",as.character(aeo_data_series$Series_IDs$name),fixed = TRUE,invert=TRUE),
                             grep("Total",as.character(aeo_data_series$Series_IDs$name),invert=TRUE,fixed=TRUE))
  #Keep only series containing "car" and "light truck"
  aeo_series_row2<-union(grep("car",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE),
                         grep("light truck",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE))
  aeo_series_row<-intersect(aeo_series_row1,aeo_series_row2)
  #Output
  dt_col<-c("tmp","Year","Value")
  aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (i in aeo_series_row){
    serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
    serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
    tmp_dt<-getEIA(ID=serie_id,key=key)
    tmp_dt$Value <- tmp_dt$Value*aeo_data_unit
    tmp_dt[,"tmp"]<-serie_name
    #Combine
    aeo_dt<-rbind(aeo_dt,tmp_dt)
  }
  #Format table
  aeo_dt <- subset(cbind(aeo_dt,colsplit(aeo_dt$tmp,": ",names=c("Data_type","Size","Technology"))),select=-tmp)
  aeo_dt$Technology<-colsplit(aeo_dt$Technology,", ",names=c("Technology","Country","Aeo_case","Aeo_year"))$Technology
  aeo_dt$Size[grep("car",aeo_dt$Size,ignore.case = TRUE)]<-"Car"
  aeo_dt$Size[grep("light truck",aeo_dt$Size,ignore.case = TRUE)]<-"Light truck"
  #Enter unit
  aeo_dt[,"Unit"] <- "vehicle"
  aeo_dt[,"Aeo_year"] <- aeo_year
  aeo_dt[,"Aeo_case"] <- aeo_case
  aeo_dt[,"Data_type"] <- aeo_data
  return(aeo_dt)
}

getAEOMarketShare <- function(aeo_year,aeo_case,key){
  sales_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="sales",key=key)
  #Outputs
  dt_col<-c("Year","Size","Value")
  size_market_share_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  dt_col<-c("Year","Size","Technology","Value")
  Technology_market_share_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (yr in unique(sales_dt$Year)){
    #total_sales is total number of sales
    total_sales <- sum(subset(sales_dt,Year==yr,select=Value))
    for (sz in unique(sales_dt$Size)){
      #size_sales is total number of sales per size
      size_sales <- sum(subset(sales_dt,Year==yr&Size==sz,select=Value))
      #Update size_market_share
      size_market_share_dt[nrow(size_market_share_dt)+1,]<-c(yr,sz,size_sales/total_sales)
      for (techno in unique(subset(sales_dt,Year==yr&Size==sz)$Technology)){
        techno_sales <- sum(subset(sales_dt,Year==yr&Size==sz&Technology==techno,select=Value))
        Technology_market_share_dt[nrow(Technology_market_share_dt)+1,]<-c(yr,sz,techno,techno_sales/size_sales)
      }
    }
  }
  
}

getAEOTransportation_reg <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  aeo_data_name <- "Light-Duty Vehicle Sales by Technology Type"
  aeo_data_unit <- 10^3
  #Output
  out_aeo_dt <- NULL
  gen_cat_list = getCatEIA(key=key)
  aeo_cat = as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list = getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat = as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list = getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id = as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list = getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id = as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Transportation Sector"]))
  aeo_trans_list = getCatEIA(key=key,cat=aeo_trans_id)
  #Get list of data to download
  aeo_data_type_reg_id_list=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[grep(aeo_data_name,as.character(aeo_trans_list$Sub_Categories$name))]))
  for (aeo_data_type_id in aeo_data_type_reg_id_list){
    aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
    #Eliminate series with "percent" or "total"
    aeo_series_row1 <- intersect(grep("Percent",as.character(aeo_data_series$Series_IDs$name),fixed = TRUE,invert=TRUE),
                               grep("Total",as.character(aeo_data_series$Series_IDs$name),invert=TRUE,fixed=TRUE))
    #Keep only series containing "car" and "light truck"
    aeo_series_row2<-union(grep("car",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE),
                           grep("light truck",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE))
    aeo_series_row<-intersect(aeo_series_row1,aeo_series_row2)
    #Output
    dt_col <- c("tmp","Year","Value")
    aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
    for (i in aeo_series_row){
      serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
      serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
      tmp_dt<-getEIA(ID=serie_id,key=key)
      tmp_dt$Value <- tmp_dt$Value*aeo_data_unit
      tmp_dt[,"tmp"]<-serie_name
      #Combine
      aeo_dt<-rbind(aeo_dt,tmp_dt)
    }
    #Format table
    format_aeo_dt <- aeo_dt$tmp %>%
      colsplit(" : ",names=c("Data_type","Size","tmp")) %>%
      cbind(subset(aeo_dt,select=-tmp))
    format_aeo_dt <- format_aeo_dt$tmp %>%
      colsplit(", ",names=c("Technology","Region","Aeo_case","Aeo_year")) %>%
      cbind(subset(format_aeo_dt,select=-tmp))
    format_aeo_dt$Size[grep("car",format_aeo_dt$Size,ignore.case = TRUE)]<-"Car"
    format_aeo_dt$Size[grep("light truck",format_aeo_dt$Size,ignore.case = TRUE)]<-"Light truck"
    #Enter unit
    format_aeo_dt[,"Unit"] <- "vehicle"
    #Combine with output dt
    out_aeo_dt <- rbind(out_aeo_dt,format_aeo_dt)
  }
  return(out_aeo_dt)
}

# Tools for all -----------------------------------------------------------


getEIA <- function(ID, key){
  
  ID <- unlist(strsplit(ID, ";"))
  key <- unlist(strsplit(key, ";"))
  
  url <- paste("http://api.eia.gov/series?series_id=", ID, "&api_key=", key, "&out=xml", sep="" )
  
  doc <- xmlParse(file=url, isURL=TRUE)
  
  df <- data.frame(
    Year = sapply(doc["//data/row/date"], XML::xmlValue),
    Value = sapply(doc["//data/row/value"], XML::xmlValue)
  )
  
  ### Sort from oldest to newest
  df <- df[ with(df, order(Year)), ]
  ### Convert Factors in numerics
  df$Year <- as.numeric(as.character(df$Year))
  df$Value <- as.numeric(as.character(df$Value))
  
  return(df)
}

getCatEIA <- function(cat=999999999, key){
  
  key <- unlist(strsplit(key, ";"))
  
  ifelse(cat==999999999,
         url <- paste("http://api.eia.gov/category?api_key=", key, "&out=xml", sep="" ),
         
         url <- paste("http://api.eia.gov/category?api_key=", key, 
                      "&category_id=", cat, "&out=xml", sep="" )
  )
  for( i in 1:3 ) {
    doc <- tryCatch(readLines(url, warn = FALSE), error = function(w) FALSE)
    if (class(doc) != "logical"){
      doc <- xmlParse(doc)
      break
    }
    else
      if(i == 3)
        stop(paste0("Attempted to retrieve data for category #", cat, 
                    " and failed ", i, " times. \n This is likely due to a communication error ", 
                    "with the EIA website."))
  }
  
  Parent_Category <- tryCatch(xmlToDataFrame(
    nodes = XML::getNodeSet(doc, "//category/parent_category_id")), 
    warning=function(w) FALSE, error=function(w) FALSE)
  
  Sub_Categories <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "//childcategories/row"))
  
  Series_IDs <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "///childseries/row"))
  
  Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
  names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
  
  return(Categories)
}
