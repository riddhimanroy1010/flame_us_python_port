library(readxl)
#Convert vision data
#Inputs
vis_dts <- read.csv("inputs/data/fleet_st&sl_VISION_70-100.csv",header = FALSE,stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
#List of technologies to consider
technology_list <- unique(as.character(vis_dts[3,vis_dts[1,]=="Technology Market Shares"]))
yr_list <- 1970:2006
#Output files
dt_col<-c("Data_type","Year","Size","Technology","Value")
vision_hist_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Get stock and sales data
for (yr in yr_list){
  #Obtain total sales
  total_sales <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Vehicle Sales"&vis_dts[2,]=="LDV"])
  for (sz in c("Car","Light truck")){
    size_market_share <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Vehicle Sales"&vis_dts[2,]==sz])
    for (techno in technology_list){
      #veh_techno is equivalent vehicle technology
      veh_techno<-vh_techno$Own[sapply(1:nrow(vh_techno),function(x)techno %in% unlist(strsplit(vh_techno$Vision[x],";")))]
      techno_market_share <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Technology Market Shares"&vis_dts[2,]==sz&vis_dts[3,]==techno])
      #Update data output with sales
      vision_hist_dt[nrow(vision_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,sz,veh_techno,total_sales*size_market_share*techno_market_share)
      #Update data ouput with stock
      techo_stock<-as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Duty Vehicle Stock, million"&vis_dts[2,]==sz&vis_dts[3,]==techno])*10^6
      vision_hist_dt[nrow(vision_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,sz,veh_techno,techo_stock)
    }
  }
}
#Format and aggregate multiple technologies
vision_hist_dt$Year<-as.numeric(vision_hist_dt$Year)
vision_hist_dt$Value<-as.numeric(vision_hist_dt$Value)
agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type"),response = "Value")
vision_hist_dt<-aggregate(data = vision_hist_dt,agg.formula,FUN=sum)
vision_hist_dt[,"Unit"]<-"vehicle"

vision_sales_dt <- subset(vision_hist_dt,Data_type=="sales")
vision_stock_dt <- subset(vision_hist_dt,Data_type=="stock")
#Read AEO data
aeo_sales_dt1 <- read.csv("inputs/model/fleet_sales_hist_aeo_10-13.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
aeo_sales_dt2 <- read.csv("inputs/model/fleet_sales_hist_aeo_14-21.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
aeo_stock_dt1 <- read.csv("inputs/model/fleet_stock_hist_aeo_10-13.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
aeo_stock_dt2 <- read.csv("inputs/model/fleet_stock_hist_aeo_14-21.csv",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)

#Combine historical data
sales_dt <- rbind(vision_sales_dt,subset(aeo_sales_dt1,select=-Aeo_year),subset(aeo_sales_dt2,select=-Aeo_year))
stock_dt <- rbind(vision_stock_dt,subset(aeo_stock_dt1,select=-Aeo_year),subset(aeo_stock_dt2,select=-Aeo_year))


#ISSUE: Important gap in total car and light truck stockes between 2013 and 2014 in data
#Assumption: Discrepancies are due to changes in size standards. Some cars became LT in 2014.
#We adjust car and light trucks stock in 2013 and previous data to limit impact of discrepancies on vintage stock
adj_vector <- data.frame(Year=2013:1970)
for (sz in c("Car","Light truck")){
  #diff is total stock different from 2013 to 2014
  diff<-sum(subset(stock_dt,Size==sz&Year==2014&Data_type=="stock",select=Value))-sum(subset(stock_dt,Size==sz&Year==2013&Data_type=="stock",select=Value))
  #Fill adj_vector with initial adjustement (initial difference)
  adj_vector[adj_vector$Year==2013,sz]<-diff
  #Assumption: The stock adjustement follows the same distribution than the light truck vehicle development from initial diff data.
  adj_vector[which(adj_vector$Year%in%(2012:1970)),sz]<-diff*
    cumprod(sapply(2012:1970, function(x)sum(subset(stock_dt,Size=="Light truck"&Year==x&Data_type=="stock",select=Value)))/
              sapply(2013:1971, function(x)sum(subset(stock_dt,Size=="Light truck"&Year==x&Data_type=="stock",select=Value))))
}
#We adjust stock data with previous adjustement factors
#Assumption: We do not change stock share. We assume 2014 and 2013 total stock to be the same.
adj_stock_dt <- stock_dt
for (sz in c("Car","Light truck")){
  for (y in 2013:1970){
    #i_stock is the initial stock
    i_stock<-sum(subset(stock_dt,Size==sz&Year==y&Data_type=="stock",select=Value))
    #f_stock is the final stock
    f_stock<-i_stock+adj_vector[adj_vector$Year==y,sz]
    #Adjust data
    adj_stock_dt[adj_stock_dt$Size==sz&adj_stock_dt$Year==y&adj_stock_dt$Data_type=="stock","Value"] <- (subset(stock_dt,Size==sz&Year==y&Data_type=="stock")[,"Value"]/i_stock)*f_stock
  }
}
#Format
adj_stock_dt$Value<-trunc(adj_stock_dt$Value)

#Check data consistencies
agg.formula<-reformulate(termlabels = setdiff(colnames(adj_stock_dt),c("Technology","Unit","Value")),response = "Value")
check_dt<-aggregate(data = adj_stock_dt,agg.formula,FUN=sum)
#Output

write.csv(adj_stock_dt,"inputs/model/fleet_stock_hist.csv", row.names = FALSE)
write.csv(sales_dt,"inputs/model/fleet_sales_hist.csv", row.names = FALSE)

