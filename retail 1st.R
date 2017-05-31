library(RMySQL)
con<-dbConnect(MySQL(),user="root",db="retail2",password="root")

#Getting Data from MYSQL:
demand_std_dev_data_weekly.csv<-read.csv("C:/Users/madhumitaj/Desktop/Retail Tables/Retail/demand_std_dev_data_weekly/demand_std_dev_data_weekly.csv")
res <- dbSendQuery(con, "SELECT * from markdown_puma_products_forecasted_demand;")
markdown_puma_products_forecasted_demand <- fetch(res, n = -1)
markdown_puma_products_forecasted_demand

write.csv(markdown_puma_products_forecasted_demand,file="C:/Users/madhumitaj/Desktop/markdown_puma_products_forecasted_demand.csv")

res <- dbSendQuery(con, "SELECT * from markdown_products_demand;")
markdown_products_demand <- fetch(res, n = -1)
markdown_products_demand
write.csv(markdown_products_demand,file="C:/Users/madhumitaj/Desktop/markdown_products_demand.csv")

demand_Forecased<-merge(markdown_products_demand,markdown_puma_products_forecasted_demand,by=c("product_id","brand_name","sku","week","brand_id"))

---------------------------------------------------------------------------------------------------------------------
  
res <- dbSendQuery(con, "SELECT * from markdown_b2b_price_demand;")
markdown_b2b_price_demand <- fetch(res, n = -1)
markdown_b2b_price_demand

res <- dbSendQuery(con, "SELECT * from markdown_cnf_data;")
markdown_cnf_data <- fetch(res, n = -1)
markdown_cnf_data

res <- dbSendQuery(con, "SELECT * from markdown_demand_stablity;")
markdown_demand_stablity<- fetch(res, n = -1)
markdown_demand_stablity

res <- dbSendQuery(con, "SELECT * from markdown_eligible_reasons;")
markdown_eligible_reasons<- fetch(res, n = -1)
markdown_eligible_reasons

res <- dbSendQuery(con, "SELECT * from markdown_exclusion_list;")
markdown_exclusion_list<- fetch(res, n = -1)
markdown_exclusion_list

res <- dbSendQuery(con, "SELECT * from markdown_exclusion_params;")
markdown_exclusion_params<- fetch(res, n = -1)
markdown_exclusion_params

res <- dbSendQuery(con, "SELECT * from markdown_exclusion_params_details;")
markdown_exclusion_params_details<-fetch(res, n = -1)
markdown_exclusion_params_details


res <- dbSendQuery(con, "SELECT * from markdown_exclusion_reasons;")
markdown_exclusion_reasons<-fetch(res, n = -1)
markdown_exclusion_reasons

res <- dbSendQuery(con, "SELECT * from markdown_exclusion_reports;")
markdown_exclusion_reports<-fetch(res, n = -1)
markdown_exclusion_reports

res <- dbSendQuery(con, "SELECT * from markdown_fact_sales_weekly_sales;")
markdown_fact_sales_weekly_sales<-fetch(res, n = -1)
markdown_fact_sales_weekly_sales

res <- dbSendQuery(con, "SELECT * from markdown_products;")
markdown_products<-fetch(res, n = -1)
markdown_products


res <- dbSendQuery(con, "SELECT * from markdown_products_eligible;")
markdown_products_eligible<-fetch(res, n = -1)
markdown_products_eligible

res <- dbSendQuery(con, "SELECT * from markdown_products_gmroi_log;")
markdown_products_gmroi_log<-fetch(res, n = -1)
markdown_products_gmroi_log

res <- dbSendQuery(con, "SELECT * from markdown_products_gmroi_results;")
markdown_products_gmroi_results<-fetch(res, n = -1)
markdown_products_gmroi_results

res <- dbSendQuery(con, "SELECT * from markdown_products_list_uat;")
markdown_products_list_uat<-fetch(res, n = -1)
markdown_products_list_uat

res <- dbSendQuery(con, "SELECT * from markdown_puma_products_current_inventory;")
markdown_puma_products_current_inventory<-fetch(res, n = -1)
markdown_puma_products_current_inventory

res <- dbSendQuery(con, "SELECT * from markdown_reports;")
markdown_reports<-fetch(res, n = -1)
markdown_reports

res <- dbSendQuery(con, "SELECT * from markdown_sku_log;")
markdown_sku_log<-fetch(res, n = -1)
markdown_sku_log

res <- dbSendQuery(con, "SELECT * from markdown_sku_max_week ;")
markdown_sku_max_week <-fetch(res, n = -1)
markdown_sku_max_week 

res <- dbSendQuery(con, "SELECT * from markdown_target_margin;")
markdown_target_margin<-fetch(res, n = -1)
markdown_target_margin


res <- dbSendQuery(con, "SELECT * from markdown_process_reports;")
markdown_process_reports<-fetch(res, n = -1)
markdown_process_reports

res <- dbSendQuery(con, "SELECT * from markdown_sku_status;")
markdown_sku_status<-fetch(res, n = -1)
markdown_sku_status

res <- dbSendQuery(con, "SELECT * from markdown_markup_calculations;")
markdown_markup_calculations<-fetch(res, n = -1)
markdown_markup_calculations

# res <- dbSendQuery(con, "SELECT * from markdown_markup_calculations;")
# markdown_markup_calculations<-fetch(res, n = -1)
# markdown_markup_calculations

head(markdown_b2b_price_demand)
head(markdown_reports)


res<-dbSendQuery(con,"SELECT DISTINCT product_id,sku,brand_name FROM retail2.markdown_products_demand order by brand_name;")
unique_data<-fetch(res,n=-1)
dim(unique_data)

-------------------------------------------------------------------------------
# Merging
 
a<-merge(markdown_products,markdown_products_gmroi_log,by="product_id") 
b<-merge(a,markdown_products_demand,by=c("product_id","sku","brand_name"))
c<-merge(markdown_products,markdown_sku_log,by=c("product_id"))
d<-merge(markdown_products,markdown_markup_calculations,by=c("product_id"))
e<-merge(markdown_products,markdown_demand_stablity,by=c("product_id"))
#merge(markdown_products,markdown_reports_sales,by=c("product_id"))

sku<-merge(markdown_sku_log,markdown_sku_status,by=("markdown_status_id"))
sku<-sku[,-3]
colnames(sku)[13] <- "description"
colnames(sku)[14] <- "created_datetime_sku"
colnames(sku)[6] <- "created_datetime_sku_log"

f<-merge(sku,markdown_products,by="product_id")
g<-merge(markdown_b2b_price_demand,markdown_products,by="product_id")
h<-merge(markdown_sku_max_week,sku,by="product_id")

head(markdown_products)

product_demand<-merge(markdown_products_demand,markdown_products,by=c("product_id","sku","brand_name"))
forecast<-merge(markdown_puma_products_current_inventory,markdown_puma_products_forecasted_demand,by=c("product_id","sku","brand_name","brand_id"))
Ecomm_demand<-product_demand[,c(1:8,11,12,15,16,19,20,23,24,25,28,31,32,35,36,39,39,40,41,42,43,46,47,50,51,54,55,58,59,62,63,66,67,70:72)]
Market_demands<-product_demand[,c(1:7,9,11,13,15,17,19,23,24,26,29,31,33,35,37,39,40,41,42,44,46,48,50,52,54,56,58,60,64,66,68,70:72)]
Whsales_demands<-product_demand[,c(1:7,10,11,14,15,18,19,23,24,27,30,31,34,35,38,39,40,41,42,45,46,49,50,53,54,57,58,61,65,66,69:72)]








# 
# for(j in seq(NROW(unique(Ecomm_demand$brand_name))))
# {
#   prod_name<-unique(Ecomm_demand$brand_name)[j]
#   data1<-Ecomm_demand[which(Ecomm_demand$brand_name==as.character(prod_name)),]
#   print("--------------------")
#   print(data1)
#   for(i in seq(1:NROW(unique(data1$sku))))
#   {
#     sku1<-unique(data1$sku)[i]
#     data2<-data1[which(data1$sku==sku1),]
#     print("###############")
#     print(data2)
#   }
# }
#   
# 
# l=1
# k=NULL
# k<-as.data.frame(k)
# 
# for(i in seq(1:NROW(unique_data)))
# {
#   a<-Ecomm_demand[which(Ecomm_demand$brand_name==unique_data$brand_name[i] & Ecomm_demand$product_id==unique_data$product_id[i] & Ecomm_demand$sku==unique_data$sku[i]),]
#   for(j in seq(nrow(a)))
#   {
#     k[l,]<-a[j,]
#     l=l+1
#   }
# }
dbWriteTable(con, value = product_demand, name = "MyTable", append = TRUE )
dbWriteTable(con, value = forecast, name = "puma_forecast", append = TRUE )
dbWriteTable(con, value = demand_Forecased, name = "demandForecasted", append = TRUE )
--------------------------------------------Corelation------------------------
  
  
  Correlation_Plot<-function(Ecomm_demand1)
  {
    library(corrplot)
    M <- cor(Ecomm_demand1)
    corrplot(cor(Ecomm_demand1), method="number",order="hclust", addrect=5)
  }


Ecomm_demand2<-Ecomm_demand1[,-c(3,4,5,7,9,16,21,24,26,28,32)]
corrplot(cor(Ecomm_demand2), method="number",order="hclust", addrect=5)


---------------------------------------------------------------------------
  
  Ecomm_demand


