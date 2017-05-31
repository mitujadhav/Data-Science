
merged_pc_clicks_events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_merged_pc_clicks_events.csv")

### Top 10 Advertisers having highest CTR ###
### Advertisers

advertisers<-unique(merged_pc_clicks_events$advertiser_id)
top_advertisers<- data.frame(advertiser_id=numeric(3356),CTR=numeric(3356))

for(i in seq(1:3356))
{
  ad<-merged_pc_clicks_events[which(merged_pc_clicks_events$advertiser_id==advertisers[i]),]
  CTR_ad<-dim(ad[which(ad$clicked==1),])[1]/dim(ad)[1]
  top_advertisers$advertiser_id[i]<-advertisers[i]
  top_advertisers$CTR[i]<-CTR_ad
}
top_advertisers[order(-top_advertisers$CTR),]
dim(top_advertisers[which(top_advertisers$CTR!=0),])

### Top 10 Campaigns having highest CTR ###
### Campaigns

campaigns<-unique(merged_pc_clicks_events$campaign_id)
top_campaigns<- data.frame(advertiser_id=numeric(20213),CTR=numeric(20213))

for(i in seq(1:20213))
{
  camp<-merged_pc_clicks_events[which(merged_pc_clicks_events$campaign_id==campaigns[i]),]
  CTR_camp<-dim(camp[which(camp$clicked==1),])[1]/dim(camp)[1]
  top_campaigns$campaign_id[i]<-campaigns[i]
  top_campaigns$CTR[i]<-CTR_camp
}
top_campaigns[order(-top_campaigns$CTR),]
dim(top_campaigns[which(top_campaigns$CTR!=0),])


### Top 10 Ads having highest CTR ###
### Ads

ads<-unique(merged_pc_clicks_events$ad_id)
top_ads <- data.frame(ad_id=numeric(80377),CTR=numeric(80377))

for(i in seq(1:80377))
{
  adver<-merged_pc_clicks_events[which(merged_pc_clicks_events$ad_id==ads[i]),]
  CTR_ad<-dim(adver[which(adver$clicked==1),])[1]/dim(adver)[1]
  top_ads$ad_id[i]<-ads[i]
  top_ads$CTR[i]<-as.numeric(CTR_ad)
}

top_ads[order(-top_ads$CTR),]
dim(top_ads[which(top_ads$CTR!=0),])

### Top Users whose CTR is highest ###
### Users

uuid<-unique(merged_pc_clicks_events$uuid)
top_users <- data.frame(user_id=numeric(685302),CTR=numeric(685302))

for(i in seq(1:685302))
{
  user<-merged_pc_clicks_events[which(merged_pc_clicks_events$uuid==uuid[i]),]
  CTR_user<-dim(user[which(user$clicked==1),])[1]/dim(user)[1]
  top_users$user_id[i]<-uuid[i]
  top_users$CTR[i]<-as.numeric(CTR_user)
}
top_users[order(-top_users$CTR),]
dim(top_users[which(top_users$CTR!=0),])

## Clustering ##

data1<-merged_pc_clicks_events[complete.cases(merged_pc_clicks_events),]

data1[data1=="NULL"] <- NA
data1[data1==""] <- NA
data2<-data1[,-c(2,4,9,13)]
data2<-data2[complete.cases(data2),]
z<-data2

data2<-data2[,-c(2,3,5)]
data2$ad_id<-as.factor(data2$ad_id)
data2$platform<-as.factor(data2$platform)
data2$clicked<-as.factor(data2$clicked)
data2$campaign_id<-as.factor(data2$campaign_id)
data2$advertiser_id<-as.factor(data2$advertiser_id)

write.csv(data2,file = "D:/outbrain.csv")

#install.packages("cba")
library(cba)

## Our Data ##

rm(data1)
rm(merged_pc_clicks_events)

## As the memory is not sufficient I have taken only one advertiser's data
#[sample(dim(x)[1],100),]

data3<-data2[which(data2$advertiser_id==2724),]
data3<-data3[,-5]

x <- as.dummy(data3[-3])
rc <- rockCluster(x, n=10, theta=0.8)
print(rc)
rp <- predict(rc, x)
table(data3$clicked, rp$cl)

clusters<-as.data.frame(rp$cl)
data4<-cbind(data3,clusters)

# OUTPUT CLUSTERS :  
#    1  2  3  4  5  6  7  8  9 10 13 14 15 17 18 19 20 21 22 23 24 25 26 31 <NA>
# 0  9  3  9 10  7 18 11 12  9  3  5  4  9  6  6  6  9 33 52 15  5 18 11  3    8
# 1  2  0  1  2  1  3  1  1  1  1  1  0  5  2  2  6  2 15 26  2  2  1  5  1    0




