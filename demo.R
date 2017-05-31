documents_category<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_categories.csv")
doc_meta<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_meta.csv")
doc_topics<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_topics.csv")
doc_entities<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_entities.csv")

install.packages("caret")
library(caret)

promoted_content<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/promoted_content.csv")
intrain<-createDataPartition(promoted_content$ad_id,p = 0.1,list = FALSE)
sample_pc<-promoted_content[intrain,]
rm(promoted_content)
rm(intrain)

click_train<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/clicks_train.csv")
intrain<-createDataPartition(click_train$ad_id,p = 0.1,list = FALSE)
sample_click_train<-click_train[intrain,]
rm(click_train)
rm(intrain)

merged<-merge(sample_click_train,sample_pc,by="ad_id")

events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/events.csv")
intrain<-createDataPartition(events$display_id,p = 0.1,list = FALSE)
sample_event<-events[intrain,]
rm(events)
rm(intrain)

merged2<-merge(merged,events,by=c("display_id","document_id"))

install.packages("dplyr")
install.packages("ggplot2")
install.packages("shiny")
install_github("StatsWithR/statsr")
install.packages("devtools")

library(devtools)
library(dplyr)
library(ggplot2)
library(shiny)

merge3<-merge(merged2,page_views,by="uuid")
intrain<-createDataPartition(page_views$traffic_source,p = 0.1,list = FALSE)
pw<-page_views[intrain,]

merge3<-merge(merged2,page_views,by="uuid")
length(unique(merge3$uuid))

pagev_pc<-merge(page_views,promoted_content,by="document_id")
click_pagev_pc<-merge(pagev_pc,click_train,by="ad_id")

rxOptions(xdfCompressionLevel = 3)
inFile <- file.path(rxGetOption("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/"), "AirlineDemoSmall.csv")
airData <- rxImport(inData = inFile, outFile="airExample.xdf", 
                    stringsAsFactors = TRUE, missingValueString = "M", 
                    rowsPerRead = 200000, overwrite = TRUE)

#-------------------------------

page_views<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/page_views.csv")

tail(sort(table(page_views$document_id)),100)
length(unique(page_views$document_id))

tail(sort(table(page_views$uuid)),100)
length(unique(page_views$uuid))

tail(sort(table(page_views$platform)),100)
length(unique(page_views$platform))

tail(sort(table(page_views$traffic_source)),100)
length(unique(page_views$traffic_source))

tail(sort(table(page_views$timestamp)),100)
length(unique(page_views$timestamp))

tail(sort(table(page_views$geo_location)),100)
length(unique(page_views$geo_location))

page_views$Country<- substr(page_views$geo_location, 1, 2)
(as.data.frame(sort(table(page_views$Country))))
barplot(tail(sort(table(page_views$Country)),10))


page_views[which(page_views$Country=="US"),]

page_views$state<- substr(page_views$geo_location, 4, 5)
page_views[which(page_views$state=="" & page_views$Country=="US" ),10]<-"Not Available"

page_views[which(page_views$Country=="US"),]

#------------------------------------------------------

events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/events.csv")

tail(sort(table(events$document_id)),100)
length(unique(events$document_id))

tail(sort(table(events$uuid)),100)
length(unique(events$uuid))

tail(sort(table(events$platform)),100)
length(unique(events$platform))

tail(sort(table(events$display_id)),100)
length(unique(events$display_id))

tail(sort(table(events$timestamp)),100)
length(unique(events$timestamp))

tail(sort(table(events$geo_location)),100)
length(unique(events$geo_location))

events$Country<- substr(events$geo_location, 1, 2)
(as.data.frame(sort(table(events$Country))))
barplot(tail(sort(table(events$Country)),10))

events$state<- substr(events$geo_location, 4, 5)
events[which(events$Country=="US"),]


#-----------------------

d_categories<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_categories.csv")
length(unique(d_categories$category_id))
table(d_categories$category_id)

d_topics<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_topics.csv")
tail(sort(table(a$document_id)))

#---------------------------------

pc_d_meta_cat_topic<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/pc_d_meta_cat_topic.csv")
events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/events.csv")

doc_id<-unique(pc_d_meta_cat_topic$document_id)
doc_id
events1<-events[events$document_id %in% doc_id,]

events_pc_d_meta_cat_topic<-merge(events1,pc_d_meta_cat_topic)
merge(document_topics,merge(document_categories,(merge(document_meta.csv , document_entites ,by=document_id)),by=document_id),by=document_id)

## want to mearge  

d_meta_entity<-merge(document_meta,document_entities,by="documet_id")
d_cat_meta_entity<-merge(d_meta_entites,document_categories,by="documet_id")
d_topic_cat_meta_entity<-merge(d_cat_meta_entity,document_topics,by="documet_id")

length(unique(click_train$display_id))
length(unique(click_train$ad_id))
length(unique(click_train$clicked))


##---------------------------------------------------

sample_clicks<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_clicks.csv")
sample_events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_events.csv")

merged_clicks_events<-merge(sample_events,sample_clicks,by="display_id")
merged_clicks_events<-merged_clicks_events[,-c(2,8)]


write.csv(merged_clicks_events,file="D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_merged_clicks_events.csv")

merged_pc_clicks_events<-merge(merged_clicks_events,promoted_content,by="ad_id")
head(merged_pc_clicks_events)


merged_pc_clicks_events<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_merged_pc_clicks_events.csv")
merged_pc_clicks_events$Country<- substr(merged_pc_clicks_events$geo_location, 1, 2)
barplot(tail(sort(table(merged_pc_clicks_events$Country)),10))

merged_pc_clicks_events$state<- substr(merged_pc_clicks_events$geo_location, 4, 5)
barplot(tail(sort(table(merged_pc_clicks_events$state)),10))

write.csv(merged_pc_clicks_events,file="D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_merged_pc_clicks_events.csv")


US_users<-merged_pc_clicks_events[which(merged_pc_clicks_events$Country=="US"),]
NonUS_users<-merged_pc_clicks_events[which(merged_pc_clicks_events$Country!="US"),]



d_categories<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_categories.csv")
d_topics<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_topics.csv")
d_meta<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_meta.csv")
d_entities<-read.csv("D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/documents_entities.csv")

d_meta_category<-merge(d_meta,d_categories,by="document_id")
d_entity_meta_category<-merge(d_meta_category,d_entities,by="document_id")
d_topics_entity_meta_category<-merge(d_entity_meta_category,d_topics,by="document_id")
                                     

colnames(merged_pc_clicks_events)[4]<-"document_id"
merged_doc_pc_clicks_events<-merge(merged_pc_clicks_events,d_entity_meta_category,by="document_id")

write.csv(merged_doc_pc_clicks_events,file="D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/merged_doc_pc_clicks_events.csv")

sample_merged_doc_entity_pc_clicks_events<-merge(merged_doc_pc_clicks_events,d_topics,by="document_id")
write.csv(sample_merged_doc_entity_pc_clicks_events,"D:/Data Science Internal/Look Alike Modeling for Audience Targeting/outbrain/sample_merged_doc_entity_pc_clicks_events.csv")

main_data<-sample_merged_doc_entity_pc_clicks_events
rm(sample_merged_doc_entity_pc_clicks_events)
colnames(main_data)[22]<-"topic_confidence"

head(main_data)

aggregate(merged_pc_clicks_events(count = merged_pc_clicks_events$advertiser_id), list(value = merged_pc_clicks_events), length)
data1<-merged_pc_clicks_events
rm(merged_pc_clicks_events)

























