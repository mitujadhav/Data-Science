############## Load  Data##################

Ch3Start <- Sys.time()
readPath <- rxGetOption("sampleDataDir")
infile <- file.path(readPath, "promoted_content1.txt")
hyphensTxt <- RxTextData(infile, delimiter="\t")
promoted_content <- rxImport(hyphensTxt)

colnames(promoted_content)[1]<-"ad_id"
colnames(promoted_content)[2]<-"document_id"
colnames(promoted_content)[3]<-"campaign_id"
colnames(promoted_content)[4]<-"advertiser_id"
names(promoted_content)
------------------------------------------------------
Ch3Start <- Sys.time()
readPath <- rxGetOption("sampleDataDir")
infile <- file.path(readPath, "page_views.txt")
hyphensTxt <- RxTextData(infile, delimiter="\t")
page_views <- rxImport(hyphensTxt)
page_views

colnames(page_views)[1]<-"uuid_id"
colnames(page_views)[2]<-"document_id"
colnames(page_views)[3]<-"timestamp_id"
colnames(page_views)[4]<-"platform_id"
colnames(page_views)[5]<-"geo_location"
colnames(page_views)[6]<-"traffic_source"
-------------------------------------------------------
Ch3Start <- Sys.time()
readPath <- rxGetOption("sampleDataDir")
infile <- file.path(readPath, "clicks_train.txt")
hyphensTxt <- RxTextData(infile, delimiter="\t")
clicks_train <- rxImport(hyphensTxt)

colnames(clicks_train)[1]<-"display_id"
colnames(clicks_train)[2]<-"ad_id"
colnames(clicks_train)[3]<-"clicked"
names(clicks_train)
-------------------------------------------------------
Ch3Start <- Sys.time()
readPath <- rxGetOption("sampleDataDir")
infile <- file.path(readPath, "events.txt")
hyphensTxt <- RxTextData(infile, delimiter="\t")
events<- rxImport(hyphensTxt)

colnames(events)[1]<-"display_id"
colnames(events)[2]<-"uuid_id"
colnames(events)[3]<-"document_id"
colnames(events)[4]<-"timestamp_id"
colnames(events)[5]<-"platform"
colnames(events)[6]<-"geo_location"
#-------------------------------------------------------
 
dbinom(3,size=7,p=0.3)+dbinom(4,size=7,p=0.3)+dbinom(5,size=7,p=0.3)+dbinom(6,size=7,p=0.3)+dbinom(7,size=7,p=0.3)
dbinom(92,size=100,p=0.9) 
  
  
  
  
  
  
  
  
