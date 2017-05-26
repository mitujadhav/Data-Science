
a<-"https://cdn-lb.vungle.com/zen/Client_30s_SoulHunters_PT_160516_iOS-1280x720-Q2.mp4"
b<-"http://cdn-lb.vungle.com/zen/Summoner%20Fantasy%20cg%20trailer%2030s-1280x720-Q2.mp4"
url2 <- "http://cdn-lb.vungle.com/zen/Client_15s_ChaoNengJiChengZhe_160429_android_launch_ch2-1280x720-Q2.mp4"

download.file(a,"demo_video1.mp4",method = "libcurl")
download.file(b,"demo_video2.mp4",method = "curl")
download.file(url2,"demo_video.mp4",method = "curl")



urls<-read.csv("D:/Data Science Internal/Komal Vungle/myurl.csv")
url_list<-urls$data2.url

urls[urls=="NULL"] <- NA
urls[urls==""] <- NA

urls<-urls[complete.cases(urls),]
urls[which(urls$data2.url=="1280" | urls$data2.url=="720"),]

setwd("D:/Data Science Internal/Komal Vungle/Videos")

for(i in 501:600)
{
  videotitle <- paste0("demo_video",i,".mp4")
  download.file(as.character(urls$data2.url[i]),videotitle,method = "curl")
  i <- i+1
}

### If ffmpeg is present, we can load videos as well:
###
tennis <- load.video(system.file('extdata/tennis_sif.mpeg',package='imager'))

plot(tennis,frame=1)

tennis.g <- grayscale(tennis)
motion <- deriche(tennis.g,1,order=1,axis="z")^2 #Differentiate along z axis and square
combined <- list(motion/max(motion),tennis.g/max(tennis.g)) %>% imappend("x") #Paste the two videos together

#----------------------------------------------



file <- system.file('extdata/parrots.png',package='imager')
parrots <- load.image(file)
#The file format is defined by the extension. Here we save as JPEG
imager::save.image(parrots,"D:/parrots.jpeg")
#We call imager::save.image to avoid ambiguity, as base R already has a save.image function 


library(rvest)
#Run a search query (returning html content)
search <- read_html("https://www.google.com/search?site=&tbm=isch&q=parrot")

#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src") #Get urls of parrot pictures

#Load the first four, return as image list, display
map_il(urls[1:4],load.image) %>% plot

