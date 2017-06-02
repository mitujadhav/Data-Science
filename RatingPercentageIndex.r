df<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonDetailedResults.csv")
table(df$Season)
names(df)
df<-df[,c(1:8)]

a<-as.data.frame(unique(df$Wteam))
b<-as.data.frame(unique(df$Lteam))
colnames(a)[1]<-"team"
colnames(b)[1]<-"team"
c<-rbind(a,b)
d<-unique(c)

df<-df[which(df$Wteam=="1105" | df$Lteam=="1105" & df$Season=="2003"),]
(0.6*(dim(df[which(df$Wteam=="1105" & df$Wloc=="H"),])[1]+dim(df[which(df$Lteam=="1105" & df$Wloc=="A"),])[1]))+(1.4*(dim(df[which(df$Wteam=="1105" & df$Wloc=="A"),])[1]+dim(df[which(df$Lteam=="1105" & df$Wloc=="H"),])[1]))















