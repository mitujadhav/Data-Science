grp_chat<-read.csv("D:/My Stuff/whatsapp chat/grp chat.csv")
barplot(table(grp_chat$Date))
barplot(table(grp_chat$HH))
barplot(tail(sort(table(grp_chat$Mobile.No))))

msg<-grp_chat$Message
writeLines(as.character(msg))
getTransformations()

table(tail(sort(grp_chat$Mobile.No)))

sorted_chat<-sort(table(grp_chat$Mobile.No))
sorted_chat<-as.data.frame(sorted_chat)
top_members<-sorted_chat[which(sorted_chat$Freq>1),]

a<-as.character(top_members$Var1)
a<-gsub("[?/+/(/)]","",a)
a<-gsub("[[:blank:]]", "", a)


for(i in seq(1:length(top_members$Var1)))
{
  file1<-as.character(a[i])
  file1<-paste0("D:/My Stuff/whatsapp chat/",a[i],".txt")
  fileConn<-file(file1)
  chat<-grp_chat[which(grp_chat$Mobile.No==top_members$Var1[i]),7]
  writeLines(as.character(chat),fileConn)
  close(fileConn)
}

 docs <- Corpus(DirSource("D:/My Stuff/whatsapp chat/Itm Admits only/msg"))
 writeLines(as.character(docs[[1]]))
 getTransformations()
 
 toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
 docs <- tm_map(docs, toSpace, "-")
 docs <- tm_map(docs, toSpace, ":")
 docs <- tm_map(docs, removePunctuation)
 docs <- tm_map(docs, toSpace, "'")
 docs <- tm_map(docs, toSpace, "'")
 docs <- tm_map(docs, toSpace, " - ")
 docs <- tm_map(docs,content_transformer(tolower))
 
 docs <- tm_map(docs, removeNumbers)
 docs <- tm_map(docs, removeWords, stopwords("english"))
 
 docs <- tm_map(docs, stripWhitespace)
 
 writeLines(as.character(docs[[1]]))
 library(SnowballC)
 docs <- tm_map(docs,stemDocument)
 writeLines(as.character(docs[[2]]))
 docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
 docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
 docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
 docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
 docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")
 docs[[2]]
 writeLines(as.character(docs[[2]]))
 
 dtm <- DocumentTermMatrix(docs)
 #inspect(dtm[1:2,170:175])
 freq <- colSums(as.matrix(dtm))
 length(freq)
 ord <- order(freq,decreasing=TRUE)
 freq[head(ord)]
 freq[tail(ord)]
 
 
 dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
                                              bounds = list(global = c(3,27))))
 freqr <- colSums(as.matrix(dtmr))
 #length should be total number of terms
 length(freqr)
 ordr <- order(freqr,decreasing=TRUE)
 #inspect most frequently occurring terms
 freqr[head(ordr)]
 findFreqTerms(dtmr,lowfreq=80)
 findFreqTerms(dtmr,lowfreq=80)
 findAssocs(dtmr,"love",0.6)
 findAssocs(dtmr,"yeah",0.6)
 
 
 
 wf=data.frame(term=names(freqr),occurrences=freqr)
 library(ggplot2)
 p <- ggplot(subset(wf, freqr>100), aes(term, occurrences))
 p <- p + geom_bar(stat="identity")
 p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
 p
 
 ####WORDCLOUD#####
 
 library(wordcloud)
 set.seed(1)
 wordcloud(names(freqr),freqr, min.freq=20)
 wordcloud(names(freqr),freqr,min.freq=20,colors=brewer.pal(6,"Dark2"))
 
 
 ### Text Summarization
msg<-grp_chat$Message
#install.packages("LSAfun")
 library(LSAfun)
 genericSummary(msg,k=1)
 
install.packages("lexRankr")
library(lexRankr)
lexRank(c("This is a test.","Tests are fun.",
           "Do you think the exam will be hard?","Is an exam the same as a test?",
           "How many questions are going to be on the exam?"))
msg<-as.character(msg)
lexRank(msg) 
 
concordance_str <- function(string, pattern, span=5)
{
  str_match(string, sprintf(".{0,%d}%s.{0,%d}", span, pattern, span))
}
concordance_str(msg,"utd")
genericSummary(msg,k=1,split = "\n")


D <- "This is just a test document. It is set up just to throw some random 
sentences in this example. So do not expect it to make much sense. Probably, even 
the summary won't be very meaningful. But this is mainly due to the document not being
meaningful at all. For test purposes, I will also include a sentence in this 
example that is not at all related to the rest of the document. Lions are larger than cats."
lexRank(D)
genericSummary(D,k=1)

as.character(msg)

for(i in seq(1:length(msg)))
{
  msg[1]<-paste(c(msg[1],msg[i+1]), collapse='. ' )
}
genericSummary(msg[1],k=1)
D<-c("Seniors/students who r currently studying in UTD currently What the batch size for ITM for Fall 2016 is it 1200 for fall or it includes spring students as well?."," Hello Folks ."," has anyone applied for the UTD scholarship ?.","Yes i have."," Hey Guys.","have just received an Admit for MSBA from UTD.","Congrats.","I hope all have received Admits on this group ?.","Thx man !!.","When did u submit the application?.","I guess a first week of Jan.","Ok.","Can we complete the UTD MSBA program in less than 18 months?.","Yes you can!!.","Can u elaborate please? Pros and cons.","Cons Another is it may get hard to complete degree without a full time offer.","Pros Organization may file your H1B in the following March.","Cool thanks????.","Did anyone get unsecured loan at a intetest rate less than 12%.","Even if early bt need a job else in 3 mnths home calling.","Lol no."," 12:95%.","Its 3+3 months.","Upto 6 months.","And even later than that people take letters sanctioning OPT ")
lexRank(D)

grp_chat[,c(7,8,9)]
paste0(grp_chat$Message[1],grp_chat$X[1],grp_chat$X.1[1])
msg<-paste0(grp_chat$Message,grp_chat$X,grp_chat$X.1)







