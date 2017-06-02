
# Load Libraries
library(ggplot2)
library(ggthemes)
library(pscl) # Logistic Regression
library(ROCR) # ROCR Curve

#Load Data
seasons <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/Seasons.csv")
fullSeasonData <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonDetailedResults.csv")
compactSeasonData <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonCompactResults.csv")
teams <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/Teams.csv")
tourneyData <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/TourneyDetailedResults.csv")
compactTourneyData <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/TourneyCompactResults.csv")
seeds <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/TourneySeeds.csv")
submission <- read.csv("D:/My Stuff/Kaggle/march ml mania 2017/sample_submission.csv")

seeds$pureSeed <- as.numeric(substr(seeds$Seed, 2,3))  # Extract the substring from the 'seed' value starting with the second character and going to the third character, then convert to a numeric and store as new variable 'pureSeed'
seeds$region <- as.character(substr(seeds$Seed,1,1)) #Extract the region as well, which we'll need for calculating dates of games later

compactTourneyData$team1 <- ifelse(compactTourneyData$Wteam > compactTourneyData$Lteam, compactTourneyData$Lteam, compactTourneyData$Wteam) #If the ID of the winning team is higher than the ID of the losing team, team1 is the losing team, else its the winning team
compactTourneyData$team2 <- ifelse(compactTourneyData$Wteam > compactTourneyData$Lteam, compactTourneyData$Wteam, compactTourneyData$Lteam) #Vice versa to find team2
compactTourneyData$team1Victory <- ifelse(compactTourneyData$Wteam == compactTourneyData$team1, 1, 0) #Create a "Team1 Victory" binary variable

# The first step is parsing the detailed regular season results into team stats per game
winnerHistory <- fullSeasonData[,c("Season","Wteam","Daynum","Wscore","Numot","Wfgm","Wfga","Wfgm3","Wfga3","Wftm","Wfta","Wor","Wdr","Wast","Wto","Wstl","Wblk","Wpf")]
winnerHistory$Victory <- 1
loserHistory <- fullSeasonData[,c("Season","Lteam","Daynum","Lscore","Numot","Lfgm","Lfga","Lfgm3","Lfga3","Lftm","Lfta","Lor","Ldr","Last","Lto","Lstl","Lblk","Lpf")]
loserHistory$Victory <- 0

# Now we normalize the column names before combining the two dataframes
names(winnerHistory) <- c("season","team","daynum","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul","victory")
names(loserHistory) <- c("season","team","daynum","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul","victory")
teamHistory <- rbind(winnerHistory, loserHistory)

# We'll likely use this teamHistory archive for several different things, but for now we'll pull out season long averages for each stat for each team
teamAverages <- aggregate(teamHistory, by=list(teamHistory$season,teamHistory$team), FUN=mean, na.rm=TRUE)


#We'll start by adding both teams' season average stats to each tourney match
train1 <- merge(compactTourneyData[,c("Season","Daynum","team1","team2","team1Victory")],teamAverages[,c("season","team","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul","victory")], by.x = c("Season","team1"), by.y = c("season","team"))
names(train1)[6:21] <- paste("team1avg",names(train1)[6:21],sep = "")
train1 <- merge(train1,teamAverages[,c("season","team","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul","victory")], by.x = c("Season","team2"), by.y = c("season","team"))
names(train1)[22:37] <- paste("team2avg",names(train1)[22:37],sep = "")

# Lets also pull in their respective seeds, as I suspect the most important independent variable will involve seeds
train1 <- merge(train1, seeds[,c("Season","Team","pureSeed","region")], by.x = c("Season","team1"), by.y = c("Season","Team"))
colnames(train1)[colnames(train1)=="pureSeed"] <- "seed1"
colnames(train1)[colnames(train1)=="region"] <- "region1"
train1 <- merge(train1, seeds[,c("Season","Team","pureSeed", "region")], by.x = c("Season","team2"), by.y = c("Season","Team"))
colnames(train1)[colnames(train1)=="region"] <- "region2"

# Now comes my primary focus: the difference between both teams' seed values, which should have a powerful predictive effect
train1$seedDelta <- train1$seed1 - train1$pureSeed

# While we're at it, it might not hurt to calculate deltas for all of the stats
train1$scoreDiff <- train1$team1avgscore - train1$team2avgscore
train1$numotDiff <- train1$team1avgnumot - train1$team2avgnumot
train1$fgmadeDiff <- train1$team1avgfgmade - train1$team2avgfgmade
train1$fgattemptDiff <- train1$team1avgfgattempt - train1$team2avgfgattempt
train1$fgm3Diff <- train1$team1avgfgm3 - train1$team2avgfgm3
train1$fga3Diff <- train1$team1avgfga3 - train1$team2avgfga3
train1$ftmadeDiff <- train1$team1avgftmade - train1$team2avgftmade
train1$ftattemptDiff <- train1$team1avgftattempt - train1$team2avgftattempt
train1$offrebDiff <- train1$team1avgoffreb - train1$team2avgoffreb
train1$defrebDiff <- train1$team1avgdefreb - train1$team2avgdefreb
train1$astDiff <- train1$team1avgast - train1$team2avgast
train1$turnoverDiff <- train1$team1avgturnover - train1$team2avgturnover
train1$stealDiff <- train1$team1avgsteal - train1$team2avgsteal
train1$blockDiff <- train1$team1avgblock - train1$team2avgblock
train1$pfoulDiff <- train1$team1avgpfoul - train1$team2avgpfoul
train1$victoryDiff <- train1$team1avgvictory - train1$team2avgvictory

# One last bit of data housekeeping: We need to convert Daynum into a broader measure of the tournament round, since we can't forecast which specific day any two teams will play during rounds 1-4
roundConverter <- function(x){return(switch(as.character(x),'134'=0,'135'=0,'136'=1,'137'=1,'138'=2,'139'=2,'143'=3,'144'=3, '145'=4,'146'=4,'152'=5,'154'=6))}
train1$round <- sapply(train1$Daynum, roundConverter)



upsets <- subset(train1, seedDelta > 0 & team1Victory == TRUE) #This identifies upsets where team 1 is the victorious underdog
upsets <- rbind(upsets, subset(train1, seedDelta < 0 & team1Victory == FALSE)) #This identifies upsets where team 2 is the victorious underdog
1 - dim(upsets)[1] / dim(train1)[1]  # 1 - (# of upsets)/(# of total tourney games) = Accuracy of playing a perfectly "By the books" bracket, which assumes in every match the stronger seed team will win

ggplot(aes(x = upsets$Daynum, y = abs(upsets$seedDelta)), data =  upsets) +
  geom_point(position = position_jitter(width = .6, height = .4))

train <- train1[which(train1$Season <= 2012),]
test <- train1[which(train1$Season >= 2013),]

lr_model <- glm(team1Victory ~ pureSeed + seed1
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Let's visualize the accuracy of this analysis by plotting true positive rate against false positive rate
predict <- predict(lr_model, type = 'response')
ROCRpred <- prediction(predict, train$team1Victory)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE) #The area under this curve is one approximation of the model's accuracy

acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute same acc measure as above
ROCRpred <- prediction(test_prediction, test$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/268

lr_model <- glm(team1Victory ~ pureSeed + seed1 + round
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Let's visualize the accuracy of this analysis by plotting true positive rate against false positive rate
predict <- predict(lr_model, type = 'response')
ROCRpred <- prediction(predict, train$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute same acc measure as above
ROCRpred <- prediction(test_prediction, test$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/268

lr_model <- glm(team1Victory ~ pureSeed + seed1 + round + team1avgscore + team1avgvictory + team2avgscore + team2avgvictory
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Let's visualize the accuracy of this analysis by plotting true positive rate against false positive rate
predict <- predict(lr_model, type = 'response')
ROCRpred <- prediction(predict, train$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc


#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute same acc measure as above
ROCRpred <- prediction(test_prediction, test$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/268


lr_model <- glm(team1Victory ~ pureSeed + seed1 + round + team1avgscore + team1avgvictory + team2avgscore +
                  team2avgvictory + team1avgnumot + team1avgfgmade + team1avgfgattempt + team1avgfgm3 + 
                  team1avgfga3 + team1avgftmade + team1avgftattempt + team1avgoffreb + team1avgdefreb + team1avgast + 
                  team1avgturnover + team1avgsteal +team1avgblock + team1avgpfoul + 
                  team2avgnumot + team2avgfgmade + team2avgfgattempt + team2avgfgm3 + team2avgfga3 + team2avgftmade + 
                  team2avgftattempt + team2avgoffreb + team2avgdefreb + team2avgast + team2avgturnover + 
                  team2avgsteal + team2avgblock + team2avgpfoul
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Let's visualize the accuracy of this analysis by plotting true positive rate against false positive rate
predict <- predict(lr_model, type = 'response')
ROCRpred <- prediction(predict, train$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute same acc measure as above
ROCRpred <- prediction(test_prediction, test$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/268

lr_model <- glm(team1Victory ~ seedDelta + round + scoreDiff + numotDiff + fgmadeDiff + fgattemptDiff +
                  fgm3Diff + fga3Diff + ftmadeDiff + ftattemptDiff + offrebDiff + defrebDiff +
                  astDiff + turnoverDiff + stealDiff + blockDiff + pfoulDiff + victoryDiff
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Let's visualize the accuracy of this analysis by plotting true positive rate against false positive rate
predict <- predict(lr_model, type = 'response')
ROCRpred <- prediction(predict, train$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

test_prediction <- predict(lr_model, test, type = 'response')
#Compute same acc measure as above
ROCRpred <- prediction(test_prediction, test$team1Victory)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/268

#This function is the culmination of several hours of manual analysis of the NCAA bracket structure. I'm fully aware of how strange and disjointed its various segments appear, and explaining the logic of it falls outside the scope of this kernel, but I'll give a basic overview in comments
roundFinder <- function(region1, region2, seed1, seed2){
  if(region1 == region2 && seed1 == seed2){return(0)} #This is for the 'First Four', the play-in games
  else if(region1 != region2){   
    if((region1 == 'W' && region2 == 'X')||(region1=='X' && region2=='W')|| (region1=='Y'&&region2=='Z')|| (region1=='Z'&&region2=='Y')){ 
      return(5)     #If the two teams are from different regions, but both from either the left side or right side of the bracket (left side is W and X, right side is Y and Z), they will play each other in round 5, the semi final
    }
    else return(6)  #If the two teams are from different regions and different sides of the bracket, they'll only play each other in the championship, round 6
  }
  else{  #Now it gets finnicky. Within each regional bracket, about half of the matchup rounds can be determined by simply adding the two teams' seeds together. The other half need to be called out individually.
    z <- as.character(as.numeric(seed1) + as.numeric(seed2))
    #We also need to put the lower seed first, so we don't have to do twice as many individual callouts
    x <- as.character(min(c(as.numeric(seed1),as.numeric(seed2))))
    y <- as.character(max(c(as.numeric(seed1),as.numeric(seed2))))
    #This first switch function checks the seed sum to see if it can identify the round
    w <- switch(z, '17'=1, '9'=2, '25'=2, '2'=4, '3'=4, '4'=4, '5'=3, '7'=4, '11'=4, '13'=3, '15'=4, '19'=4, '21'=3, '23'=4, '27'=4, '29'=3, '30'=4, '31'=4)      
    if(is.numeric(w)){
      return(w)
    }
    else{ #If the first switch didn't find an answer, this one will. It calls out the remaining combinations and returns the correct round
      y <- as.character(y) 
      return(switch(as.character(x), '1'=switch(y,'9'=2,'5'=3,'13'=3,'7'=4,'11'=4,'15'=4),'2'=switch(y,'10'=2,'6'=3,'14'=3,'4'=4,'8'=4,'12'=4,'16'=4),'3'=switch(y, '11'=2,'7'=3,'15'=3,'5'=4,'9'=4,'13'=4),'4'=switch(y,'12'=2,'8'=3,'16'=3,'6'=4,'10'=4,'14'=4),'5'=switch(y,'13'=2,'9'=3,'7'=4,'11'=4,'15'=4),'6'=switch(y,'14'=2,'10'=3,'8'=4,'12'=4,'16'=4),'7'=switch(y,'15'=2,'11'=3,'9'=4,'13'=4),'8'=switch(y,'16'=2,'12'=3,'10'=4,'14'=4),'9'=switch(y,'13'=3,'11'=4,'15'=4),'10'=switch(y,'14'=3,'12'=4,'16'=4),'11'=switch(y,'15'=3,'13'=4),'12'=switch(y,'16'=3,'14'=4),'13'=4))
    }
  }
}

#Now lets test it on the existing data to verify its accuracy
train1$roundCheck <- unlist(mapply(roundFinder,train1$region1,train1$region2,train1$seed1,train1$pureSeed, SIMPLIFY = TRUE), use.names = FALSE)
train1$roundDelta <- train1$round - train1$roundCheck
summary(train1$roundDelta)


test$prediction <- test_prediction
test$submissionID <- paste(test$Season, test$team1, test$team2, sep = "_")

submission <- merge(submission, test[,c("submissionID","prediction")], by.x = c("id"), by.y = c("submissionID"), all.x = TRUE)
submission$pred <- ifelse(is.na(submission$prediction),submission$pred, submission$prediction)
submission$prediction <- NULL
write.csv(submission, file="D:/My Stuff/Kaggle/march ml mania 2017/LogisticRegressionSubmission.csv", row.names=FALSE)





















