teams<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/Teams.csv")
season_result<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonCompactResults.csv")
season_detailed_result<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonDetailedResults.csv")

tourney_seeds<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/TourneySeeds.csv")
tourney_slots<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/TourneySlots.csv")

# Merging Teams with Regular seasoncompactresults
names(season_result)
names(teams)
season_result1<-season_result
colnames(season_result1)[3]<-"Team_Id"
names(season_result1)
season_result1<-merge(season_result1,teams,by="Team_Id")
colnames(season_result1)[1]<-"Wteam"
colnames(season_result1)[5]<-"Team_Id"
colnames(season_result1)[9]<-"WTeam_Name"
season_result1<-merge(season_result1,teams,by="Team_Id")
names(season_result1)
colnames(season_result1)[1]<-"Lteam"
colnames(season_result1)[10]<-"LTeam_Name"
names(season_result1)


# Merging Tourney_Slots with Tourney_seeds
colnames(tourney_slots)[3]<-"Seed"
merged_tourney<-merge(tourney_slots,tourney_seeds,by=c("Season","Seed"))
names(merged_tourney)
colnames(merged_tourney)[2]<-"Strongseed"
colnames(merged_tourney)[5]<-"StrongseedName"
colnames(tourney_slots)[3]<-"Strongseed"
colnames(tourney_slots)[4]<-"Seed"

colnames(merged_tourney)[4]<-"Seed"
merged_tourney<-merge(merged_tourney,tourney_seeds,by=c("Season","Seed"))
names(merged_tourney)
colnames(merged_tourney)[2]<-"Weakseed"
colnames(merged_tourney)[6]<-"WeakSeedName"
names(merged_tourney)

write.csv(merged_tourney,file="D:/My Stuff/Kaggle/march ml mania 2017/merged_tourney.csv")
write.csv(season_result1,file="D:/My Stuff/Kaggle/march ml mania 2017/season_results.csv")





