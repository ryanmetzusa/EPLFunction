#Creating a function with dateaug and season as function variables
EPL_Standings <- function(dateaug, Season) {
#Importing library
library(tidyverse)
library(lubridate)
#Create a if statement to read in Season inputted in function. Will return a statement if season is not offered.
  if (Season == "2020/21"){
    EPL <-read.csv("https://www.football-data.co.uk/mmz4281/2021/E0.csv")
  } else if (Season == "2021/22"){
    EPL <- read.csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv")
  } else if (Season == "2022/23"){
    EPL <- read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv")
  } else {return(print("Not valid season"))}
  
#Binding pulled DF into one DF then mutating and filtering data frame by dateaug
EPL <- EPL %>% mutate(Date = dmy(Date)) %>% filter(Date <= dmy(dateaug))


#Make two different DF for HomeTeam and AwayTeam to calculate record.
EPLH <- EPL %>% rename(TeamName = HomeTeam) %>% rename(TeamGoals = FTHG) %>% rename(OpponentGoals = FTAG)
EPLH <- EPLH %>% 
  select(Date, TeamName, TeamGoals, OpponentGoals, FTR) %>%
  mutate(Win = (ifelse(EPL$FTR %in% "H", 1, 0)),
         Loss = ifelse(EPL$FTR %in% "A", 1, 0),
         Draw = ifelse(EPL$FTR %in% "D",1,0))
EPLA <- EPL %>% rename(TeamName = AwayTeam) %>% rename(TeamGoals = FTAG) %>% rename(OpponentGoals = FTHG)
EPLA <- EPLA %>% 
  select(Date, TeamName, TeamGoals, OpponentGoals, FTR) %>%
  mutate(Win = (ifelse(EPL$FTR %in% "A", 1, 0)),
         Loss = ifelse(EPL$FTR %in% "H", 1, 0),
         Draw = ifelse(EPL$FTR %in% "D",1,0))

#To calculate HomeRecord and AwayRecord, created its own dataframe for each to call in final dataframe
HRec <- EPLH %>%
  group_by(TeamName) %>%
      summarise(HomeRec = paste(sum(Win %in% 1),"-",sum(Loss %in% 1),"-",sum(Draw %in% 1)))
ARec <- EPLA %>%
  group_by(TeamName) %>%
  summarise(AwayRec = paste(sum(Win %in% 1),"-",sum(Loss %in% 1),"-",sum(Draw %in% 1)))
#Bind Dataframes created earlier to standardize teamnames and count win/loss/draws.
EPLX <- bind_rows(EPLH,EPLA)
#Created a last10 dataframe for last10 function to call in final dataframe.
LTRec <- EPLX %>%
    group_by(TeamName) %>%
    summarize(Last10 = paste(sum(head(Win,10) %in% 1),"-",sum(head(Loss,10) %in% 1),"-",sum(head(Draw,10) %in% 1)))
#Created the final dataframe from EPLX (the bound dataframe) and summarize the required performance indicators
#Then Mutated the records (HRec,Arec,LTRec) into the table.
EPLF <- EPLX %>%
  group_by(TeamName) %>%
  summarize(Record = paste(sum(Win %in% 1),"-",sum(Loss %in% 1),"-",sum(Draw %in% 1)),
            MatchesPlayed = n(),
            Points = sum(TeamGoals),
            PPM = Points/MatchesPlayed,
            PtPct = Points/3 * MatchesPlayed,
            GS = sum(TeamGoals),
            GSM = GS/MatchesPlayed,
            GA = sum(OpponentGoals),
            GAM = GA/MatchesPlayed) %>%
  mutate(HomeRec = HRec$HomeRec,
         AwayRec = ARec$AwayRec,
         Last10 = LTRec$Last10)
#Rearranged the columns into the correct order for the result. Then arrange by PPM,Record, GSM, GAM
EPLF <- EPLF %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM,Last10) %>%
  arrange(desc(PPM), desc(Record), GSM, GAM)
#return the final dataframe when the function is called.
return(EPLF)
}

EPL_Standings("07/11/2021","2021/22")