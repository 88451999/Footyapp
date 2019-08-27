library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)

setwd("/GitDev/Footyapp/data/") # use here function instead
FootyGames <- data.frame()
if(file.exists("FootyGamesFull.csv")){
  FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)
}

footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)
#footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)
games <- unique(footy[! footy$gameID %in% FootyGames$gameID, ]$gameID)

#footy_match <- footy %>% filter(ourName == " Socceroos")
#footy_match <- footy
print(paste("Number of games -", length(games)))

for (j in 1:length(games)) {
  print(paste(j, games[j]))
  footy_match <- footy %>% filter(gameID == games[j])
  
  kickoff = FALSE
  prevPlayNumber = 0
  half = 1
  footy_match$half = 0
  footy_match$attackIncursion = 0
  footy_match$adjEventName = ""
  footy_match$penaltyIncursion = 0
  footy_match$adjPass = 0
  footy_match$playNumber = 0
  footy_match$sequenceNumber = 0
  footy_match$isBackPass = 0
  footy_match$isFwdPass = 0
  footy_match$isSidePass = 0
  footy_match$isPass10m = 0
  footy_match$isPass10_20m = 0
  footy_match$isPass20m = 0
  footy_match$usPhase = ""
  footy_match$oppositionPhase = ""
  footy_match$pseudoEvent = FALSE
  footy_match$adjEventName = case_when(footy_match$eventName != "dribble" ~ footy_match$eventName,
                                       footy_match$distanceToNextEventMetres / footy_match$duration > 8 ~ "first touch",
                                       footy_match$distanceToNextEventMetres > 15 ~ "first touch",
                                       TRUE ~ footy_match$eventName)
  footy_match$player = 0
  footy_match$zone = ""
  footy_match$maxSeq = 0
  footy_match$isAttackIncursion = 0
  footy_match$isGoal = 0
  footy_match$totPass = 0
  footy_match$firstEvent = ""
  footy_match$playByUs = ""
  footy_match$matchName = ""

  for (i in 1:nrow(footy_match)) {
  
     footy_match[i, ]$half = half
     if (!kickoff) {
       if (footy_match[i, ]$adjEventName == "first touch" ) {
         footy_match[i, ]$adjEventName = "kick off"
         kickoff = TRUE
         footy_match[i, ]$playNumber = 1
         footy_match[i, ]$sequenceNumber = 1
         footy_match[i, ]$player = 1
         
         if (footy_match[i, ]$byUs == "true") {
           footy_match[i, ]$usPhase = "BP"
           footy_match[i, ]$oppositionPhase = "BPO"
         } else {
           footy_match[i, ]$usPhase = "BPO"
           footy_match[i, ]$oppositionPhase = "BP"
         } 
       }
     # If after the kickoff then decide what has happened
     } else {
         if (footy_match[i, ]$adjEventName %in% c("change ends", "finish")) {
          print("change ends")
           if (footy_match[i, ]$adjEventName == "change ends") {
             half = half + 1
           }
         } else if (footy_match[i-1, ]$adjEventName %in% c("sideline out", "sideline out defence") &
           footy_match[i, ]$adjEventName == "first touch") {       
           footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "throw in"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
  
        } else if (footy_match[i-1, ]$adjEventName == "goalkick" &
                   footy_match[i, ]$adjEventName == "first touch") {
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "goal kick"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
         } else if (footy_match[i-1, ]$adjEventName == "corner" &
                    footy_match[i, ]$adjEventName == "first touch") {
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "corner kick"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
         } else if (footy_match[i-1, ]$adjEventName == "penalty" &
                    footy_match[i, ]$adjEventName == "first touch") {
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "penalty kick"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
         } else if (footy_match[i, ]$adjEventName == "keeper") {
           # first touch is outside the penalty area so punt has been missed
           # Insert a kick..
             if ((footy_match[i+1, ]$lengthMetres > 16.5 |
                 footy_match[i+1, ]$widthMetres > (75 - 17.34) |
                 footy_match[i+1, ]$widthMetres < 17.34) &
                 !(footy_match[i+1, ]$adjEventName %in% c("change ends", "finish")) ) {

                 # Put in an end marker for this play when there is a turnover
               footy_match[i, ]$playNumber = prevPlayNumber + 1
               footy_match[i, ]$sequenceNumber = 1
               footy_match[i, ]$player = 1
               footy_match[i, ]$adjEventName = "keeper punt"
               if (footy_match[i, ]$byUs == "true") {
                 footy_match[i, ]$usPhase = "BP"
                 footy_match[i, ]$oppositionPhase = "BPO"
               } else {
                 footy_match[i, ]$usPhase = "BPO"
                 footy_match[i, ]$oppositionPhase = "BP"
               }

             }
         } else if (footy_match[i+1, ]$adjEventName == "keeper") {
           
           footy_match[i, ]$playNumber = prevPlayNumber 
           footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_match[i, ]$player = footy_match[i-1, ]$player + 1
           footy_match[i, ]$adjEventName = "keeper catch"
           footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
           footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
           
         } else if (footy_match[i-1, ]$adjEventName == "keeper" &
                    !(footy_match[i, ]$adjEventName %in% c("change ends", "finish")) &
                    !(footy_match[i, ]$lengthMetres > 16.5 |
                      footy_match[i, ]$widthMetres > (75 - 17.34) |
                      footy_match[i, ]$widthMetres < 17.34)) {
          
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "keeper punt"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
           
         } else if (footy_match[i, ]$adjEventName %in% c("offside", "direct freekick", "indirect freekick")) {
           footy_match[i, ]$playNumber = prevPlayNumber
           footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_match[i, ]$player = footy_match[i-1, ]$player
           
         } else if (footy_match[i-1, ]$adjEventName %in% c("offside", "direct freekick", "indirect freekick")) {
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = case_when(footy_match[i-1, ]$adjEventName == "offside" ~ "offside restart",
                                                     footy_match[i-1, ]$adjEventName == "direct freekick" ~ "direct restart",
                                                     TRUE ~ "indirect restart")
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
           
         } else if (footy_match[i-1, ]$adjEventName == "goal" &
                    footy_match[i, ]$adjEventName == "first touch") {
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$player = 1
           footy_match[i, ]$adjEventName = "kick off"
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BP"
             footy_match[i, ]$oppositionPhase = "BPO"
           } else {
             footy_match[i, ]$usPhase = "BPO"
             footy_match[i, ]$oppositionPhase = "BP"
           } 
        # Include action before sideline out/corner/goal kick as part of same play
         } else if (footy_match[i+1, ]$adjEventName %in% c("sideline out", "corner", "goalkick") &
                    footy_match[i, ]$byUs != footy_match[i-1, ]$byUs) {
           footy_match[i, ]$playNumber = prevPlayNumber
           footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
           footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
           footy_match[i, ]$player = footy_match[i-1, ]$player + 1
           if (footy_match[i+1, ]$adjEventName == "sideline out") {
             footy_match[i+1, ]$adjEventName = "sideline out defence"
           }
           
         } else if (footy_match[i, ]$byUs == footy_match[i-1, ]$byUs) {
           footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_match[i, ]$playNumber = prevPlayNumber
           footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
           footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
           footy_match[i, ]$player = case_when(footy_match[i-1, ]$adjEventName %in% c("first touch", "dribble") &
                                                 footy_match[i, ]$adjEventName %in% c("dribble") ~ footy_match[i-1, ]$player,
                                               footy_match[i, ]$adjEventName %in% c("sideline out", "corner", "goalkick", "shot", "rebound", "goal",
                                                                                    "sideline out defence") ~ 
                                                 footy_match[i-1, ]$player,
                                               TRUE ~ footy_match[i-1, ]$player + 1)
            
         # turnover ball
         } else {
           # Put in an end marker for this play when there is a turnover
           footy_extra = footy_match[i, ]
           footy_extra$pseudoEvent = TRUE
           footy_extra$lengthFraction = 1 - footy_extra$lengthFraction
           footy_extra$lengthMetres = 110 - footy_extra$lengthMetres 
           footy_extra$widthFraction = 1 - footy_extra$widthFraction
           footy_extra$widthMetres = 75 - footy_extra$widthMetres         
           footy_extra$playNumber = prevPlayNumber
           footy_extra$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
           footy_extra$player = footy_match[i-1, ]$player + 1
           footy_extra$usPhase = footy_match[i-1, ]$usPhase
           footy_extra$oppositionPhase = footy_match[i-1, ]$oppositionPhase
           footy_extra$adjEventName = "turnover"
           
           footy_match <- rbind(footy_match, footy_extra)
  
           footy_match[i, ]$sequenceNumber = 1
           footy_match[i, ]$playNumber = prevPlayNumber + 1
           footy_match[i, ]$player = 1
           
           if (footy_match[i, ]$byUs == "true") {
             footy_match[i, ]$usPhase = "BPO > BP"
             footy_match[i, ]$oppositionPhase = "BP > BPO"
           } else { 
             footy_match[i, ]$usPhase = "BP > BPO"
             footy_match[i, ]$oppositionPhase = "BPO > BP"
           }  
           
         }       
           
      }
   
    # sort out was it an attacking incursion, penalty area incursion
    # and track previous play number
    if (footy_match[i, ]$playNumber > 0) {
      prevPlayNumber = footy_match[i, ]$playNumber
      if (footy_match[i, ]$byUs == footy_match[i+1, ]$byUs) {
          if (footy_match[i, ]$adjEventName %in% c("throw in", "dribble", "kick off", "first touch", 
                                               "goal kick", "keeper punt", "corner kick",
                                               "offside restart", "direct restart", "indirect restart") &
              footy_match[i+1, ]$adjEventName %in% c("first touch")) {
           footy_match[i, ]$adjPass = 1
          }
          if (footy_match[i, ]$lengthFraction >= 0.66666667) {
            footy_match[i, ]$attackIncursion = 1
          }
          if (footy_match[i, ]$lengthMetres >= (110 - 16.5) &
              footy_match[i, ]$widthMetres <= (75 - 17.34) &
              footy_match[i, ]$widthMetres >= 17.34) {
            footy_match[i, ]$penaltyIncursion = 1
          }
      }
    }
    
  }
  FootyGames <- rbind(FootyGames, footy_match)
   
}

FootyGames[is.na(FootyGames$direction), ]$direction <- 0

FootyGames$isBackPass <- ifelse(FootyGames$adjPass == 1 &
                                  FootyGames$lengthChangeMetres < 0 &
                                  (abs(FootyGames$direction) < 75 |  
                                   abs(FootyGames$direction) > 105), 1, 0)
FootyGames$isFwdPass <- ifelse(FootyGames$adjPass == 1 &
                                  FootyGames$lengthChangeMetres > 0 &
                                  (abs(FootyGames$direction) < 75 |  
                                     abs(FootyGames$direction) > 105), 1, 0)
FootyGames$isSidePass <- ifelse(FootyGames$adjPass == 1 &
                                 (abs(FootyGames$direction) >= 75 &  
                                    abs(FootyGames$direction) <= 105), 1, 0)
FootyGames$isPass10m <- ifelse(FootyGames$isFwdPass == 1 &
                                 FootyGames$distance <= 10, 1, 0)
FootyGames$isPass10_20m <- ifelse(FootyGames$isFwdPass == 1 &
                                 FootyGames$distance >10 &
                                    FootyGames$distance <= 20, 1, 0)
FootyGames$isPass20m <- ifelse(FootyGames$isFwdPass == 1 &
                                 FootyGames$distance > 20, 1, 0)
FootyGames$zone <- (floor(ifelse(FootyGames$widthFraction== 1, 2, 
                                 FootyGames$widthFraction*3))+1)+(3*floor(ifelse(FootyGames$lengthFraction>= 1, 5, 
                      FootyGames$lengthFraction*6)))
FootyGames %>% filter(zone > 18) %>% select(adjEventName,
                                            widthFraction, lengthFraction, zone)
FootyGames <- FootyGames %>% group_by(gameID, byUs, playNumber) %>%
  mutate(maxSeq = max(sequenceNumber), 
         isAttackIncursion = max(attackIncursion),
         isGoal = ifelse(last(adjEventName == "goal"), 1, 0),
         totPass = sum(adjPass),
         firstEvent = first(adjEventName),
         playByUs = first(byUs),
         matchName = paste(format(ymd(substr(time, 1, 10)), "%d/%m/%y"), "-", theirName) )


write.csv(FootyGames, "FootyGamesFull.csv", row.names = FALSE)
FgUs <- FootyGames %>% filter(competition == "2019 NSFA 1 Boys Under 13")
write.csv(FgUs, "FootyGamesUs.csv", row.names = FALSE)
FgUs <- FootyGames %>% filter(competition == "2019 NSFA 1 Girls Under 16")
write.csv(FgUs, "FootyGames16.csv", row.names = FALSE)


footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(playByUs = first(byUs),
            matchName = first(matchName),
            gameDate = first(ymd(substr(time, 1, 10))),
            usPhase = first(usPhase),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            Area = first(case_when(lengthFraction <= 0.333333 ~ "Defence",
                                   lengthFraction >= 0.666667 ~ "Attack",
                                   TRUE ~ "Midfield") ),
            firstEventArea = paste(firstEvent, "-", Area),
            firstPenalyIncursion = first(penaltyIncursion),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            isAttackIncursion = max(attackIncursion),
            isPenaltyIncursion = max(penaltyIncursion),
            playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
            totalPasses = sum(adjPass),
            possessionDuration = sum(ifelse(pseudoEvent == FALSE &
                                              byUs == playByUs &
                                              eventName != "keeper", duration, 0)),
            playDuration = sum(duration),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)) )
fmp_Us <- footy_match_play %>% filter(competition == "2019 NSFA 1 Boys Under 13")
write.csv(fmp_Us, "FootyMatchPlayUs.csv", row.names = FALSE)
fmp_Us <- footy_match_play %>% filter(competition == "2019 NSFA 1 Girls Under 16")
write.csv(fmp_Us, "FootyMatchPlay16.csv", row.names = FALSE)





FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)

sum(footy_match_play$isZone17Incursion, na.rm = TRUE)
team <- "NFC Red"
FootyGames$kickoff_date = as.POSIXct(substr(FootyGames$time, 1, 19), format='%Y-%m-%d %H:%M:%S')
footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(gameDate = first(kickoff_date),
            playByUs = first(byUs),
            teamName = first(ifelse(playByUs == "true", ourName, theirName)),
            usPhase = first(usPhase),
            half = first(half),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            Area = first(case_when(lengthFraction <= 0.333333 ~ "Defence",
                                   lengthFraction >= 0.666667 ~ "Attack",
                                   TRUE ~ "Midfield") ),
            lastArea = last(case_when(lengthFraction <= 0.333333 ~ "Defence",
                                   lengthFraction >= 0.666667 ~ "Attack",
                                   TRUE ~ "Midfield") ),
            firstEventArea = paste(firstEvent, "-", Area),
            firstPenalyIncursion = first(penaltyIncursion),
            firstZone = first(zone),
            lastZone = last(zone),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            distance = sum(ifelse(lead(byUs) == byUs &
                                    lead(adjEventName) %in% c("first touch", "dribble", "shot", "goal"), 
                                  distanceToNextEventMetres, 0)),
            isAttackIncursion = max(attackIncursion),
            isPenaltyIncursion = max(penaltyIncursion),
            isZone14Incursion = max(ifelse(zone==14 & byUs == playByUs, 1, 0), na.rm=TRUE),
            isZone17Incursion = max(ifelse(zone==17 & byUs == playByUs, 1, 0), na.rm=TRUE),
            playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
            totalPasses = sum(adjPass),
            totalBackPasses = sum(isBackPass),
            totalFwdPasses = sum(isFwdPass),
            totalSidePasses = sum(isSidePass),
            total10mPasses = sum(isPass10m),
            total10_20mPasses = sum(isPass10_20m),
            total20mPasses = sum(isPass20m),
            possessionDuration = sum(ifelse(pseudoEvent == FALSE &
                                              byUs == playByUs &
                                              eventName != "keeper", duration, 0)),
            AttackDuration = sum(ifelse(pseudoEvent == FALSE &
                                              byUs == playByUs &
                                              eventName != "keeper" &
                                          Area == "Attack", duration, 0)),
            MidfieldDuration = sum(ifelse(pseudoEvent == FALSE &
                                          byUs == playByUs &
                                          eventName != "keeper" &
                                          Area == "Midfield", duration, 0)),
            DefenceDuration = sum(ifelse(pseudoEvent == FALSE &
                                          byUs == playByUs &
                                          eventName != "keeper" &
                                          Area == "Defence", duration, 0)),
            AttackHalfDuration = sum(ifelse(pseudoEvent == FALSE &
                                           byUs == playByUs &
                                           eventName != "keeper" &
                                           lengthMetres >= 55, duration, 0)),
            DefenceHalfDuration = sum(ifelse(pseudoEvent == FALSE &
                                               byUs == playByUs &
                                               eventName != "keeper" &
                                               lengthMetres < 55, duration, 0)),
            playDuration = sum(duration),
            crossfieldPlay = ifelse(max(widthMetres) - min(widthMetres) > 50, 1, 0),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)) )
footy_match_play$orderCol <- case_when(footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)


write.csv(footy_match_play, "fooy_match_play.csv", row.names = FALSE)
footy_match_play <- read.csv("fooy_match_play.csv", stringsAsFactors = FALSE)
footy_match_play$joinColumn <- 1

joinColumn <- c(1, 1)
UsTeam <- c(1, 2)
duplicate <- data.frame(joinColumn, UsTeam)
fmp <- footy_match_play %>%
  full_join(duplicate, c("joinColumn" = "joinColumn"))

footy_match_game1 <- footy_match_play %>%
  group_by(competition, gameID, teamName, playByUs, joinColumn) %>%
  summarise(gameDate= min(gameDate),
         gameDuration = sum(possessionDuration),
         gameAttackDuration = sum(AttackDuration),
         gameMidfieldDuration = sum(MidfieldDuration),
         gameDefenceDuration = sum(DefenceDuration),
         gameDistance = sum(distance),
         goals = sum(goals),
         shots = sum(shots),
         ppm = sum(totalPasses)/sum(possessionDuration/60),
         attackPlay = sum(ifelse(lastArea == "Attack", 1, 0)),
         midfieldPlay = sum(ifelse(lastArea == "Midfield", 1, 0)),
         defencePlay = sum(ifelse(lastArea == "Defence", 1, 0)),
         penaltyPlay = sum(ifelse(isPenaltyIncursion == 1, 1, 0)),
         zone14Play = sum(ifelse(isZone14Incursion == 1, 1, 0)),
         zone17Play = sum(ifelse(isZone17Incursion == 1, 1, 0)),
         backPass = sum(totalBackPasses),
         sidePass = sum(totalSidePasses),
         fwdPass = sum(totalFwdPasses),
         backPlay = sum(ifelse(totalBackPasses > 0, 1, 0)),
         sidePlay = sum(ifelse(totalSidePasses > 0, 1, 0)),
         fwdPlay = sum(ifelse(totalFwdPasses > 0, 1, 0)),
         noPassPlay = sum(ifelse(totalPasses == 0, 1, 0)),
         passPlay = sum(ifelse(totalPasses > 1, 1, 0)),
         plays = n(),
         Plays1_3Passes = sum(ifelse(totalPasses > 0 & totalPasses <= 3, 1, 0)),
         Plays4_6Passes = sum(ifelse(totalPasses > 3 & totalPasses <= 6, 1, 0)),
         Plays7PlusPasses = sum(ifelse(totalPasses > 6, 1, 0)),
         playsAttTurn = sum(ifelse(Area == "Attack" & firstEvent == "first touch", 1, 0))
         ) %>%
         left_join(duplicate, c("joinColumn" = "joinColumn")) %>%
  ungroup () %>%
         group_by(competition, gameID, UsTeam) %>%
         mutate(gameAttackDurationPcnt = gameAttackDuration / sum(gameAttackDuration),
                gameMidfieldDurationPcnt = gameMidfieldDuration / sum(gameMidfieldDuration),
                gameDefenceDurationPcnt = gameDefenceDuration / sum(gameDefenceDuration),
                ppmPcnt = ppm / sum(ppm),
                result = case_when(sum(goals) == 0 ~ "Draw",
                                   goals/sum(goals) == 0.5 ~ "Draw",
                                   goals/sum(goals) < 0.5 ~ "Loss", 
                                   TRUE ~ "Win"),
                default = ifelse(result == "Draw", 1,
                                ifelse(result == "Loss", 0, 2)),
                zone14PlayPcnt = zone14Play / sum(zone14Play),
                zone17PlayPcnt = zone17Play / sum(zone17Play),
                attackPlayPcnt = attackPlay / sum(attackPlay),
                backPlayPcnt = backPlay / sum(backPlay),
                sidePlayPcnt = sidePlay / sum(sidePlay),
                fwdPlayPcnt = fwdPlay / sum(fwdPlay),
                backPassPcnt = backPass / sum(backPass),
                sidePassPcnt = sidePass / sum(sidePass),
                fwdPassPcnt = fwdPass / sum(fwdPass),
                noPassPlayPcnt = noPassPlay / sum(noPassPlay),
                passPlayPcnt = passPlay / sum(passPlay),
                playsPcnt = plays / sum(plays),
                Plays1_3PassesPcnt = Plays1_3Passes / sum(Plays1_3Passes),
                Plays4_6PassesPcnt = ifelse(sum(Plays4_6Passes) == 0, 0, Plays4_6Passes / sum(Plays4_6Passes)),
                Plays7PlusPassesPcnt = ifelse(sum(Plays7PlusPasses) == 0, 0, Plays7PlusPasses / sum(Plays7PlusPasses)),
                playsAttTurnPcnt = playsAttTurn / sum(playsAttTurn),
                shotPcnt = shots / sum(shots),
                distPcnt = gameDistance / sum(gameDistance)) %>%
             filter(UsTeam == 1)
footy_match_game1 <- footy_match_game1[!footy_match_game1$competition == "2017 Mens Open", ]

# Run RandomForest and remove ID field as this is NOT a predictor
footy_match_game1$result <- as.factor(footy_match_game1$result)
footy_match_game1$default = as.factor(footy_match_game1$default)
trainset = footy_match_game1[!footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                           "2019 NSFA 1 Boys Under 13", 
                                                                           "2019 World Cup 2019 Mens Open",             
                                                                           "2019 International Friendly Mens Open"   ,  
                                                                           "2019 Boys Under 14"                ,        
                                                                           "2019 Icc Football Mens Open"
), ]

library(DMwR)
library(caret)
# Since positive Target is small, use SMOTE to rebalance the class in Training Set
trainset$competition <- as.factor(trainset$competition)
trainset$gameID <- as.factor(trainset$gameID)
trainset$gameDate <- as.factor(trainset$gameDate)
trainset$playByUs <- as.factor(trainset$playByUs)
trainset$teamName <- as.factor(trainset$teamName)
trainset$prob <- NULL

testset = footy_match_game1[footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", "2019 NSFA 1 Boys Under 13", "2019 World Cup 2019 Mens Open",             
                                                                         "2019 International Friendly Mens Open"   ,  
                                                                         "2019 Boys Under 14"                ,        
                                                                         "2019 Icc Football Mens Open"
), ]
tx.rf <- randomForest(default ~ shotPcnt + zone14PlayPcnt + 
                        zone17PlayPcnt +
                        gameAttackDurationPcnt +
                      gameMidfieldDurationPcnt +
                      gameDefenceDurationPcnt +
                      ppmPcnt +
                      attackPlayPcnt +
                      backPlayPcnt +
                      sidePlayPcnt +
                      fwdPlayPcnt +
                      backPassPcnt +
                      sidePassPcnt +
                      fwdPassPcnt +
                      noPassPlayPcnt +
                      passPlayPcnt +
                      playsPcnt +
                      Plays1_3PassesPcnt +
                      Plays4_6PassesPcnt +
                      Plays7PlusPassesPcnt +
                      playsAttTurnPcnt ,
                      data = trainset, 
                      importance=TRUE, ntree=2000, #xtest=testset[, !(colnames(testset) %in% c("ID", 'default'))],
                      keep.forest = T)
footy_match_game$probability = predict(tx.rf, newdata = footy_match_game, type = "response")
footy_match_game$prob  = predict(tx.rf, newdata = footy_match_game, type = "prob")
footy_match_game$predicted = ifelse(footy_match_game$probability < 0.5, 0, 1)

testset$probability = predict(tx.rf, newdata = testset, type = "response")
testset$prob  = predict(tx.rf, newdata = testset, type = "prob")

table(testset$default, testset$probability)

trainset[is.na(trainset), ]

new_DF <- trainset[rowSums(is.na(trainset)) > 0,]
new_df1 <- FootyGames[is.na(FootyGames$isSidePass) > 0,]
footy_match_game <- footy_match_play %>%
  group_by(competition, gameID) %>%
  mutate(gameDate= min(gameDate),
         gameDuration = sum(possessionDuration),
         gameAttackDuration = sum(AttackDuration),
         gameMidfieldDuration = sum(MidfieldDuration),
         gameDefenceDuration = sum(DefenceDuration),
         goalsUs = sum(ifelse(playByUs == "true", goals, 0)),
         goalsThem = sum(ifelse(playByUs == "false", goals, 0)),
         ppmUs = sum(ifelse(playByUs == "true", totalPasses, 0))/
                 sum(ifelse(playByUs == "true", possessionDuration/60, 0)),
         ppmThem = sum(ifelse(playByUs == "false", totalPasses, 0))/
           sum(ifelse(playByUs == "false", possessionDuration/60, 0)),
         ppmUsAtt = sum(ifelse(playByUs == "true" & Area == "Attack", totalPasses, 0))/
           sum(ifelse(playByUs == "true" & Area == "Attack", possessionDuration/60, 0)),
         ppmThemAtt = sum(ifelse(playByUs == "false" & Area == "Attack", totalPasses, 0))/
           sum(ifelse(playByUs == "false" & Area == "Attack", possessionDuration/60, 0)),
         ppmUsMid = sum(ifelse(playByUs == "true" & Area == "Midfield", totalPasses, 0))/
           sum(ifelse(playByUs == "true" & Area == "Midfield", possessionDuration/60, 0)),
         ppmThemMid = sum(ifelse(playByUs == "false" & Area == "Midfield", totalPasses, 0))/
           sum(ifelse(playByUs == "false" & Area == "Midfield", possessionDuration/60, 0)),
         ppmUsDef = sum(ifelse(playByUs == "true" & Area == "Defence", totalPasses, 0))/
           sum(ifelse(playByUs == "true" & Area == "Defence", possessionDuration/60, 0)),
         ppmThemDef = sum(ifelse(playByUs == "false" & Area == "Defence", totalPasses, 0))/
           sum(ifelse(playByUs == "false" & Area == "Defence", possessionDuration/60, 0)),
         playsUs  = sum(ifelse(playByUs == "true", 1, 0)),
         playsThem = sum(ifelse(playByUs == "false", 1, 0)),  
         playsUsAtt = sum(ifelse(playByUs == "true" & Area != "Attack" &
                                   isAttackIncursion == 1, 1, 0)),
         playsThemAtt = sum(ifelse(playByUs == "false" & Area != "Attack" &
                                   isAttackIncursion == 1, 1, 0)),
         playsThemPen = sum(ifelse(playByUs == "false" & firstPenalyIncursion == 0 &
                                     isPenaltyIncursion == 1, 1, 0)),  
         playsUsPen = sum(ifelse(playByUs == "true" & firstPenalyIncursion == 0 &
                                   isPenaltyIncursion == 1, 1, 0)),
         playsThem17 = sum(ifelse(playByUs == "false" & firstZone != 17 &
                              isZone17Incursion == 1, 1, 0)),  
         playsUs17 = sum(ifelse(playByUs == "true" & firstZone != 17 &
                                    isZone17Incursion == 1, 1, 0)),
         playsThem14 = sum(ifelse(playByUs == "false" & firstZone != 14 &
                                    isZone14Incursion == 1, 1, 0)),  
         playsUs14 = sum(ifelse(playByUs == "true" & firstZone != 14 &
                                  isZone14Incursion == 1, 1, 0)),
         playsUsAttTurn = sum(ifelse(playByUs == "true" & usPhase == "BPO > BP" &
                                    Area == "Attack" & firstEvent == "first touch", 1, 0)),
         playsThemAttTurn = sum(ifelse(playByUs == "false" & usPhase == "BPO > BP" &
                                       Area == "Attack" & firstEvent == "first touch", 1, 0))) %>%
  group_by(competition, gameID, gameDate, playByUs, teamName) %>%
  summarise(passesPerMinute = max(ifelse(playByUs == "true", ppmUs, ppmThem)),
            passesPerMinutePcnt = max(ifelse(playByUs == "true", ppmUs, ppmThem))/
              max(ppmUs + ppmThem),
            passesPerMinutePcntDiff = passesPerMinutePcnt - 0.5,
            passesPerMinuteAtt = max(ifelse(playByUs == "true", ppmUsAtt, ppmThemAtt)),
            passesPerMinuteAttPcnt = max(ifelse(playByUs == "true", ppmUsAtt, ppmThemAtt))/
              max(ppmUsAtt + ppmThemAtt),
            passesPerMinuteAttPcntDiff = passesPerMinuteAttPcnt - 0.5,
            passesPerMinuteMid = max(ifelse(playByUs == "true", ppmUsMid, ppmThemMid)),
            passesPerMinuteMidPcnt = max(ifelse(playByUs == "true", ppmUsMid, ppmThemMid))/
              max(ppmUsMid + ppmThemMid),
            passesPerMinuteMidPcntDiff = passesPerMinuteMidPcnt - 0.5,
            passesPerMinuteDef = max(ifelse(playByUs == "true", ppmUsDef, ppmThemDef)),
            passesPerMinuteDefPcnt = max(ifelse(playByUs == "true", ppmUsDef, ppmThemDef))/
              max(ppmUsDef + ppmThemDef),
            passesPerMinuteDefPcntDiff = passesPerMinuteDefPcnt - 0.5,
            result = max(case_when((playByUs == "true" & goalsUs > goalsThem) |
                                  (playByUs == "false" & goalsUs < goalsThem) ~ "Win",
                                  goalsUs == goalsThem ~ "Draw",
                                  TRUE ~ "Loss")),
            possessionTime = sum(possessionDuration),
            possessionPcnt = sum(possessionDuration)/ 
                             max(gameDuration),
            Passes = sum(totalPasses),
            Plays = n(),
            PlaysPassess = sum(ifelse(totalPasses > 0, 1, 0)),
            Plays1_3Passes = sum(ifelse(totalPasses > 0 & totalPasses <= 3, 1, 0))/
                             sum(ifelse(totalPasses > 0, 1, 0)),
            Plays4_6Passes = sum(ifelse(totalPasses > 3 & totalPasses <= 6, 1, 0))/
              sum(ifelse(totalPasses > 0, 1, 0)),
            Plays7PlusPasses = sum(ifelse(totalPasses > 6, 1, 0))/
              sum(ifelse(totalPasses > 0, 1, 0)),
            PlaysNoPasses = sum(ifelse(totalPasses == 0, 1, 0))/
              sum(totalPasses),
            playsOpportunityPcnt = max(ifelse(playByUs == "true", playsUs, playsThem))/
              max(playsUs + playsThem),
            playsOpportunityPcntDiff = playsOpportunityPcnt - 0.5,
            playIntoAttack = sum(ifelse(Area != "Attack" & isAttackIncursion == 1, 1, 0)),
            playIntoPenalty = sum(ifelse(firstPenalyIncursion == 0 & isPenaltyIncursion == 1, 1, 0)),
            playIntoAttackPcnt = playIntoAttack / PlaysPassess,
            playIntoAttackDiff = max(ifelse(playByUs == "true", playsUsAtt, playsThemAtt))/
              max(playsUsAtt + playsThemAtt),
            playIntoPenaltyDiff = ifelse(max(playsUsPen + playsThemPen) == 0, 0, max(ifelse(playByUs == "true", playsUsPen, playsThemPen))/
              max(playsUsPen + playsThemPen)),
         #   playsUs17 = max(playsThem14),
          #  playsThem17 = max(playsThem17),
            playInto17Diff = ifelse(max(playsUs17 + playsThem17) == 0, 0, max(ifelse(playByUs == "true", playsUs17, playsThem17))/
                                           max(playsUs17 + playsThem17)),
            playInto14Diff = ifelse(max(playsUs14 + playsThem14) == 0, 0, max(ifelse(playByUs == "true", playsUs14, playsThem14))/
                                           max(playsUs14 + playsThem14)),
            playAttTurnDiff = ifelse(max(playsUsAttTurn + playsThemAttTurn) == 0, 0, max(ifelse(playByUs == "true", playsUsAttTurn, playsThemAttTurn))/
                                         max(playsUsAttTurn + playsThemAttTurn))
            )

footy_match_game <- footy_match_game[!footy_match_game$competition == "2017 Mens Open", ]

# Run RandomForest and remove ID field as this is NOT a predictor
footy_match_game$result <- as.factor(footy_match_game$result)
footy_match_game$default = as.factor(as.numeric(ifelse(footy_match_game$result == "Loss", 0, ifelse(footy_match_game$result == "Win", 2, 1))))
footy_match_game_test <- footy_match_game %>% filter(playByUs == "true")

unique(footy_match_game$competition)
trainset = footy_match_game[!footy_match_game$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                 "2019 NSFA 1 Boys Under 13", 
                                                                 "2019 World Cup 2019 Mens Open",             
                                                                "2019 International Friendly Mens Open"   ,  
                                                                "2019 Boys Under 14"                ,        
                                                                "2019 Icc Football Mens Open"
                                                                ), ]

library(DMwR)
library(caret)
# Since positive Target is small, use SMOTE to rebalance the class in Training Set
trainset$competition <- as.factor(trainset$competition)
trainset$gameID <- as.factor(trainset$gameID)
trainset$gameDate <- as.factor(trainset$gameDate)
trainset$playByUs <- as.factor(trainset$playByUs)
trainset$teamName <- as.factor(trainset$teamName)
trainset$prob <- NULL

?SMOTE
smote_train <- SMOTE(default ~ ., data  = as.data.frame(trainset),
                     perc.over = 400, perc.under = 200
)
table(trainset$default)
table(smote_train$default)
# Perform Logistic regression, removing ID and correlated fields and other fields with a high p-value
tx.glm = glm(formula = Target ~ . -ID -total_paid_services -total_services -sched_serv_warr -non_sched_serv_warr
             -num_dealers_visited,
             data = smote_train,
             family = "binomial")


testset = footy_match_game[footy_match_game$competition %in% c("2019 NSFA 1 Girls Under 16", "2019 NSFA 1 Boys Under 13", "2019 World Cup 2019 Mens Open",             
                                                               "2019 International Friendly Mens Open"   ,  
                                                               "2019 Boys Under 14"                ,        
                                                               "2019 Icc Football Mens Open"
                                                            ), ]
tx.rf <- randomForest(default ~ playInto17Diff + playInto14Diff +
                        playAttTurnDiff +
                        playIntoAttackDiff + 
                        passesPerMinutePcntDiff +
                        playIntoPenaltyDiff +
                        passesPerMinuteMidPcntDiff +
                        passesPerMinuteAttPcntDiff + 
                        passesPerMinuteDefPcntDiff + 
                        possessionPcnt  + 
                        Plays1_3Passes +
                        Plays4_6Passes +
                        Plays7PlusPasses +
                        playsOpportunityPcnt ,
                      data = trainset, 
                      importance=TRUE, ntree=2000, #xtest=testset[, !(colnames(testset) %in% c("ID", 'default'))],
                      keep.forest = T)
footy_match_game$probability = predict(tx.rf, newdata = footy_match_game, type = "response")
footy_match_game$prob  = predict(tx.rf, newdata = footy_match_game, type = "prob")
footy_match_game$predicted = ifelse(footy_match_game$probability < 0.5, 0, 1)

testset$probability = predict(tx.rf, newdata = testset, type = "response")
testset$prob  = predict(tx.rf, newdata = testset, type = "prob")

table(testset$default, testset$probability)

summary(tx.rf)
varImpPlot(tx.rf)
# Plot Partial Dependency plots
partialPlot(tx.rf, testset, x.var = "playInto17Diff", which.class = "1")
partialPlot(tx.rf, testset, x.var = "playInto14Diff", which.class = "1")
partialPlot(tx.rf, testset, x.var = "LIMIT_BAL", which.class = "1")
partialPlot(tx.rf, testset, x.var = "AMT_PC1", which.class = "1")

install.packages("gbm")
library(gbm)

# defining some parameters
gbm_depth = 10 #maximum nodes per tree
gbm_n.min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 8 #number of cores
gbm_cv.folds=0 #number of cross-validation folds to perform
num_trees = 10000

smote_train$result <- NULL
smote_train$competition <- NULL
smote_train$gameID <- NULL
smote_train$gameDate <- NULL
smote_train$playByUs <- NULL
smote_train$teamName <- NULL
table(smote_train$default)

?gbm
  gbm_clf = gbm(default~playInto17Diff + playInto14Diff +
                  playAttTurnDiff +
                  playIntoAttackDiff + 
                  passesPerMinutePcntDiff +
                  playIntoPenaltyDiff +
                  passesPerMinuteMidPcntDiff +
                  passesPerMinuteAttPcntDiff + 
                  passesPerMinuteDefPcntDiff + 
                  possessionPcnt  + 
                  Plays1_3Passes +
                  Plays4_6Passes +
                  Plays7PlusPasses +
                  playsOpportunityPcnt,
                data=smote_train,
                #continuous response
                n.trees=num_trees, #the number of GBM interaction
                interaction.depth= gbm_depth,
                n.minobsinnode = gbm_n.min, 
                shrinkage=gbm_shrinkage, 
                train.fraction = 0.5,
                cv.folds=gbm_cv.folds,
                verbose = FALSE, #print the preliminary output
                n.cores = cores_num
  )
  summary(gbm_clf)
  # Get AUC and Threshold for determining Prediction
  testset$probability_gbm = predict(gbm_clf, testset, type = "response")
  trainset$prob_gbm = predict(gbm_clf, trainset, type = "link")
  Measures = model_evaluation(trainset, testset, gbm_clf, target = "default", plot_results=FALSE)
  
  predBST = predict(gbm_clf,n.trees=10000, newdata=testset,type='response')
  
  p.predBST <- apply(predBST, 1, which.max) - 1
  testset$bst <- p.predBST
  predRF = predict(tx.rf, newdata = testset, type = "prob")
  
  
  table(testset$default, testset$probability)
  summary(gbm_clf, ylab = "Variable", main = "Variable Relative Importance")
  #OR just as a table
  summary(gbm_clf)
  
names(tx.rf)
comp <- "2019 NSFA 1 Boys Under 13"
team <- "NFC Red"

footy_match_play <- footy_match_play %>%
  filter(!competition %in%
           c("2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") )

footy_match_play$teamName = ifelse(footy_match_play$playByUs == "true", footy_match_play$ourName, footy_match_play$theirName)




FootyGames %>% group_by(gameID, playNumber) %>%
  mutate(numSequences = n()) %>%
  filter(numSequences != sequenceNumber) %>%
  group_by(gameID, playNumber, player) %>%
  summarise(byUs = first(byUs),
            numSequences = max(numSequences),
            sectorStart = first(case_when(lengthFraction <= 0.33333 ~ 1,
                               lengthFraction >= 0.66667 ~ 3,
                               TRUE ~ 2)),
            playerDuration = sum(ifelse(adjEventName %in%
                            c("dribble", "first touch"), duration, 0))) %>%
  ungroup %>% filter(numSequences > 2) %>%
  group_by(gameID, byUs, sectorStart) %>%
  summarise(aveDuration = mean(playerDuration))

footy_match %>% group_by(byUs, adjEventName) %>%
  filter(adjPass == 1) %>%
  summarise(n())

footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(playByUs = first(byUs),
            usPhase = first(usPhase),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            isAttackIncursion = max(attackIncursion),
            isPenaltyIncursion = max(penaltyIncursion),
            playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
            totalPasses = sum(adjPass),
            playDuration = sum(duration),
            possessionDuration = sum(ifelse(pseudoEvent == FALSE &
                                              byUs == playByUs &
                                              eventName != "keeper", duration, 0)),
            firstDirection = first(direction),
            firstLength = first(lengthMetres),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)) )

fm1 <- footy_match_play %>% 

ggplot(footy_match_play %>% filter(#firstEvent == "throw in" &#playByUs == "true" & #theirName == "Lindfield A" &
                                  competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
#"2019 International Friendly Mens Open",
#"2019 Icc Football Mens Open")# &
                                    "2019 NSFA 1 Boys Under 13" &
                                    totalPasses > 0
) %>%
              mutate(teamName = ifelse(playByUs == "true", ourName, theirName)) %>%
              group_by(teamName) %>% mutate(tp = n(),
                                            totPass1 = ifelse(totalPasses < 7, totalPasses, 7)) %>%
            group_by(teamName, totPass1) %>%
              summarise(perc = n() / max(tp) ),
       aes(x=totPass1, y= perc, fill=teamName) ) +
 geom_bar(stat = "identity", position = "dodge") #+
# theme(legend.position="none")

fmp1 <- footy_match_play %>% filter(totalPasses > 0) %>%
  group_by(competition) %>%
  mutate(sum_tp = n(),
         totPass1 = ifelse(totalPasses < 7, totalPasses, 7)) %>%
  group_by(competition, totPass1) %>%
  summarise(stp = n(),
            smp = max(sum_tp))
fmp2 <- footy_match_play %>% filter(isAttackIncursion == 0 | goals > 0) %>%
  group_by(competition) %>%
  summarise(sum_tp = n(),
         goals = sum(goals)) 

fmp3 <- footy_match_play %>% filter(goals > 0) %>%
  group_by(competition) %>%
  mutate(totalGoals = sum(goals)) %>%
  group_by(competition, totalPasses) %>%
  summarise(sum_tp = n(),
            goals = max(totalGoals)) 


ggplot(fmp1 %>% filter(totPass1 <= 3) %>%
         group_by(competition) %>%
         summarise(stp = sum(stp),
                   smp = max(smp)),
       aes(x=competition, y= stp / smp) ) +
  geom_bar(stat = "identity")  + 
  geom_text(aes(label=scales::percent(stp / smp)), position = position_stack(vjust = .5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# theme(legend.position="none")
ggplot(fmp3 %>% filter(competitio) %>%
         group_by(competition) %>%
         summarise(stp = sum(stp),
                   smp = max(smp)),
       aes(x=competition, y= stp / smp) ) +
  geom_bar(stat = "identity")  + 
  geom_text(aes(label=scales::percent(stp / smp)), position = position_stack(vjust = .5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# theme(legend.position="none")


library(scales)
ggplot(fmp2 ,
         aes(x=competition, y= goals / sum_tp) ) +
  geom_bar(stat = "identity")  + 
  geom_text(aes(label=scales::percent(goals / sum_tp)), position = position_stack(vjust = .5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# theme(legend.position="none")

table(ifelse(footy_match_play[footy_match_play$competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
                                #"2019 International Friendly Mens Open",
                                #"2019 Icc Football Mens Open")# &
                                "2019 NSFA 1 Boys Under 13" &
                                footy_match_play$totalPasses > 0, ]$totalPasses < 7, footy_match_play[footy_match_play$competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
                                                                                                        #"2019 International Friendly Mens Open",
                                                                                                        #"2019 Icc Football Mens Open")# &
                                                                                                        "2019 NSFA 1 Boys Under 13" &
                                                                                                        footy_match_play$totalPasses > 0, ]$totalPasses, 7),
      footy_match_play[footy_match_play$competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
                         #"2019 International Friendly Mens Open",
                         #"2019 Icc Football Mens Open")# &
                         "2019 NSFA 1 Boys Under 13" &
                         footy_match_play$totalPasses > 0, ]$totalPasses)
sum(footy_match_play[footy_match_play$competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
    #"2019 International Friendly Mens Open",
    #"2019 Icc Football Mens Open")# &
    "2019 NSFA 1 Boys Under 13" &
      footy_match_play$totalPasses > 0, ]$totalPasses)
) )
ggplot(footy_match_play %>% filter(#firstEvent == "throw in" &#playByUs == "true" & #theirName == "Lindfield A" &
  competition ==#%in% c("2019 World Cup 2019 Mens Open" ,
    #"2019 International Friendly Mens Open",
    #"2019 Icc Football Mens Open")# &
    "2019 NSFA 1 Boys Under 13" &
    totalPasses > 0
) %>% mutate(sum_tp = sum(totalPasses)) %>%
   mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         totPass1 = ifelse(totalPasses < 7, totalPasses, 7)) %>%
  group_by(totPass1) %>%
  summarise(perc = sum(totalPasses) / max(sum_tp) ),
aes(x=totPass1, y= perc) ) +
  geom_bar(stat = "identity", position = "dodge") #+
# theme(legend.position="none")

sum(total_duration$totalDuration)
sum(footy_match_play$possessionDuration)
total_duration <- FootyGames %>% filter(pseudoEvent == FALSE & playNumber > 0) %>%
  group_by(gameID, byUs) %>% summarise(totalDuration = sum(ifelse(eventName == "keeper", 0, duration)))

footySum <- footy_match_play %>% filter(playNumber > 0) %>% 
  group_by(gameID, competition, ourName, theirName, playByUs) %>%
  summarise(teamName = first(ifelse(playByUs == "true", ourName, theirName)),
            numPlays = sum(ifelse(totalPasses > 1, 1, 0)),
            onePlays = sum(ifelse(totalPasses == 1, 1, 0)),
            nonePlays = sum(ifelse(totalPasses == 0, 1, 0)),
            totalPlays = n(),
            avePasses = sum(totalPasses/numPlays),
            maxPasses = max(totalPasses),
            sumPasses = sum(totalPasses),
            goals = sum(goals),
            shots = sum(shots)) %>% 
  left_join(total_duration, by=c("playByUs" =  "byUs", "gameID" = "gameID")) %>%
  mutate(playPerMin = sumPasses/(totalDuration/60))

footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(playByUs = first(byUs),
            usPhase = first(usPhase),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            Area = first(case_when(lengthFraction <= 0.333333 ~ "Defence",
                                   lengthFraction >= 0.666667 ~ "Attack",
                                   TRUE ~ "Midfield") ),
            firstEventArea = paste(firstEvent, "-", Area),
            firstPenalyIncursion = first(penaltyIncursion),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            isAttackIncursion = max(attackIncursion),
            isPenaltyIncursion = max(penaltyIncursion),
            playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
            totalPasses = sum(adjPass),
            possessionDuration = sum(ifelse(pseudoEvent == FALSE &
                                              byUs == playByUs &
                                              eventName != "keeper", duration, 0)),
            playDuration = sum(ifelse(pseudoEvent == FALSE, duration, 0)),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)) )

sum(footy_match_play$possessionDuration)
table(total_duration$gameID, total_duration$totalDuration)
footySum %>% group_by(competition, teamName) %>%
  summarise(sum(sumPasses), sum(totalDuration))

comp <- "2019 NSFA 1 Boys Under 13"
team <- "NFC Red"
footy_match_play$teamName = ifelse(footy_match_play$playByUs == "true", footy_match_play$ourName, footy_match_play$theirName)
footy_match_play$orderCol <- case_when(footy_match_play$competition == comp & footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)

 # All teams 3 passes or less percentage
ggplot(footy_match_play %>% 
         ungroup %>% group_by(teamName, orderCol) %>%
         summarise(sum(totalPasses)/ sum(possessionDuration/60)) ,
       aes(x=reorder(teamName, orderCol, FUN=max), y= ppm, fill=teamName) ) +
  geom_bar( stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust=0.5)) + 
  labs(x = "Team", y = "Passes", 
       title = "Passes Per Minute") 
```


footy_match %>% filter(adjPass == 1 & lengthChangeMetres < -8) %>%
       group_by(byUs) %>% summarise(n())

footy_match %>% filter(adjEventName %in% c("dribble", "first touch") & adjPass == 1 & 
                         lengthChangeMetres > 15) %>%
  group_by(byUs) %>% summarise(n())

errors <- footy_match %>% filter(pseudoEvent == FALSE & eventName == "dribble") %>%
  group_by(competition, ourName, theirName, gameID) %>%
  summarise(adjustments = sum(ifelse(eventName == "dribble" & adjEventName == "first touch", 1, 0)),
            numDribble = n(),
            chgPcnt = adjustments/numDribble,
            startTime = min(time))

nrow(footy_match %>% filter(pseudoEvent == FALSE & eventName == "dribble" &
                              adjEventName == "first touch")) /
  nrow(footy_match %>% filter(pseudoEvent == FALSE & eventName == "dribble"))
                              )
footy_throwin <- footy_match_play %>% filter(firstEvent == "corner kick")
table(footy_throwin$byUs, footy_throwin$numSequences)

table(footy_match[footy_match$pseudoEvent == FALSE, ]$byUs, footy_match[footy_match$pseudoEvent == FALSE, ]$adjEventName, footy_match[footy_match$pseudoEvent == FALSE, ]$adjPass)
table(footy_match$byUs, footy_match$adjPass)
table(footy_match$byUs, ifelse(footy_match$pass == "true" &
                                 footy_match$pseudoEvent == FALSE, 1, 0))

nrow(footy_match %>% filter(adjPass == 0 & pass == "true" & pseudoEvent == FALSE))
nrow(footy_match %>% filter(adjPass == 1 & pass == "false" & pseudoEvent == FALSE))
write.csv(footy_match, "data_out2.csv")

unique(FG1$playByUs)
FG1 <- FootyGames %>% filter(playByUs %in% c("true", "false") & isGoal == 1)
pppxy <- ppp(FG1$lengthMetres, FG1$widthMetres, at="points",
             c(0,110), c(0,75), checkdup=FALSE, marks=factor(FG1$playByUs))
plot(density(pppxy, diggle=TRUE), multiplot=FALSE)
plot(Smooth.ppp(pppxy), multiplot=FALSE)

FG1 <- FootyGames %>% filter(playByUs %in% c("false") & isGoal == 1 &
                               sequenceNumber == 1 & theirName == "North Sydney"
                             )

ggplot(FG1, aes(x = lengthMetres, y = widthMetres)) + 
  stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE,
                 lims=c(0, 0, 110, 75)) +
  geom_point(size = 1, alpha = 0.5) + 
  coord_fixed(xlim=c(0, 110), ylim=c(0, 75))+
  scale_fill_gradient2(low = "white", high = "red")
