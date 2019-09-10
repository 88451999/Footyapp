library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)

setwd("/GitDev/Footyapp/data/") # use here function instead
FootyGames <- data.frame()
#if(file.exists("FootyGamesFull.csv")){
#  FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)
#}

footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)
#footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)
footy <- footy %>% filter(competition != "2019 NSFA 1 Girls Under 16")
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
        
      } else if (footy_match[i-1, ]$adjEventName == "change ends" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "kick off"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } 
        # Include action before sideline out/corner/goal kick as part of same play
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
        footy_match[i, ]$player = case_when(footy_match[i-1, ]$adjEventName %in% c("first touch", "dribble", "clearance") &
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
        
        # Turnover and then straight back as turnover
        if (footy_match[i, ]$byUs != footy_match[i+1, ]$byUs &
            footy_match[i, ]$distance >= 10 & footy_match[i, ]$lengthChangeMetres > 3 &
              footy_match[i, ]$lengthFraction <= 0.33) {
            footy_match[i, ]$adjEventName = "clearance"
        }
        
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

FootyGames$widthMetres = 75 - FootyGames$widthMetres 
FootyGames$widthFraction = 1 - FootyGames$widthFraction 

FootyGames$zone <- (floor(ifelse(FootyGames$widthFraction== 1, 2, 
                                 FootyGames$widthFraction*3))+1)+(3*floor(ifelse(FootyGames$lengthFraction>= 1, 5, 
                                                                                 FootyGames$lengthFraction*6)))

FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)

FootyGames$kickoff_date = as.POSIXct(substr(FootyGames$time, 1, 19), format='%Y-%m-%d %H:%M:%S')

FootyGames1 <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  mutate(maxSequenceNumber = max(sequenceNumber),
         isAttackIncursion = max(attackIncursion),
         isGoal = ifelse(last(adjEventName == "goal"), 1, 0),
         numberPasses = sum(adjPass),
         playByUs = first(byUs),
         firstEvent = first(adjEventName),
         lastEvent = last(adjEventName),
         matchName = paste(format(ymd(substr(time, 1, 10)), "%d/%m/%y"), "-", theirName),
         backPassTurnover = ifelse(isBackPass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         fwdPassTurnover = ifelse(isFwdPass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         sidePassTurnover = ifelse(isSidePass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         rightAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==10 & lead(zone) %in% c(13, 16) & byUs == lead(byUs), 1, 0)),
         middleAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==11 & lead(zone)%in% c(14, 17) & byUs == lead(byUs), 1, 0)),
         leftAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==12 & lead(zone)%in% c(15, 18) & byUs == lead(byUs), 1, 0)),
         isZone14Incursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=14 & lead(zone) == 14 & byUs == lead(byUs), 1, 0)),
         isZone17Incursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=17 & lead(zone) == 17 & byUs == lead(byUs), 1, 0)),
         isZone11PassIncursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=11 & lead(zone) == 11 & adjPass == 1 & byUs == lead(byUs), 1, 0)),
         isBackKeeperPass = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=2 & lead(zone) == 2 & adjPass == 1 & byUs == lead(byUs), 1, 0)),
         playArea = case_when(lengthFraction <= 0.333333 ~ "Defence",
                              lengthFraction >= 0.666667 ~ "Attack",
                              TRUE ~ "Midfield"),
         Movement = case_when(playNumber == 0 ~ "",
                              sequenceNumber == maxSequenceNumber ~ "",
                              adjPass == 1 & playArea == 'Defence' &
                                lead(playArea) == 'Midfield' ~ "D-M",
                              adjPass == 1 & playArea == 'Midfield' &
                                lead(playArea) == 'Defence' ~ "M-D",
                              adjPass == 1 & playArea == 'Midfield' &
                                lead(playArea) == 'Attack' ~ "M-A",
                              adjPass == 1 & playArea == 'Attack' &
                                lead(playArea) == 'Midfield' ~ "A-M",
                              TRUE ~ ""),
  #       Movement = case_when(playNumber == 0 ~ "",
  #                            sequenceNumber == maxSequenceNumber ~ "",
  #                            adjPass == 1 & lengthFraction < 0.333333 &
  #                              lead(lengthFraction)  > 0.333333 & 
  #                              lead(lengthFraction) < 0.666667 ~ "D-M",
  #                            adjPass == 1 & lengthFraction > 0.333333 & lengthFraction < 0.666667 &
  #                              lead(lengthFraction) < 0.333333 ~ "M-D",
  #                            adjPass == 1 & lengthFraction > 0.333333 & lengthFraction < 0.666667 &
  #                              lead(lengthFraction) >= 0.66666667 ~ "M-A",
  #                            adjPass == 1 & lengthFraction >= 0.66666667 &
  #                              lead(lengthFraction) > 0.333333 & 
  #                              lead(lengthFraction) < 0.666667 ~ "A-M",
  #                            TRUE ~ "")
  )

footy_match_play <- FootyGames1 %>% filter(playNumber != 0) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(gameDate = first(kickoff_date),
            playByUs = first(playByUs),
            matchName = first(matchName),
            teamName = first(ifelse(playByUs == "true", ourName, theirName)),
            usPhase = first(usPhase),
            half = first(half),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            Area = first(playArea),
            lastArea = last(playArea),
            firstEventArea = paste(firstEvent, "-", Area),
            firstPenalyIncursion = first(penaltyIncursion),
            firstZone = first(zone),
            lastZone = last(zone),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            distance = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                    !lead(adjEventName) %in% c("goal kick", "sideline out"), 
                                  distanceToNextEventMetres, 0)),
            attackDistance = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                          !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                          playArea == 'Attack', 
                                        distanceToNextEventMetres, 0)),
            midfieldDistance = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                          !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                          playArea == 'Midfield', 
                                        distanceToNextEventMetres, 0)),
            defenceDistance = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                            !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                            playArea == 'Defence', 
                                          distanceToNextEventMetres, 0)),
            isAttackIncursion = max(attackIncursion),
            isGoal = max(isGoal),
            isPenaltyIncursion = max(penaltyIncursion),
            isZone17Incursion = max(isZone17Incursion, na.rm=TRUE),
            isZone14Incursion = ifelse(isZone17Incursion == 1, 0, max(isZone14Incursion, na.rm=TRUE)),
            isZone11PassIncursion = max(isZone11PassIncursion, na.rm=TRUE),
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
            AttackDuration = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                                 !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                                 playArea == 'Attack', duration, 0)),
            MidfieldDuration = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                            !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                            playArea == 'Midfield', duration, 0)),
            DefenceDuration = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                           !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                           playArea == 'Defence', duration, 0)),
            AttackHalfDuration = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                              !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                              lengthMetres >= 55, duration, 0)),
            DefenceHalfDuration = sum(ifelse(lead(byUs) == byUs & playByUs == byUs &
                                               !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                               lengthMetres < 55, duration, 0)),
            playDuration = sum(duration),
            attackPasses = sum(ifelse(playArea == "Attack", adjPass, 0)),
            midfieldPasses = sum(ifelse(playArea == "Midfield", adjPass, 0)),
            defencePasses = sum(ifelse(playArea == "Defence", adjPass, 0)),
            crossfieldPlay = ifelse(max(widthMetres) - min(widthMetres) > 50 &
                                      Area == "Defence", 1, 0),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)),
            backPassTurnover = max(backPassTurnover),
            fwdPassTurnover = max(fwdPassTurnover),
            sidePassTurnover = max(sidePassTurnover),
            rightAttack = max(rightAttack),
            middleAttack = max(middleAttack),
            leftAttack = max(leftAttack),
            defenceOut = ifelse(lastEvent == "sideline out defence", 1, 0),
            MAPlay = max(ifelse(Movement == "M-A", 1, 0)),
            AMPlay = max(ifelse(Movement == "A-M", 1, 0)),
            MDPlay = max(ifelse(Movement == "M-D", 1, 0)),
            DMPlay = max(ifelse(Movement == "D-M", 1, 0)),
            backKeeperPasses = sum(isBackKeeperPass),
            clearances = sum(ifelse(adjEventName == "clearance", 1, 0))
            )
footy_match_play$orderCol <- case_when(footy_match_play$teamName == team ~ 0,
                                       footy_match_play$competition %in% c("2019 NSFA 1 Boys Under 13",
                                                                           "2019 NSFA 1 Girls Under 16", "2019 Boys Under 14") ~ 1,
                                       footy_match_play$competition=="2019 International Friendly Mens Open" ~ 2,
                                       footy_match_play$competition=="2019 Icc Football Mens Open" ~ 3,
                                       TRUE ~ 4)
write.csv(FootyGames1, "FootyGames1Full.csv", row.names = FALSE)
write.csv(footy_match_play, "FootyMatchPlayFull.csv", row.names = FALSE)
write.csv(FootyGames1 %>%
            filter(competition == "2019 NSFA 1 Boys Under 13"), "FootyGames113.csv", row.names = FALSE)
write.csv(footy_match_play %>%
            filter(competition == "2019 NSFA 1 Boys Under 13"), "FootyMatchPlay13.csv", row.names = FALSE)


is.na(footy_match_play$crossfieldPlay)
footy_match_pca <- footy_match_play %>%
  filter(competition %in% c(#"2019 NSFA 1 Girls Under 16", 
    "2019 NSFA 1 Boys Under 13", 
    "2019 World Cup 2019 Mens Open",             
    "2019 International Friendly Mens Open"   ,  
    #  "2019 Boys Under 14"                ,        
    "2019 Icc Football Mens Open",
    "2019 Mens Open"
  )
  ) %>%
  group_by(competition, gameID, 
           teamName, playByUs) %>%
  summarise(gameDate= min(gameDate),
            theirName = max(theirName),
            gameDuration = sum(possessionDuration),
            gameAttackDurationPcnt = sum(AttackDuration) / gameDuration,
            gameMidfieldDurationPcnt = sum(MidfieldDuration) / gameDuration,
            gameDefenceDurationPcnt = sum(DefenceDuration) / gameDuration,
            gameDistance = sum(distance),
            plays = n(),
            goals = sum(goals),
            shots = sum(shots),
            ppm = sum(totalPasses)/sum(possessionDuration/60),
            aveVelocity = gameDistance/(gameDuration/60),
            attackPlayPcnt = sum(ifelse(Area == "Attack", 1, 0)) / plays,
            midfieldPlayPcnt = sum(ifelse(Area == "Midfield", 1, 0)) / plays,
            defencePlayPcnt = sum(ifelse(Area == "Defence", 1, 0)) / plays,
            penaltyPlayPcnt = sum(ifelse(isPenaltyIncursion == 1, 1, 0)) / plays,
            zone14PlayPcnt = sum(ifelse(isZone14Incursion == 1, 1, 0)) / plays,
            zone17PlayPcnt = sum(ifelse(isZone17Incursion == 1, 1, 0)) / plays,
            zone11PlayPcnt = sum(ifelse(isZone11PassIncursion == 1, 1, 0)) / plays,
            backPassPcnt = sum(totalBackPasses) / sum(totalPasses),
            sidePassPcnt = sum(totalSidePasses) / sum(totalPasses),
            fwdPassPcnt = sum(totalFwdPasses) / sum(totalPasses),
            Pass10mPcnt = sum(total10mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass10_20mPcnt = sum(total10_20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass20mPcnt = sum(total20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            noPassPlayPcnt = sum(ifelse(totalPasses == 0, 1, 0)) / plays,
            passPlayPcnt = sum(ifelse(totalPasses > 1, 1, 0)) / plays,
            Plays1PassesPcnt = sum(ifelse(totalPasses > 0 & totalPasses <= 1, 1, 0)) / plays,
            Plays2_4PassesPcnt = sum(ifelse(totalPasses > 1 & totalPasses <= 5, 1, 0)) / plays,
            Plays5_7PassesPcnt = sum(ifelse(totalPasses > 5 & totalPasses <= 7, 1, 0)) / plays,
            Plays8PlusPassesPcnt = sum(ifelse(totalPasses > 7, 1, 0)) / plays,
            crossfieldPlayPcnt = sum(crossfieldPlay) / plays,
            backPassTurnoverPcnt = sum(backPassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                                 sidePassTurnover),
            fwdPassTurnoverPcnt = sum(fwdPassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                               sidePassTurnover),
            sidePassTurnoverPcnt = sum(sidePassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                                 sidePassTurnover),
            rightAttackPcnt = sum(rightAttack) / sum(rightAttack + middleAttack +
                                                       leftAttack),
            middleAttackPcnt = sum(middleAttack) / sum(rightAttack + middleAttack +
                                                         leftAttack),
            leftAttackPcnt = sum(leftAttack) / sum(rightAttack + middleAttack +
                                                     leftAttack),
            #    defenceOut = ifelse(lastEvent == "sideline out defence", 1, 0),
            MAPlayPcnt = sum(MAPlay) / sum(MAPlay + AMPlay +
                                             MDPlay + DMPlay),
            AMPlayPcnt = sum(AMPlay) / sum(MAPlay + AMPlay +
                                             MDPlay + DMPlay),
            MDPlayPcnt = sum(MDPlay) / sum(MAPlay + AMPlay +
                                             MDPlay + DMPlay),
            DMPlayPcnt = sum(DMPlay) / sum(MAPlay + AMPlay +
                                             MDPlay + DMPlay),
            useTurnover = sum(ifelse(playPhase == "BPO > BP" &
                                     totalPasses > 1, 1, 0)) / 
                               sum(ifelse(playPhase == "BPO > BP", 1, 0)),
            backKeeperPassesPcnt = sum(backKeeperPasses) / 
              sum(totalPasses),
            clearancePcnt = sum(clearances) / plays,
            playsAttTurn = sum(ifelse(Area == "Attack" & firstEvent == "first touch", 1, 0)),
            matchName = max(matchName)
  ) %>%
  ungroup () %>%
  group_by(competition, gameID) %>%
  mutate(result = case_when(sum(goals) == 0 ~ "Draw",
                            goals/sum(goals) == 0.5 ~ "Draw",
                            goals/sum(goals) < 0.5 ~ "Loss",
                            TRUE ~ "Win")
  )
footy_match_pca <- footy_match_pca %>%
  select(-gameID, everything())

footy_match_pcac <- footy_match_play %>%
  filter(competition %in% c(#"2019 NSFA 1 Girls Under 16", 
    "2019 NSFA 1 Boys Under 13", 
    "2019 World Cup 2019 Mens Open",             
    "2019 International Friendly Mens Open"   ,  
    #  "2019 Boys Under 14"                ,        
    "2019 Icc Football Mens Open",
    "2019 Mens Open"
  )
  ) %>%
  group_by(competition, 
           teamName, playByUs) %>%
  summarise(gameDate= min(gameDate),
            theirName = max(theirName),
            gameDuration = sum(possessionDuration),
            gameAttackDurationPcnt = sum(AttackDuration) / gameDuration,
            gameMidfieldDurationPcnt = sum(MidfieldDuration) / gameDuration,
            gameDefenceDurationPcnt = sum(DefenceDuration) / gameDuration,
            gameDistance = sum(distance),
            plays = n(),
            goals = sum(goals),
            shots = sum(shots),
            ppm = sum(totalPasses)/sum(possessionDuration/60),
            aveVelocity = gameDistance/(gameDuration/60),
            attackPlayPcnt = sum(ifelse(Area == "Attack", 1, 0)) / plays,
            midfieldPlayPcnt = sum(ifelse(Area == "Midfield", 1, 0)) / plays,
            defencePlayPcnt = sum(ifelse(Area == "Defence", 1, 0)) / plays,
            penaltyPlayPcnt = sum(ifelse(isPenaltyIncursion == 1, 1, 0)) / plays,
            zone14PlayPcnt = sum(ifelse(isZone14Incursion == 1, 1, 0)) / plays,
            zone17PlayPcnt = sum(ifelse(isZone17Incursion == 1, 1, 0)) / plays,
            zone11PlayPcnt = sum(ifelse(isZone11PassIncursion == 1, 1, 0)) / plays,
            backPassPcnt = sum(totalBackPasses) / sum(totalPasses),
            sidePassPcnt = sum(totalSidePasses) / sum(totalPasses),
            fwdPassPcnt = sum(totalFwdPasses) / sum(totalPasses),
            Pass10mPcnt = sum(total10mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass10_20mPcnt = sum(total10_20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass20mPcnt = sum(total20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            noPassPlayPcnt = sum(ifelse(totalPasses == 0, 1, 0)) / plays,
            passPlayPcnt = sum(ifelse(totalPasses > 1, 1, 0)) / plays,
            Plays1PassesPcnt = sum(ifelse(totalPasses > 0 & totalPasses <= 1, 1, 0)) / plays,
            Plays2_4PassesPcnt = sum(ifelse(totalPasses > 1 & totalPasses <= 5, 1, 0)) / plays,
            Plays5_7PassesPcnt = sum(ifelse(totalPasses > 5 & totalPasses <= 7, 1, 0)) / plays,
            Plays8PlusPassesPcnt = sum(ifelse(totalPasses > 7, 1, 0)) / plays,
            crossfieldPlayPcnt = sum(crossfieldPlay) / plays,
            backPassTurnoverPcnt = sum(backPassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                             sidePassTurnover),
            fwdPassTurnoverPcnt = sum(fwdPassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                           sidePassTurnover),
            sidePassTurnoverPcnt = sum(sidePassTurnover) / sum(backPassTurnover + fwdPassTurnover +
                                                             sidePassTurnover),
            rightAttackPcnt = sum(rightAttack) / sum(rightAttack + middleAttack +
                                                   leftAttack),
            middleAttackPcnt = sum(middleAttack) / sum(rightAttack + middleAttack +
                                                     leftAttack),
            leftAttackPcnt = sum(leftAttack) / sum(rightAttack + middleAttack +
                                                 leftAttack),
            #    defenceOut = ifelse(lastEvent == "sideline out defence", 1, 0),
            MAPlayPcnt = sum(MAPlay) / sum(MAPlay + AMPlay +
                                         MDPlay + DMPlay),
            AMPlayPcnt = sum(AMPlay) / sum(MAPlay + AMPlay +
                                         MDPlay + DMPlay),
            MDPlayPcnt = sum(MDPlay) / sum(MAPlay + AMPlay +
                                         MDPlay + DMPlay),
            DMPlayPcnt = sum(DMPlay) / sum(MAPlay + AMPlay +
                                         MDPlay + DMPlay),
            useTurnover = sum(ifelse(playPhase == "BPO > BP" &
                                       totalPasses > 1, 1, 0)) / 
              sum(ifelse(playPhase == "BPO > BP", 1, 0)),
            backKeeperPassesPcnt = sum(backKeeperPasses) / 
              sum(totalPasses),
            clearancePcnt = sum(clearances) / plays,
            playsAttTurn = sum(ifelse(Area == "Attack" & firstEvent == "first touch", 1, 0)),
            matchName = max(matchName)
  ) 
                                           