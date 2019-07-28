library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

setwd("/GitDev/Footyapp/data/") # use here function instead
footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)

footy_match <- footy %>% filter(gameID == "-LklLiQLB8ze2uy6wHZr")
footy_match <- footy %>% filter(ourName == " Socceroos")

kickoff = FALSE
prevPlayNumber = 0
footy_match$attackIncursion = 0
footy_match$adjEventName = ""
footy_match$penaltyIncursion = 0
footy_match$adjPass = 0
footy_match$playNumber = 0
footy_match$sequenceNumber = 0
footy_match$usPhase = ""
footy_match$oppositionPhase = ""
footy_match$pseudoEvent = FALSE
footy_match$adjEventName = case_when(footy_match$eventName != "dribble" ~ footy_match$eventName,
                                     footy_match$distanceToNextEventMetres / footy_match$duration > 8 ~ "first touch",
                                     footy_match$distanceToNextEventMetres > 15 ~ "first touch",
                                     TRUE ~ footy_match$eventName)

for (i in 1:nrow(footy_match)) {

   if (!kickoff) {
     if (footy_match[i, ]$adjEventName == "first touch" ) {
       footy_match[i, ]$adjEventName = "kick off"
       kickoff = TRUE
       footy_match[i, ]$playNumber = 1
       footy_match[i, ]$sequenceNumber = 1
       
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
       } else if (footy_match[i-1, ]$adjEventName == "sideline out" &
         footy_match[i, ]$adjEventName == "first touch") {       
         footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
         footy_match[i, ]$playNumber = prevPlayNumber + 1
         footy_match[i, ]$sequenceNumber = 1
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
         footy_match[i, ]$adjEventName = "corner kick"
         if (footy_match[i, ]$byUs == "true") {
           footy_match[i, ]$usPhase = "BP"
           footy_match[i, ]$oppositionPhase = "BPO"
         } else {
           footy_match[i, ]$usPhase = "BPO"
           footy_match[i, ]$oppositionPhase = "BP"
         } 
       } else if (footy_match[i+1, ]$adjEventName == "keeper") {

       } else if (footy_match[i, ]$adjEventName == "keeper") {
         footy_match[i, ]$playNumber = prevPlayNumber 
         footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
         footy_match[i, ]$adjEventName = "keeper catch"
         footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
         footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
         
       } else if (footy_match[i-1, ]$adjEventName == "keeper" &
                  footy_match[i, ]$adjEventName == "first touch") {
         footy_match[i, ]$playNumber = prevPlayNumber + 1
         footy_match[i, ]$sequenceNumber = 1
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
         
       } else if (footy_match[i-1, ]$adjEventName == "goal" &
                  footy_match[i, ]$adjEventName == "first touch") {
         footy_match[i, ]$playNumber = prevPlayNumber + 1
         footy_match[i, ]$sequenceNumber = 1
         footy_match[i, ]$adjEventName = "kick off"
         if (footy_match[i, ]$byUs == "true") {
           footy_match[i, ]$usPhase = "BP"
           footy_match[i, ]$oppositionPhase = "BPO"
         } else {
           footy_match[i, ]$usPhase = "BPO"
           footy_match[i, ]$oppositionPhase = "BP"
         } 
       } else if (footy_match[i, ]$byUs == footy_match[i-1, ]$byUs) {
         footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
         footy_match[i, ]$playNumber = prevPlayNumber
         footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
         footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
          
       # turnover ball
       } else {
         # Put in an end marker for this play when there is a turnover
         footy_extra = footy_match[i, ]
         footy_extra$pseudoEvent = TRUE
         footy_extra$lengthFraction = 1 - footy_extra$lengthFraction
         footy_extra$lengthMetres = 110 - footy_extra$lengthMetres         
         footy_extra$playNumber = prevPlayNumber
         footy_extra$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
         footy_extra$usPhase = footy_match[i-1, ]$usPhase
         footy_extra$oppositionPhase = footy_match[i-1, ]$oppositionPhase
         footy_extra$adjEventName = "turnover"
         
         footy_match <- rbind(footy_match, footy_extra)

         footy_match[i, ]$sequenceNumber = 1
         footy_match[i, ]$playNumber = prevPlayNumber + 1
         
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
    if (footy_match[i, ]$byUs == footy_match[i+1, ]$byUs &
        footy_match[i, ]$adjEventName %in% c("throw in", "dribble", "kick off", "first touch", 
                                             "goal kick", "keeper punt", "corner kick") &
        footy_match[i+1, ]$adjEventName %in% c("first touch")) {
      footy_match[i, ]$adjPass = 1
    }
    prevPlayNumber = footy_match[i, ]$playNumber
    if (footy_match[i, ]$lengthFraction >= 0.66666667) {
      footy_match[i, ]$attackIncursion = 1
    }
    if (footy_match[i, ]$lengthMetres >= (110 - 16.5) &
        footy_match[i, ]$widthMetres <= (75 - 18.75) &
        footy_match[i, ]$widthMetres >= 18.75) {
      footy_match[i, ]$penaltyIncursion = 1
    }
  }
  
}

table(footy_match$byUs, footy_match$adjPass)
table(footy_match$byUs, ifelse(footy_match$pass == "true" &
                                 footy_match$pseudoEvent == FALSE, 1, 0))
