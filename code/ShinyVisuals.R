## ---- load_data  ----
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(shiny)

setwd("/GitDev/Footyapp/data/") # use here function instead
FootyGames <- read.csv("FootyGames.csv", stringsAsFactors = FALSE)

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
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)) )

## ---- end-of-load_data  ----

## ---- plot_win_loss ----
comp <- "2019 NSFA 1 Boys Under 13"
team <- "NFC Red"

# All teams 3 passes or less percentage
ggplot(footy_match_play %>% filter(competition == comp & totalPasses > 0
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 4, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Our Teams breakdown of plays
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                     totalPasses > 0 & playByUs == "true"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# Our throw in stats
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                   playByUs == "true" & firstEvent == "throw in"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# All teams 1pass or less on throw in
ggplot(footy_match_play %>% filter(competition == comp & firstEvent == "throw in"
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 2, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lindfield A throw in stats
ggplot(footy_match_play %>% filter(competition == comp & theirName == "Lindfield A" &
                                     playByUs == "false" & firstEvent == "throw in"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=as.factor(totalPasses), y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# Our keeper punt stats
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                     playByUs == "true" & firstEvent == "keeper punt"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# All teams 1 pass or less on keeper punt
ggplot(footy_match_play %>% filter(competition == comp & firstEvent == "keeper punt"
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 2, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lindfield A keeper punt stats
ggplot(footy_match_play %>% filter(competition == comp & theirName == "Lindfield A" &
                                     playByUs == "false" & firstEvent == "keeper punt"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=as.factor(totalPasses), y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# Our goalkick restart stats
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                     playByUs == "true" & firstEvent == "goal kick"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# All teams 1 pass or less on keeper punt
ggplot(footy_match_play %>% filter(competition == comp & firstEvent == "goal kick"
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 2, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lindfield A keeper punt stats
ggplot(footy_match_play %>% filter(competition == comp & theirName == "Lindfield A" &
                                     playByUs == "false" & firstEvent == "goal kick"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=as.factor(totalPasses), y= perc) ) +
  geom_bar(stat = "identity", fill = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# All teams 3 passes or less percentage
ggplot(footy_match_play %>% filter(competition == comp & totalPasses > 0 &
                            ifelse(playByUs == "true", usPhase, oppositionPhase) == "BPO > BP"
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 4, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# All teams 3 passes or less percentage
ggplot(footy_match_play %>% filter(competition == comp & totalPasses > 0 &
                                     ifelse(playByUs == "true", usPhase, oppositionPhase) == "BP"
) %>% 
  mutate(teamName = ifelse(playByUs == "true", ourName, theirName),
         threePass = ifelse(totalPasses < 4, TRUE, FALSE) ) %>%
  group_by(teamName) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=teamName, y= perc, fill=teamName) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Our Teams breakdown of plays
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                     totalPasses > 0 & playByUs == "true" &
                                     usPhase == "BP"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# Our Teams breakdown of plays
ggplot(footy_match_play %>% filter(competition == comp & ourName == team &
                                     playByUs == "true" &
                                     usPhase == "BPO > BP"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# Our Teams breakdown of plays
ggplot(footy_match_play %>% filter(competition == comp & theirName == "Lindfield A" &
                                     playByUs == "false" &
                                     oppositionPhase == "BPO > BP"
) %>% ungroup() %>% #group_by(ourName) %>%
  mutate(tp = n() ) %>%
  group_by(totalPasses) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=totalPasses, y= perc) ) +
  geom_bar(stat = "identity", color = "light blue") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))

# All teams 3 passes or less percentage
ggplot(footy_match_play %>% filter(competition == comp & totalPasses > 0 &
                                     playByUs == "true"
) %>% 
  mutate(threePass = ifelse(totalPasses < 4, TRUE, FALSE) ) %>%
  group_by(gameID) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=gameID, y= perc) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# transition phase to attack
ggplot(footy_match_play %>% filter(competition == comp & 
                                     playByUs == "true" & usPhase == "BPO > BP"
) %>% 
  mutate(threePass = ifelse(totalPasses < 2, TRUE, FALSE) ) %>%
  group_by(gameID) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=gameID, y= perc) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# transition phase to attack
ggplot(footy_match_play %>% filter(competition == comp & 
                                     playByUs == "true" & firstEvent == "throw in"
) %>% 
  mutate(threePass = ifelse(totalPasses < 2, TRUE, FALSE) ) %>%
  group_by(gameID) %>% mutate(tp = n() ) %>%
  filter(threePass == TRUE) %>%
  summarise(perc = n() / max(tp), cnt = n(), max_tp = max(tp) ),
aes(x=gameID, y= perc) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
