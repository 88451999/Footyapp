library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)
library(xgboost)

setwd("/GitDev/Footyapp/data/") # use here function instead

FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)

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
footy_match_play$joinColumn <- 1

joinColumn <- c(1, 1)
UsTeam <- c(1, 2)
duplicate <- data.frame(joinColumn, UsTeam)

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


# Convert the Species factor to an integer class starting at 0
# This is picky, but it's a requirement for XGBoost

results = footy_match_game1$result
label = as.integer(footy_match_game1$result)-1
xgb.data <- footy_match_game1[, c(34:58)]
xgb.data$default = NULL
xgb.data$result = NULL

train.index = !footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                   "2019 NSFA 1 Boys Under 13", 
                                                                   "2019 World Cup 2019 Mens Open",             
                                                                   "2019 International Friendly Mens Open"   ,  
                                                                   "2019 Boys Under 14"                ,        
                                                                   "2019 Icc Football Mens Open"
)

n = nrow(xgb.data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(xgb.data[!footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                       "2019 NSFA 1 Boys Under 13", 
                                                                       "2019 World Cup 2019 Mens Open",             
                                                                       "2019 International Friendly Mens Open"   ,  
                                                                       "2019 Boys Under 14"                ,        
                                                                       "2019 Icc Football Mens Open"
), ])
train.label = label[!footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                          "2019 NSFA 1 Boys Under 13", 
                                                          "2019 World Cup 2019 Mens Open",             
                                                          "2019 International Friendly Mens Open"   ,  
                                                          "2019 Boys Under 14"                ,        
                                                          "2019 Icc Football Mens Open"
)]
test.data = as.matrix(xgb.data[footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                     "2019 NSFA 1 Boys Under 13", 
                                                                     "2019 World Cup 2019 Mens Open",             
                                                                     "2019 International Friendly Mens Open"   ,  
                                                                     "2019 Boys Under 14"                ,        
                                                                     "2019 Icc Football Mens Open"
),])
test.label = label[footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                         "2019 NSFA 1 Boys Under 13", 
                                                         "2019 World Cup 2019 Mens Open",             
                                                         "2019 International Friendly Mens Open"   ,  
                                                         "2019 Boys Under 14"                ,        
                                                         "2019 Icc Football Mens Open"
)]

str(xgb.train)
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

# Define the parameters for multinomial classification
num_class = 3 # Win, Loss, Draw
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(results)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])

# Calculate the final accuracy
xgb.pred$label = levels(results)[test.label+1]
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))


table(xgb.pred$label, xgb.pred$prediction)


n = nrow(footy_match_game1)
train.index = sample(n,floor(0.75*n))
train.data = footy_match_game1[!footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                    "2019 NSFA 1 Boys Under 13", 
                                                                    "2019 World Cup 2019 Mens Open",             
                                                                    "2019 International Friendly Mens Open"   ,  
                                                                    "2019 Boys Under 14"                ,        
                                                                    "2019 Icc Football Mens Open"
),]
test.data = footy_match_game1[footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                   "2019 NSFA 1 Boys Under 13", 
                                                                   "2019 World Cup 2019 Mens Open",             
                                                                   "2019 International Friendly Mens Open"   ,  
                                                                   "2019 Boys Under 14"                ,        
                                                                   "2019 Icc Football Mens Open"
),]

tx.rf <- randomForest(default ~ shotPcnt  + zone14PlayPcnt + 
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
                      data = train.data, 
                      importance=TRUE, ntree=2000, #xtest=testset[, !(colnames(testset) %in% c("ID", 'default'))],
                      keep.forest = T)

test.data$probability = predict(tx.rf, newdata = test.data, type = "response")
test.data$prob  = predict(tx.rf, newdata = test.data, type = "prob")

table(test.data$default, test.data$probability)

result = sum(test.data$default==test.data$probability)/nrow(test.data)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))



library(gbm)

# defining some parameters
gbm_depth = 10 #maximum nodes per tree
gbm_n.min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 8 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 10000

gbm_clf = gbm(default ~ shotPcnt + distPcnt + zone14PlayPcnt + 
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
                playsAttTurnPcnt,
              data=train.data,
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

predBST = predict(gbm_clf,n.trees=10000, newdata=test.data,type='response')

p.predBST <- apply(predBST, 1, which.max) - 1
test.data$bst <- p.predBST

table(test.data$default, test.data$bst)
summary(gbm_clf, ylab = "Variable", main = "Variable Relative Importance")
#OR just as a table
summary(gbm_clf)

result = sum(test.data$default==test.data$bst)/nrow(test.data)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))


install.packages("corrplot")
library(corrplot)
abc<- footy_match_game1[, c(34:37, 40:55)]
abc$default <- ifelse(footy_match_game1$result == "Draw", 1,
                     ifelse(footy_match_game1$result == "Loss", 0, 2))
M<-cor(abc)
corrplot(M, method="color",
         , type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = 0.7)#, mar = c(1, 1, 1, 1))
?corrplot
