library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)
library(xgboost)
library(caret)

setwd("/GitDev/Footyapp/data/") # use here function instead

footy_match_play <- read.csv("FootyMatchPlayFull.csv", stringsAsFactors = FALSE)

team <- "NFC Red"
#footy_match_play <- footy_match_play %>% 
#                     mutate(passZero = ifelse(totalPasses == 0, 1, 0),
#                           pass1 = ifelse(totalPasses > 0 & totalPasses < 2, 1, 0),
#                           pass2_3 = ifelse(totalPasses > 1 & totalPasses < 4, 1, 0),
#                           pass4_6 = ifelse(totalPasses > 3 & totalPasses < 7, 1, 0),
#                           pass7Plus = ifelse(totalPasses > 6, 1, 0)
#                          )
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

pcnt_fun <- function(x, na.rm = TRUE) (x / sum(x, na.rm) )


footy_match_play$gameDate = substr(footy_match_play$gameDate, 1, 10)

footy_match_game1 <- footy_match_play %>%
#  filter(gameID == "-LE6m4eLGNTqcr9JmgTQ") %>%
   group_by(competition, gameID, teamName, playByUs, joinColumn, orderCol, matchName, gameDate,
           ourName, theirName) %>%
  summarise_at(c("distance","attackDistance", "midfieldDistance",
                 "defenceDistance", "isAttackIncursion", 
                 "isPenaltyIncursion", "isZone17Incursion", "isZone14Incursion",
                 "isZone11PassIncursion", "totalPasses", "totalBackPasses",
                 "totalFwdPasses", "totalSidePasses", "total10mPasses",
                 "total10_20mPasses", "total20mPasses","possessionDuration",
                 "AttackDuration", "MidfieldDuration", "DefenceDuration",
                 "AttackHalfDuration", "DefenceHalfDuration", "playDuration",
                 "attackPasses", "midfieldPasses", "defencePasses",
                 "crossfieldPlay", "goals", "shots", "backPassTurnover",
                 "fwdPassTurnover", "sidePassTurnover", "rightAttack",
                 "middleAttack", "leftAttack", "defenceOut",
                 "MAPlay", "AMPlay", "MDPlay",
                 "DMPlay", "backKeeperPasses", "clearances",
                 "passZero", "pass1", "pass2_3", "pass4_6",
                 "pass7Plus", "shots"), sum, na.rm=TRUE) %>%
  mutate(ppm = totalPasses/(possessionDuration/60),
            aveVelocity = distance/(possessionDuration),
         zone14_17 = isZone17Incursion + isZone14Incursion,
         goal_shots = goals + shots) %>%
  left_join(duplicate, c("joinColumn" = "joinColumn")) %>%
  ungroup () %>%
  group_by(competition, gameID, UsTeam) %>%
  mutate(result = case_when(sum(goals) == 0 ~ "Draw",
                            goals/sum(goals) == 0.5 ~ "Draw",
                            goals/sum(goals) < 0.5 ~ "Loss", 
                            TRUE ~ "Win"),
         default = ifelse(result == "Draw", 1,
                          ifelse(result == "Loss", 0, 2)),
         distance_pcnt = ifelse(distance==0, 0, distance / sum(distance)),
              attackDistance_pcnt = ifelse(attackDistance==0, 0, attackDistance / sum(attackDistance)),
              midfieldDistance_pcnt = ifelse(midfieldDistance==0, 0, midfieldDistance / sum(midfieldDistance)),
              defenceDistance_pcnt = ifelse(defenceDistance==0, 0, defenceDistance / sum(defenceDistance)),
              isAttackIncursion_pcnt = ifelse(isAttackIncursion==0, 0, isAttackIncursion / sum(isAttackIncursion)),
              isPenaltyIncursion_pcnt = ifelse(isPenaltyIncursion==0, 0, isPenaltyIncursion / sum(isPenaltyIncursion)),
              isZone17Incursion_pcnt = ifelse(isZone17Incursion==0, 0, isZone17Incursion / sum(isZone17Incursion)),
              isZone14Incursion_pcnt = ifelse(isZone14Incursion==0, 0, isZone14Incursion / sum(isZone14Incursion)),
              zone14_17_pcnt = ifelse(zone14_17==0, 0, zone14_17 / sum(zone14_17)),
              isZone11PassIncursion_pcnt = ifelse(isZone11PassIncursion==0, 0, isZone11PassIncursion / sum(isZone11PassIncursion)), 
              totalPasses_pcnt = ifelse(totalPasses==0, 0, totalPasses / sum(totalPasses)), 
              totalBackPasses_pcnt = ifelse(totalBackPasses==0, 0, totalBackPasses / sum(totalBackPasses)),
              totalFwdPasses_pcnt = ifelse(totalFwdPasses==0, 0, totalFwdPasses / sum(totalFwdPasses)), 
              totalSidePasses_pcnt = ifelse(totalSidePasses==0, 0, totalSidePasses / sum(totalSidePasses)), 
              total10mPasses_pcnt = ifelse(total10mPasses==0, 0, total10mPasses / sum(total10mPasses)),
              total10_20mPasses_pcnt = ifelse(total10_20mPasses==0, 0, total10_20mPasses / sum(total10_20mPasses)), 
              total20mPasses_pcnt = ifelse(total20mPasses==0, 0, total20mPasses / sum(total20mPasses)),
              possessionDuration_pcnt = ifelse(possessionDuration==0, 0, possessionDuration / sum(possessionDuration)),
              AttackDuration_pcnt = ifelse(AttackDuration==0, 0, AttackDuration / sum(AttackDuration)), 
              MidfieldDuration_pcnt = ifelse(MidfieldDuration==0, 0, MidfieldDuration / sum(MidfieldDuration)), 
              DefenceDuration_pcnt = ifelse(DefenceDuration==0, 0, DefenceDuration / sum(DefenceDuration)),
              AttackHalfDuration_pcnt = ifelse(AttackHalfDuration==0, 0, AttackHalfDuration / sum(AttackHalfDuration)), 
              DefenceHalfDuration_pcnt = ifelse(DefenceHalfDuration==0, 0, DefenceHalfDuration / sum(DefenceHalfDuration)), 
              playDuration_pcnt = ifelse(playDuration==0, 0, playDuration / sum(playDuration)),
              attackPasses_pcnt = ifelse(attackPasses==0, 0, attackPasses / sum(attackPasses)), 
              midfieldPasses_pcnt = ifelse(midfieldPasses==0, 0, midfieldPasses / sum(midfieldPasses)), 
              defencePasses_pcnt = ifelse(defencePasses==0, 0, defencePasses / sum(defencePasses)),
              crossfieldPlay_pcnt = ifelse(crossfieldPlay==0, 0, crossfieldPlay / sum(crossfieldPlay)), 
              goals_pcnt = ifelse(goals==0, 0, goals / sum(goals)), 
              shots_pcnt = ifelse(shots==0, 0, shots / sum(shots)), 
              goal_shots_pcnt = ifelse(goal_shots==0, 0, goal_shots / sum(goal_shots)), 
              backPassTurnover_pcnt = ifelse(backPassTurnover==0, 0, backPassTurnover / sum(backPassTurnover)),
              fwdPassTurnover_pcnt = ifelse(fwdPassTurnover==0, 0, fwdPassTurnover / sum(fwdPassTurnover)), 
              sidePassTurnover_pcnt = ifelse(sidePassTurnover==0, 0, sidePassTurnover / sum(sidePassTurnover)), 
              rightAttack_pcnt = ifelse(rightAttack==0, 0, rightAttack / sum(rightAttack)),
              middleAttack_pcnt = ifelse(middleAttack==0, 0, middleAttack / sum(middleAttack)), 
              leftAttack_pcnt = ifelse(leftAttack==0, 0, leftAttack / sum(leftAttack)), 
              defenceOut_pcnt = ifelse(defenceOut==0, 0, defenceOut / sum(defenceOut)),
              MAPlay_pcnt  = ifelse(MAPlay==0, 0, MAPlay / sum(MAPlay)),
              AMPlay_pcnt = ifelse(AMPlay==0, 0, AMPlay / sum(AMPlay)),
              MDPlay_pcnt = ifelse(MDPlay==0, 0, MDPlay / sum(MDPlay)),
              DMPlay_pcnt = ifelse(DMPlay==0, 0, DMPlay / sum(DMPlay)),
              backKeeperPasses_pcnt  = ifelse(backKeeperPasses==0, 0, backKeeperPasses / sum(backKeeperPasses)),
              clearances_pcnt = ifelse(clearances==0, 0, clearances / sum(clearances)),
              passZero_pcnt  = ifelse(passZero==0, 0, passZero / sum(passZero)),
              pass1_pcnt  = ifelse(pass1==0, 0, pass1 / sum(pass1)),
              pass2_3_pcnt  = ifelse(pass2_3==0, 0, pass2_3 / sum(pass2_3)),
              pass4_6_pcnt = ifelse(pass4_6==0, 0, pass4_6 / sum(pass4_6)),
              pass7Plus_pcnt  = ifelse(pass7Plus==0, 0, pass7Plus / sum(pass7Plus)),
              ppm_pcnt = ifelse(ppm==0, 0, ppm / sum(ppm)),
              aveVelocity_pcnt = ifelse(aveVelocity==0, 0, aveVelocity / sum(aveVelocity)) ) %>%
  filter(UsTeam == 1)
  
# Remove game with strange stats.. 
footy_match_game1 <- footy_match_game1[!footy_match_game1$competition == "2017 Mens Open", ]

library(corrplot)
abc<- footy_match_game1[, c(64:115)]
abc$default <- ifelse(footy_match_game1$result == "Draw", 1,
                      ifelse(footy_match_game1$result == "Loss", 0, 2))
M<-cor(abc)
corrplot(M, method="color",
         , type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = 0.7)#, mar = c(1, 1, 1, 1))

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(M, type = "lower",
           insig = "blank")
ggcorrplot(mcor, tl.srt=45)
?ggcorrplot
mcor <- cor(x = as.numeric(footy_data$default), y = footy_data[-1], use="complete.obs")
corrplot(mcor, tl.srt = 45, order = "hclust")
?corrplot
write.csv(footy_match_game1, "FootyGameML.csv", row.names = FALSE)
ncol(footy_match_game1)
ct <- cor(footy_match_game1[63:113])
table(footy_match_game1$default, ifelse(footy_match_game1$goals_pcnt > 0.5, 2, ifelse(footy_match_game1$goals_pcnt < 0.5, 0, 1)))

quest <- footy_match_game1[footy_match_game1$default== 1 &
                    footy_match_game1$goals_pcnt != 0.5, ]
install.packages("psych")
library(psych)
ct <- psych::corr.test(footy_match_game1[62:111])
write.csv(ct$n,file="ntmp.csv")  ## sample sizes
write.csv(ct$t,file="ttmp.csv")  ## t statistics
write.csv(ct$p,file="ptmp.csv")  ## p-values
# Run RandomForest and remove ID field as this is NOT a predictor
footy_match_game1$result <- as.factor(footy_match_game1$result)
footy_match_game1$default = as.factor(footy_match_game1$default)


# Convert the Species factor to an integer class starting at 0
# This is picky, but it's a requirement for XGBoost

str(footy_match_game1)
results = footy_match_game1$result

label = as.integer(footy_match_game1$result)-1

xgb.data <- footy_match_game1[, c("totalPasses_pcnt", "totalBackPasses_pcnt",
                                  "totalFwdPasses_pcnt", "totalSidePasses_pcnt", "total10mPasses_pcnt",
                                  "total10_20mPasses_pcnt", "total20mPasses_pcnt",
                                  "crossfieldPlay_pcnt", "backPassTurnover_pcnt",
                                  "fwdPassTurnover_pcnt", "sidePassTurnover_pcnt", "rightAttack_pcnt",
                                  "middleAttack_pcnt", "leftAttack_pcnt","backKeeperPasses_pcnt", "clearances_pcnt",
                                  "passZero_pcnt", "pass1_pcnt", "pass2_3_pcnt", "pass4_6_pcnt",
                                  "pass7Plus_pcnt", "ppm_pcnt", "aveVelocity_pcnt")]
                              #c(63:66, 69:89, 92:111)]
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
summary(xgb.fit)


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

xgb.importance(colnames(xgb.train), model = xgb.fit)
plot(xgb.fit)

n = nrow(footy_match_game1)
footy_data <- footy_match_game1[, c("totalPasses_pcnt", "totalBackPasses_pcnt",
                                    "totalFwdPasses_pcnt", "totalSidePasses_pcnt", "total10mPasses_pcnt",
                                    "total10_20mPasses_pcnt", "total20mPasses_pcnt",
                                    "crossfieldPlay_pcnt", "backPassTurnover_pcnt",
                                    "fwdPassTurnover_pcnt", "sidePassTurnover_pcnt", "rightAttack_pcnt",
                                    "middleAttack_pcnt", "leftAttack_pcnt","backKeeperPasses_pcnt", "clearances_pcnt",
                                    "passZero_pcnt", "pass1_pcnt", "pass2_3_pcnt", "pass4_6_pcnt",
                                    "pass7Plus_pcnt", "shots_pcnt", "ppm_pcnt", "aveVelocity_pcnt")]
                                #c(63:66, 69:89, 92:111)]
footy_data$default <- factor(footy_match_game1$default)
train.index = sample(n,floor(0.75*n))
train.data = footy_data[!footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                    "2019 NSFA 1 Boys Under 13", 
                                                                    "2019 World Cup 2019 Mens Open",             
                                                                    "2019 International Friendly Mens Open"   ,  
                                                                    "2019 Boys Under 14"                ,        
                                                                    "2019 Icc Football Mens Open"
),]
test.data = footy_data[footy_match_game1$competition %in% c("2019 NSFA 1 Girls Under 16", 
                                                                   "2019 NSFA 1 Boys Under 13", 
                                                                   "2019 World Cup 2019 Mens Open",             
                                                                   "2019 International Friendly Mens Open"   ,  
                                                                   "2019 Boys Under 14"                ,        
                                                                   "2019 Icc Football Mens Open"
),]

tx.rf <- randomForest(default ~ . ,
                      data = train.data, 
                      importance=TRUE, ntree=2000, #xtest=testset[, !(colnames(testset) %in% c("ID", 'default'))],
                      keep.forest = T)

test.data$probability = predict(tx.rf, newdata = test.data, type = "response")
test.data$prob  = predict(tx.rf, newdata = test.data, type = "prob")

table(test.data$default, test.data$probability)

result = sum(test.data$default==test.data$probability)/nrow(test.data)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

varImp(tx.rf)

library(gbm)

# defining some parameters
gbm_depth = 10 #maximum nodes per tree
gbm_n.min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 8 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 10000

gbm_clf = gbm(default ~ .,
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
summary(gbm_clf, plotit = FALSE)
varImp(gbm_clf)

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

install.packages("mlbench")

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
str(footy_data)
footy_data$default = as.integer(footy_data$default)
# calculate correlation matrix
correlationMatrix <- cor(footy_data)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
