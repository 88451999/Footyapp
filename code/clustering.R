library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)
library(factoextra)
library(cluster)
library(ggrepel)
library(ggbeeswarm)

setwd("/GitDev/Footyapp/data/") # use here function instead
FootyGames1 <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)
team <- "NFC Red"

FootyGames <- FootyGames1 %>% filter(competition %in% c(#"2019 NSFA 1 Girls Under 16", 
                                                          "2019 NSFA 1 Boys Under 13", 
                                                        "2019 World Cup 2019 Mens Open",             
                                                        "2019 International Friendly Mens Open"   ,  
                                                        #  "2019 Boys Under 14"                ,        
                                                        "2019 Icc Football Mens Open"
)
)
write.csv(FootyGames, "FootyGamesClus.csv", row.names = FALSE)

FootyGames1 <- read.csv("FootyGamesClus.csv", stringsAsFactors = FALSE)
team <- "NFC Red"

FootyGames <- FootyGames1 %>% filter(competition %in% c(#"2019 NSFA 1 Girls Under 16", 
                      "2019 NSFA 1 Boys Under 13", 
                      "2019 World Cup 2019 Mens Open",             
                      "2019 International Friendly Mens Open"   ,  
                    #  "2019 Boys Under 14"                ,        
                      "2019 Icc Football Mens Open"
                )
)
FootyGames$kickoff_date = as.POSIXct(substr(FootyGames$time, 1, 19), format='%Y-%m-%d %H:%M:%S')
footy_match_play <- FootyGames %>% #filter(playNumber == 1) %>%
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(gameDate = first(kickoff_date),
            playByUs = first(byUs),
            matchName = first(matchName),
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
#footy_match_play$joinColumn <- 1

#joinColumn <- c(1, 1)
#UsTeam <- c(1, 2)
#duplicate <- data.frame(joinColumn, UsTeam)

#install.packages("prcomp")
footy_match_pca <- footy_match_play %>%
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
            attackPlayPcnt = sum(ifelse(lastArea == "Attack", 1, 0)) / plays,
            midfieldPlayPcnt = sum(ifelse(lastArea == "Midfield", 1, 0)) / plays,
            defencePlayPcnt = sum(ifelse(lastArea == "Defence", 1, 0)) / plays,
            penaltyPlayPcnt = sum(ifelse(isPenaltyIncursion == 1, 1, 0)) / plays,
            zone14PlayPcnt = sum(ifelse(isZone14Incursion == 1, 1, 0)) / plays,
            zone17PlayPcnt = sum(ifelse(isZone17Incursion == 1, 1, 0)) / plays,
            backPassPcnt = sum(totalBackPasses) / sum(totalPasses),
            sidePassPcnt = sum(totalSidePasses) / sum(totalPasses),
            fwdPassPcnt = sum(totalFwdPasses) / sum(totalPasses),
            Pass10mPcnt = sum(total10mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass10_20mPcnt = sum(total10_20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass20mPcnt = sum(total20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            noPassPlayPcnt = sum(ifelse(totalPasses == 0, 1, 0)) / plays,
            passPlayPcnt = sum(ifelse(totalPasses > 1, 1, 0)) / plays,
            Plays1_3PassesPcnt = sum(ifelse(totalPasses > 0 & totalPasses <= 3, 1, 0)) / plays,
            Plays4_6PassesPcnt = sum(ifelse(totalPasses > 3 & totalPasses <= 6, 1, 0)) / plays,
            Plays7PlusPassesPcnt = sum(ifelse(totalPasses > 6, 1, 0)) / plays,
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

install.packages("Rtsne")
library(Rtsne)
fmpca[c( 14:18, 22:48)] <- lapply(fmpca[c(14:18, 22:48)], function(x) c(scale(x)))
fmpca <- fmpca %>%
  mutate_at(c( 14:18, 22:48), funs(c(scale(.))))
is.na(fmpca[c( 14:18, 22:48)])
tsne <- Rtsne(fmpca[c( 14:18, 22:48)], dims = 2, perplexity=3, verbose=TRUE, max_iter = 500)
plot(tsne)
plot(tsne$Y, main="tsne")

fmpca$sne_X <- tsne$Y[, 1]
fmpca$sne_Y <- tsne$Y[, 2]

ggplot(fmpca, mapping = aes(x=sne_X, y=sne_Y, colour=competition)) +
  geom_point() +
geom_label_repel(aes(label=paste(teamName, result, theirName)), 
                 box.padding = 0.5, point.padding =0.1, 
                 segment.color = 'grey50', seed = 13,
                 size=2)

fmpca <- footy_match_pca  %>% filter(competition == "2019 NSFA 1 Boys Under 13")

fmpca.pr <- prcomp(fmpca[c( 14:18, 22:48)], center = TRUE, scale = TRUE)
summary(fmpca.pr)
is.na(fmpca)
screeplot(fmpca.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(fmpca.pr$sdev^2 / sum(fmpca.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

plot(fmpca.pr$x[,1],fmpca.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")


rownames(fmpca.pr)
?fviz_pca_ind
fviz_pca_ind(fmpca.pr, geom.ind = c("point", "text"), 
             pointshape = 21, 
             pointsize = 2, 
             fill.ind = ifelse(fmpca$competition=="2019 NSFA 1 Boys Under 13", 
                               paste(ifelse(fmpca$playByUs == "true", "Us", "Opposition")),
                               ifelse(fmpca$competition=="2019 Boys Under 14", "14s", 
                                      "Top Flight Teams")), #paste(fmpca$matchName, fmpca$result), 
             col.ind = "black", 
             palette = "Dark2", 
             addEllipses = FALSE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             mean.point = FALSE,
             legend.title = "Result") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label_repel(aes(label=paste(fmpca$teamName, fmpca$result, fmpca$theirName)), 
                   box.padding = 0.1, point.padding =0.1, 
                   segment.color = 'grey50', seed = 13,
                   label.size = 0.1, size=2)

unique(fmpca$competition)
fviz_pca_biplot(fmpca.pr, geom = c("point", "text"),
                col.var="green",
                pointshape = 21, 
                pointsize = 2, 
                fill.ind = ifelse(fmpca$competition=="2019 NSFA 1 Boys Under 13", 
                                  paste(ifelse(fmpca$playByUs == "true", "Us", "Opposition")),
                                  ifelse(fmpca$competition=="2019 Boys Under 14", "14s", 
                                  "Top Flight Teams")), #paste(fmpca$matchName, fmpca$result), 
                alpha.var = 0.2, 
                repel = TRUE,
                mean.point = FALSE)+
  theme_minimal()


%>%
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


footy_match_pca <- footy_match_play %>%
  group_by(competition, teamName, playByUs) %>%
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
            attackPlayPcnt = sum(ifelse(lastArea == "Attack", 1, 0)) / plays,
            midfieldPlayPcnt = sum(ifelse(lastArea == "Midfield", 1, 0)) / plays,
            defencePlayPcnt = sum(ifelse(lastArea == "Defence", 1, 0)) / plays,
            penaltyPlayPcnt = sum(ifelse(isPenaltyIncursion == 1, 1, 0)) / plays,
            zone14PlayPcnt = sum(ifelse(isZone14Incursion == 1, 1, 0)) / plays,
            zone17PlayPcnt = sum(ifelse(isZone17Incursion == 1, 1, 0)) / plays,
            backPassPcnt = sum(totalBackPasses) / sum(totalPasses),
            sidePassPcnt = sum(totalSidePasses) / sum(totalPasses),
            fwdPassPcnt = sum(totalFwdPasses) / sum(totalPasses),
            Pass10mPcnt = sum(total10mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass10_20mPcnt = sum(total10_20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            Pass20mPcnt = sum(total20mPasses) / sum(total10mPasses + total10_20mPasses + total20mPasses),
            noPassPlayPcnt = sum(ifelse(totalPasses == 0, 1, 0)) / plays,
            passPlayPcnt = sum(ifelse(totalPasses > 1, 1, 0)) / plays,
            Plays1_3PassesPcnt = sum(ifelse(totalPasses > 0 & totalPasses <= 3, 1, 0)) / plays,
            Plays4_6PassesPcnt = sum(ifelse(totalPasses > 3 & totalPasses <= 6, 1, 0)) / plays,
            Plays7PlusPassesPcnt = sum(ifelse(totalPasses > 6, 1, 0)) / plays,
            playsAttTurn = sum(ifelse(Area == "Attack" & firstEvent == "first touch", 1, 0)),
            matchName = max(matchName)
  ) 

%>%
  ungroup () %>%
  group_by(competition, gameID) %>%
  mutate(result = case_when(sum(goals) == 0 ~ "Draw",
                            goals/sum(goals) == 0.5 ~ "Draw",
                            goals/sum(goals) < 0.5 ~ "Loss",
                            TRUE ~ "Win")
  )

write.csv(fmpca,file="fmpca.csv", row.names = FALSE)
fmpca <- as.data.frame(footy_match_pcac)
rownames(fmpca) <- paste(gsub('\\b(\\pL)\\pL{0,}|.','\\U\\1',fmpca$teamName,perl = TRUE), case_when(fmpca$teamName == "Beaconhill" ~ "B16",
                                                                                                    fmpca$teamName == "W Pymble" ~ "W16",
                                                                                                    fmpca$teamName == "NFC White" ~ "W16",
                                                                                                    fmpca$teamName == "STU" ~ "S16",
                                                                                                   fmpca$competition == "2019 NSFA 1 Girls Under 16" ~ "16",
                                                                                                   fmpca$competition == "2019 NSFA 1 Boys Under 13" ~ "13",
                                                                                                   fmpca$competition == "2019 Boys Under 14" ~ "14",
                                                                                                   TRUE ~ "") )

rownames(fmpca.pr) <- paste(gsub('\\b(\\pL)\\pL{0,}|.','\\U\\1',fmpca$teamName,perl = TRUE), case_when(fmpca$teamName == "Beaconhill" ~ "B16",
                                                                                                    fmpca$teamName == "W Pymble" ~ "W16",
                                                                                                    fmpca$teamName == "NFC White" ~ "W16",
                                                                                                    fmpca$teamName == "STU" ~ "S16",
                                                                                                    fmpca$competition == "2019 NSFA 1 Girls Under 16" ~ "16",
                                                                                                    fmpca$competition == "2019 NSFA 1 Boys Under 13" ~ "13",
                                                                                                    fmpca$competition == "2019 Boys Under 14" ~ "14",
                                                                                                    TRUE ~ "") )


fmpca.pr <- prcomp(fmpca[c(7:9, 16:48)], center = TRUE, scale = TRUE)
summary(fmpca.pr)

fviz_pca_ind(fmpca.pr, geom = c("point"),
             pointshape = 21, 
             pointsize = 2, 
             fill.ind = fmpca$competition, #paste(fmpca$matchName, fmpca$result), 
             col.ind = "black", 
           #  palette = "Set1", 
             addEllipses = FALSE,
           #  label  = "var",
             col.var = "black",
             repel = TRUE,
             mean.point = FALSE,
             legend.title = "Result") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

library(factoextra)
fviz_pca_var(fmpca.pr, geom = c("point", "text"),
             col.var="steelblue",
             repel = TRUE)+
  theme_minimal()


#### K Means
prcomp(fmpca[c(8:10, 15:46)], center = TRUE, scale = TRUE)
m<-as.matrix(fmpca[c( 14:18, 22:48)])
m <- scale(m)
#write as csv file
write.csv(m,file="dtmAsMatrix.csv")
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,7),rep("..",nrow(m)),
                   substring(rownames(m),
                               nchar(rownames(m))-7,nchar(rownames(m))-4))
#compute distance between document vectors
rownames(m) <- gsub('\\b(\\pL)\\pL{0,}|.','\\U\\1',fmpca$teamName,perl = TRUE)
d <- dist(m)

gsub('\\b(\\pL)\\pL{0,}|.','\\U\\1',fmpca$teamName,perl = TRUE)

#kmeans clustering
#kmeans - run with nstart=100 and k=2,3,5 to compare results with hclust
kfit <- kmeans(d, 3, nstart=100)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#rerun using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 3, nstart=100)

clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
?clusplot


groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 2 subtrees. Try 3,4,5,6 cuts; comment on your results
rect.hclust(groups,2)
hclusters <- cutree(groups,2)

groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups,2)

# Visualize using factoextra
fviz_dend(groups, k = 3, # Cut in 8 groups
          cex = 0.5, # label size
          horiz= TRUE, rect = TRUE # Add rectangle around groups
)

fviz_cluster(kfit, cd, ellipse = TRUE, ellipse.alpha= 0.1,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
             main= FALSE, xlab= FALSE, ylab = FALSE)

install.packages("mclust")
library(mclust)
gmm.mclust <- Mclust(d, 3)
plot(gmm.mclust)

1
str(d)
, gaussian_comps = 3)

test<-c(rnorm(1000),rnorm(1000,mean = 3,sd = 1))
hist(test)
a<-gmm(test,2)
plot(a)
write.csv(d,file="d.csv")
str(d)
, dist_mode = "eucl_dist",
    seed_mode = "random_subset", km_iter = 10, em_iter = 5,
    verbose = FALSE, var_floor = 1e-10, seed = 1)