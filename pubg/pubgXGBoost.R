library(data.table)
library(tidyverse)
library(xgboost)
library(tictoc)
library(Matrix)
########################

train <- as_tibble(fread('train.csv', sep = ','))
test <- as_tibble(fread('test.csv', sep = ','))

# To run in kernel on kaggle
# train <- as_tibble(fread('../input/train.csv', sep = ','))
# test <- as_tibble(fread('../input/test.csv', sep = ','))

########################

# Remove matches with large groups 

nPlayersInGroup <- group_by(train, matchId) %>%
  group_by(groupId) %>%
  summarize(nPlayersInGroup = n())

train <- left_join(train, nPlayersInGroup, by = 'groupId')

nPlayersInGroup <- group_by(test, matchId) %>%
  group_by(groupId) %>%
  summarize(nPlayersInGroup = n())

test <- left_join(test, nPlayersInGroup, by = 'groupId')

maxPlayersInGroupTest <- max(nPlayersInGroup$nPlayersInGroup)

largeTrainGroups <- filter(train, nPlayersInGroup > maxPlayersInGroupTest)

train <- filter(train, matchId != any(unique(largeTrainGroups$matchId)))


########################
# Removing matches from training data where maxWinSpread is less than 1
maxWinSpread <- group_by(train, matchId) %>%
  summarize(maxWinSpread = max(winPlacePerc) - min(winPlacePerc))


train <- left_join(train, maxWinSpread, by = 'matchId') %>%
  filter(maxWinSpread == 1) %>%
  select(-maxWinSpread)

########################
# Feature engineering

# Adding totalDist = rideDistance + swimDistance + walkDistance
# Adding healsAndBoosts = heals + boosts
# Normalizing features that depend on number of players in match

nPlayersInMatch <- group_by(train, matchId) %>%
  summarize(nPlayersInMatch = n())

train <- left_join(train, nPlayersInMatch, by = 'matchId')

train <- mutate(train, totalDist = rideDistance + swimDistance + walkDistance) %>%
  mutate(healsAndBoosts = heals + boosts) %>%
  mutate(kills = kills * 100 / nPlayersInMatch)


nPlayersInMatch <- group_by(test, matchId) %>%
  summarize(nPlayersInMatch = n())

test <- left_join(test, nPlayersInMatch, by = 'matchId')

test <- mutate(test, totalDist = rideDistance + swimDistance + walkDistance) %>%
  mutate(healsAndBoosts = heals + boosts)%>%
  mutate(kills = kills * 100 / nPlayersInMatch)

########################
# Drop group data and Id's
train <- select(train, -numGroups, -maxPlace, -matchId, -groupId, -Id, -nPlayersInMatch, -nPlayersInGroup)

testID <- test$Id
testMatchID <- test$matchId
test <- select(test, -numGroups, -maxPlace, -matchId, -groupId, -Id, -nPlayersInMatch, -nPlayersInGroup)

########################
# Prepping for training 
winPlacePercColumn <- which(names(train) == 'winPlacePerc')

y <- train[ ,winPlacePercColumn]

########################

tic()
xgb <- xgboost(data = data.matrix(train[-winPlacePercColumn]), 
               label = data.matrix(y),
               booster = 'gbtree',
               eta = 0.2, # step size of each boosting step
               nround = 250,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               tree_method = 'exact',
               max_depth = 10,
               subsample = 0.8,
               colsample_bytree = 0.8)
toc()

########################
# Predicting on test
test_pred <- predict(xgb, data.matrix(test))


########################
# All matches should have winPlacePerc with range 0-1
test <- bind_cols(as_tibble(testID), as_tibble(testMatchID), test, as_tibble(test_pred)) %>%
  rename(Id = value, matchId = value1, winPlacePerc = value2)

maxWinSpreadTest <- group_by(test, matchId) %>%
  summarize(maxWinSpread = max(winPlacePerc) - min(winPlacePerc))

# maxWinSpreadTest sholud be equal to 1 for test, use normailize to fix
maxWinTest <- group_by(test, matchId) %>%
  summarize(maxWin = max(winPlacePerc))
test <- left_join(test, maxWinTest, by = "matchId")

minWinTest <- group_by(test, matchId) %>%
  summarize(minWin = min(winPlacePerc))
test <- left_join(test, minWinTest, by = "matchId")

test <- mutate(test, winPlacePercNorm = (winPlacePerc - minWin) / (maxWin - minWin)) %>%
  mutate(winPlacePercNorm = ifelse(is.na(winPlacePercNorm) == 1, 0, winPlacePercNorm))

########################
submission <- bind_cols(as_tibble(testID), as_tibble(test$winPlacePercNorm)) %>%
  rename(Id = value, winPlacePerc = value1)

write.csv(submission, file = "submission.csv",row.names=FALSE, quote = FALSE)
