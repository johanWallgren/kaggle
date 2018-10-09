library(data.table)
library(tidyverse)
library(xgboost)
library(tictoc)
library(Matrix)
########################

train <- as_tibble(fread('train.csv', sep = ','))
test <- as_tibble(fread('test.csv', sep = ','))

########################

# Remove matches with large groups 

nPlayersInGroup <- group_by(train, matchId) %>%
  group_by(groupId) %>%
  summarize(nPlayersInGroup = n())

train <- left_join(train, nPlayersInGroup, by = 'groupId')

nPlayersInGroup <- group_by(test, matchId) %>%
  group_by(groupId) %>%
  summarize(nPlayersInGroup = n())

maxPlayersInGroupTest <- max(nPlayersInGroup$nPlayersInGroup)

largeTrainGroups <- filter(train, nPlayersInGroup > maxPlayersInGroupTest)

train <- filter(train, matchId != any(unique(largeTrainGroups$matchId))) %>%
  select(-nPlayersInGroup)

########################
# Removing matches where max(winPlacePerc) is less than 1

maxWinSpread <- group_by(train, matchId) %>%
  summarize(maxWinSpread = max(winPlacePerc) - min(winPlacePerc))

train <- left_join(train, maxWinSpread, by = 'matchId') %>%
  filter(maxWinSpread == 1) %>%
  select(-maxWinSpread)

########################

playersInMatchTest <- group_by(test, matchId) %>%
  summarize(nPlayersInMatch = n())

hist(playersInMatchTest$nPlayersInMatch,100)

playersInMatchTrain <- group_by(train, matchId) %>%
  summarize(nPlayersInMatch = n())

hist(playersInMatchTrain$nPlayersInMatch,100)

########################
# Drop group data and Id's
train <- select(train, -numGroups, -maxPlace, -matchId, -groupId, -Id)

testID <- test$Id
testMatchID <- test$matchId
test <- select(test, -numGroups, -maxPlace, -matchId, -groupId, -Id)

########################
# XGBoost

winPlacePercColumn <- which(names(train) == 'winPlacePerc')
y <- train[ ,winPlacePercColumn]

# Deviding data in to test and train
smp_size <- floor(0.95 * nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = smp_size, replace = FALSE)

Xtrain <- train[train_ind, ]
Xtest <- train[-train_ind, ]
ytrain <- y[train_ind, ]
ytest <- y[-train_ind, ]

tic()
xgb <- xgboost(data = data.matrix(Xtrain[-winPlacePercColumn]),
               label = data.matrix(ytrain),
               booster = 'gbtree',
               eta = 0.2, # step size of each boosting step
               nround = 10,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               tree_method = 'exact',
               max_depth = 10,
               subsample = 0.8,
               colsample_bytree = 0.8)
toc()

y_pred <- as_tibble(predict(xgb, data.matrix(Xtest[-winPlacePercColumn]))) %>%
  mutate(value = ifelse(value > 1, 1, value),
         value = ifelse(value < 0, 0, value))


# Using root mean squared as error function
rmseXGB <- sqrt(sum((y_pred - ytest)^2) / nrow(ytest))
print(rmseXGB)

# Predicting on test
test_pred <- predict(xgb, data.matrix(test))


# All matches should have winPlacePerc with range 0-1
test <- bind_cols(as_tibble(testID), as_tibble(testMatchID), test, as_tibble(test_pred)) %>%
  rename(Id = value, matchId = value1, winPlacePerc = value2)

maxWinSpreadTest <- group_by(test, matchId) %>%
  summarize(maxWinSpread = max(winPlacePerc) - min(winPlacePerc))

hist(maxWinSpreadTest$maxWinSpread,100)

# maxWinSpreadTest sholud be equal to 1 for test, use normailize to fix

maxWinTest <- group_by(test, matchId) %>%
  summarize(maxWin = max(winPlacePerc))

test <- left_join(test, maxWinTest, by = "matchId")

minWinTest <- group_by(test, matchId) %>%
  summarize(minWin = min(winPlacePerc))

test <- left_join(test, minWinTest, by = "matchId")

test <- mutate(test, winPlacePercNorm = (winPlacePerc - minWin) / (maxWin - minWin))

submission <- bind_cols(as_tibble(testID), as_tibble(test$winPlacePercNorm)) %>%
  rename(Id = value, winPlacePerc = value1)
