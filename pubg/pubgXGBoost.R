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

# Drop predictors with low correlation and group data
train <- select(train, -numGroups, -maxPlace, -matchId, -groupId, -Id)

testID <- test$Id
test <- select(test, -numGroups, -maxPlace, -matchId, -groupId, -Id)

########################
# XGBoost

winPlacePercColumn <- which(names(train) == 'winPlacePerc')
y <- train[ ,winPlacePercColumn]

# Deviding data in to test and train
smp_size <- floor(0.95 * nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

Xtrain <- train[train_ind, ]
Xtest <- train[-train_ind, ]
ytrain <- y[train_ind, ]
ytest <- y[-train_ind, ]

tic()
xgb <- xgboost(data = data.matrix(Xtrain[-winPlacePercColumn]), 
               label = data.matrix(ytrain),
               booster = 'gbtree',
               eta = 0.2, # step size of each boosting step
               nround = 200,
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

submission <- bind_cols(as_tibble(testID), as_tibble(test_pred)) %>%
  rename(Id = value, winPlacePerc = value1)
