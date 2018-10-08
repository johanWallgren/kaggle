library(data.table)
library(tidyverse)
library(corrplot)
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

corM <- cor(train)
corrplot(corM, method = 'circle')

# Correlation with winPlacePerc
#--------------------#
# HIGH CORR PREDICTORS
# walkDistance   	0.8119
# killPlace      -0.7083
# boosts         	0.6181
# weaponsAcquired	0.5715
# damageDealt    	0.4386
# heals          	0.4280
# kills          	0.4153
# longestKill    	0.4058
# killStreaks    	0.3725
# assists        	0.3046
# rideDistance   	0.3012
# DBNOs          	0.2795
# headshotKills  	0.2787
# revives        	0.2514
#--------------------#
# LOW CORR PREDICTORS (TO BE REMOVED BEFORE TRAINING)
# winPoints      	0.1704
# swimDistance   	0.1549
# roadKills      	0.0289
# teamKills      -0.0061
# killPoints     	0.0903 
# vehicleDestroys	0.0577
#--------------------#
# GROUP DATA (TO BE REMOVED BEFORE TRAINING)
# numGroups      	0.0358
# maxPlace       	0.0342
#--------------------#
# NONE GAME STATS (TO BE REMOVED BEFORE TRAINING)
# matchId        	0.0004
# groupId        -0.0002
# Id             -0.0182


########################

# Drop predictors with low correlation and group data
train <- select(train, -winPoints, -swimDistance, -roadKills, -teamKills, -killPoints, -vehicleDestroys,
                -numGroups, -maxPlace, -matchId, -groupId, -Id)

testID <- test$Id
test <- select(test, -winPoints, -swimDistance, -roadKills, -teamKills, -killPoints, -vehicleDestroys,
               -numGroups, -maxPlace, -matchId, -groupId, -Id)

########################
# XGBoost

winPlacePercColumn <- which(names(train) == 'winPlacePerc')
y <- train[ ,winPlacePercColumn]

# Deviding data in to test and train
smp_size <- floor(0.85 * nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

Xtrain <- train[train_ind, ]
Xtest <- train[-train_ind, ]
ytrain <- y[train_ind, ]
ytest <- y[-train_ind, ]

tic()
xgb <- xgboost(data = data.matrix(Xtrain[-winPlacePercColumn]), 
               label = data.matrix(ytrain),
               booster = 'gbtree',
               eta = 0.1, # step size of each boosting step
               nround = 50,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               tree_method = 'exact',
               max_depth = 20,
               subsample = 0.8,
               colsample_bytree = 0.8)
toc()

y_pred <- as_tibble(predict(xgb, data.matrix(Xtest[-winPlacePercColumn])))

# Using root mean squared as error function
rmseXGB <- sqrt(sum((y_pred - ytest)^2) / nrow(ytest))
print(rmseXGB)

# Predicting on test
test_pred <- predict(xgb, data.matrix(test))

submission <- bind_cols(as_tibble(testID), as_tibble(test_pred)) %>%
  rename(Id = value, winPlacePerc = value1)

