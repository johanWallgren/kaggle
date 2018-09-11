library(data.table)
library(tidyverse)
library(lubridate)
library(caret)
library(tictoc)
library(flexclust)
library(geosphere)
library(xgboost)
library(Matrix)

# n = 1e5, nround = 400, eta = 0.05 -> RMSE = 4.68
# n = 1e5, nround = 400, eta = 0.05 -> RMSE = 4.53
# n = 1e5, nround = 400, eta = 0.05 -> RMSE = 4.06
# n = 1e5, nround = 400, eta = 0.05 -> RMSE = 4.62
# n = 2e5, nround = 400, eta = 0.05 -> RMSE = 3.98
# n = 2e5, nround = 400, eta = 0.05 -> RMSE = 4.25
# n = 2e5, nround = 500, eta = 0.05 -> RMSE = 4.36
# n = 2e5, nround = 500, eta = 0.05 -> RMSE = 3.75
# n = 5e5, nround = 700, eta = 0.05 -> RMSE = 4.14
# n = 5e5, nround = 1000, eta = 0.05 -> RMSE = 4.14
# n = 5e5, nround = 1000, eta = 0.10 -> RMSE = 4.14
# n = 1e7, nround = 500, eta = 0.10 -> RMSE = 3.71

# allData <- as_tibble(fread("train.csv"))
# save(allData, file = "trainData.RData")
load('trainData.RData')
sampleTrain <- sample_n(allData, 3e7, replace = FALSE)
rm(allData)

testData <- as_tibble(read.csv('test.csv', sep = ','))

# filter out outliers

sampleTrain <- filter(sampleTrain,
                      pickup_longitude < -72.5 &
                        pickup_longitude > -74.5 &
                        pickup_latitude > 40.4 &
                        pickup_latitude < 41.8 &
                        dropoff_longitude < -72.5 &
                        dropoff_longitude > -74.5 &
                        dropoff_latitude > 40.4 &
                        dropoff_latitude < 41.8 &
                        fare_amount > 1 &
                        fare_amount <= 300 &
                        passenger_count <= 6)

# Splitting up pickup_datetime 
sampleTrain <- mutate(sampleTrain,
                      pickup_datetime = ymd_hms(pickup_datetime),
                      month = (month(pickup_datetime)),
                      year = (year(pickup_datetime)),
                      #day = day(pickup_datetime),
                      dayOfWeek = (wday(pickup_datetime)),
                      hour = hour(pickup_datetime),
                      partOfDay = (round(hour * 2 / 10))
)

testData <- mutate(testData,
                   pickup_datetime = ymd_hms(pickup_datetime),
                   month = (month(pickup_datetime)),
                   year = (year(pickup_datetime)),
                   #day = day(pickup_datetime),
                   dayOfWeek = (wday(pickup_datetime)),
                   hour = hour(pickup_datetime),
                   partOfDay = (round(hour * 2 / 10))
)


# Calculating traveled distance
sampleTrain <- mutate(sampleTrain, dist = distHaversine(
  matrix(c(pickup_longitude, pickup_latitude), ncol = 2), 
  matrix(c(dropoff_longitude, dropoff_latitude), ncol = 2),  
  r=6378137) + 1)

testData <- mutate(testData, dist = distHaversine(
  matrix(c(pickup_longitude, pickup_latitude), ncol = 2), 
  matrix(c(dropoff_longitude, dropoff_latitude), ncol = 2),  
  r=6378137) + 1)

# drop unwanted columns
sampleTrain <- select(sampleTrain, -key, -pickup_datetime, -hour)

testDataKey <- testData$key
testData <- select(testData, -key, -pickup_datetime, -hour)

# Log10 on fare_amount
sampleTrain <- mutate(sampleTrain, fare_amount = log10(fare_amount))

# XGBoost
y <- sampleTrain[ ,1]

# Deviding data in to test and train
smp_size <- floor(0.85 * nrow(sampleTrain))
train_ind <- sample(seq_len(nrow(sampleTrain)), size = smp_size)

Xtrain <- sampleTrain[train_ind, ]
Xtest <- sampleTrain[-train_ind, ]
ytrain <- y[train_ind, ]
ytest <- y[-train_ind, ]

tic()
xgb <- xgboost(data = data.matrix(Xtrain[-1]), 
               label = data.matrix(ytrain),
               booster = 'gbtree',
               eta = 0.1, # step size of each boosting step
               nround = 800,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               tree_method = 'exact',
               max_depth = 20,
               subsample = 0.8,
               colsample_bytree = 0.8,
               callbacks = list(cb.early.stop(stopping_rounds = 3, verbose = TRUE))
)
toc()
# Checking model by predicting on out of sample data
y_pred <- as_tibble(predict(xgb, data.matrix(Xtest[-1])))


# Using root mean squared as error function
rmseXGB <- sqrt(sum((y_pred - ytest)^2) / nrow(ytest))
print(rmseXGB)
rmseXGB <- sqrt(sum((10^y_pred - 10^ytest)^2) / nrow(ytest))
print(rmseXGB)

# Predicting on testDataOneHot
test_pred <- predict(xgb, data.matrix(testData))


submission <- bind_cols(as_tibble(testDataKey), as_tibble(10^test_pred)) %>%
  rename(key = value, fare_amount = value1)

write.csv(submission, file = "submission9.csv",row.names=FALSE, quote = FALSE)

