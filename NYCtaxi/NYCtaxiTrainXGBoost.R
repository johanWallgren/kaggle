library(data.table)
library(tidyverse)
library(lubridate)
library(caret)
library(tictoc)
library(flexclust)
library(geosphere)
library(xgboost)
library(Matrix)

# n = 1e5, ntree = 5 -> RMSE = 4.68
# n = 1e5, ntree = 10 -> RMSE = 4.53
# n = 1e5, ntree = 15 -> RMSE = 4.53



# allData <- as_tibble(fread("train.csv"))
# save(allData, file = "trainData.RData")
# load('trainData.RData')
# sampleTrain <- sample_n(allData, 1e5, replace = FALSE)
# rm(allData)
# save(sampleTrain, file = "sampleTrain.RData")
load('sampleTrain.Rdata')

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


# Creating clusters for pickup location
pickup_geoData <- select(sampleTrain,pickup_longitude, pickup_latitude)
pickup_clusters <- flexclust::kcca(pickup_geoData, k = 15, kccaFamily("kmeans"))
pickup_geoData$pickup_cluster <- as.factor(pickup_clusters@cluster)

pickup_geoDataPlot <- ggplot(pickup_geoData,aes(pickup_longitude, pickup_latitude, color = pickup_cluster))
pickup_geoDataPlot + geom_point(shape = 16, size = 0.2) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

sampleTrain$pickup_geoCluster <- pickup_geoData$pickup_cluster

pickup_geoData_test <- select(testData, pickup_longitude, pickup_latitude)
testData$pickup_geoCluster <- as.factor(flexclust::predict(pickup_clusters, newdata = pickup_geoData_test))


# Creating clusters for dropoff location
dropoff_geoData <- select(sampleTrain, dropoff_longitude, dropoff_latitude)
dropoff_clusters <- flexclust::kcca(dropoff_geoData, k = 15, kccaFamily("kmeans"))
dropoff_geoData$dropoff_cluster <- as.factor(dropoff_clusters@cluster)

dropoff_geoDataPlot <- ggplot(dropoff_geoData,aes(dropoff_longitude, dropoff_latitude, color = dropoff_cluster))
dropoff_geoDataPlot + geom_point(shape = 16, size = 0.2) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

sampleTrain$dropoff_geoCluster <- dropoff_geoData$dropoff_cluster

dropoff_geoData_test <- select(testData, dropoff_longitude, dropoff_latitude)
testData$dropoff_geoCluster <- as.factor(flexclust::predict(dropoff_clusters, newdata = dropoff_geoData_test))

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

# Creating onehot vectors for factors
sampleTrainOneHot <- as_tibble(as.matrix(sparse.model.matrix(fare_amount ~ .-1, data = sampleTrain)))

testData <- mutate(testData, fare_amount = 1)
testDataOneHot <- as_tibble(as.matrix(sparse.model.matrix(fare_amount ~ .-1, data = testData)))

y <- sampleTrain[ ,1]

# Deviding data in to test and train
smp_size <- floor(0.85 * nrow(sampleTrainOneHot))
train_ind <- sample(seq_len(nrow(sampleTrainOneHot)), size = smp_size)

Xtrain <- sampleTrainOneHot[train_ind, ]
Xtest <- sampleTrainOneHot[-train_ind, ]
ytrain <- y[train_ind, ]
ytest <- y[-train_ind, ]

xgb <- xgboost(data = data.matrix(Xtrain), 
               label = data.matrix(ytrain), 
               eta = 0.05, # step size of each boosting step
               max_depth = 25, # max depth of tree
               nround = 400, 
               eval_metric = "rmse",
               objective = "reg:linear"
)

# Checking model by predicting on out of sample data
y_pred <- as_tibble(predict(xgb, data.matrix(Xtest)))


# Using root mean squared as error function
rmseRF <- sqrt(sum((10^y_pred - 10^ytest)^2) / nrow(ytest))
print(rmseRF)

# Predicting on testDataOneHot
test_pred <- predict(xgb, data.matrix(testDataOneHot))


submission <- bind_cols(as_tibble(testDataKey), as_tibble(10^test_pred)) %>%
  rename(key = value, fare_amount = value1)

# write.csv(submission, file = "submission5.csv",row.names=FALSE, quote = FALSE)

