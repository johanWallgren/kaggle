library(data.table)
library(tidyverse)
library(lubridate)
library(tictoc)
library(geosphere)
library(keras)

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


## NN


# Devide into train and test, extracting fare_amount
forTrain <- sample(1:nrow(sampleTrain), floor(nrow(sampleTrain) * 0.75), replace=FALSE)

trainData <- sampleTrain[forTrain,-1]
trainDataLabels <- sampleTrain[forTrain,1]

testTrainData <- sampleTrain[-forTrain,-1]
testTrainDataLabels <- sampleTrain[-forTrain,1]


# Normalizing data
trainData <- scale(trainData)
testTrainData <- scale(testTrainData)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(trainData)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()











# submission <- bind_cols(as_tibble(testDataKey), as_tibble(10^test_pred)) %>%
#   rename(key = value, fare_amount = value1)
# 
# write.csv(submission, file = "submission7.csv",row.names=FALSE, quote = FALSE)










