library(data.table)
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(tictoc)
library(flexclust)

# nTree=10, n=1e5 -> 617s, RMSE = 3.932298
# nTree=15, n=1e5 -> 913s, RMSE = 3.927082
# nTree=15, n=1e6 -> 203481s, RMSE = 2.949493

allData <- as_tibble(fread("train.csv"))

# save(allData, file = "trainData.RData")
load('trainData.RData')
sampleTrain <- sample_n(allData, 1e6, replace = FALSE)
rm(allData)
save(sampleTrain, file = "sampleTrain.RData")
load('sampleTrain.Rdata')

testData <- as_tibble(read.csv('test.csv', sep = ','))

# filter out outliers

sampleTrain <- filter(sampleTrain,
                        pickup_longitude < -72 &
                        pickup_longitude > -75 &
                        pickup_latitude > 40.2 &
                        pickup_latitude < 42 &
                        dropoff_longitude < -72 &
                        dropoff_longitude > -75 &
                        dropoff_latitude > 40 &
                        dropoff_latitude < 42 &
                        fare_amount >= 0 &
                        fare_amount <= 500 &
                        passenger_count < 10)


# Splitting up pickup_datetime 
sampleTrain <- mutate(sampleTrain,
             pickup_datetime = ymd_hms(pickup_datetime),
             month = as.factor(month(pickup_datetime)),
             year = as.factor(year(pickup_datetime)),
             #day = day(pickup_datetime),
             dayOfWeek = as.factor(wday(pickup_datetime)),
             hour = hour(pickup_datetime),
             partOfDay = as.factor(round(hour * 2 / 10)),
             hour = as.factor(hour(pickup_datetime))
)

testData <- mutate(testData,
             pickup_datetime = ymd_hms(pickup_datetime),
             month = as.factor(month(pickup_datetime)),
             year = as.factor(year(pickup_datetime)),
             #day = day(pickup_datetime),
             dayOfWeek = as.factor(wday(pickup_datetime)),
             hour = hour(pickup_datetime),
             partOfDay = as.factor(round(hour * 2 / 10)),
             hour = as.factor(hour(pickup_datetime))
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

# Calculating manhattan distance
sampleTrain <- mutate(sampleTrain, manhattanDist = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude))
testData <- mutate(testData, manhattanDist = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude))

# drop unwanted columns
sampleTrain <- select(sampleTrain, -key, -pickup_datetime, -hour)

testDataKey <- testData$key
testData <- select(testData, -key, -pickup_datetime, -hour)


# Random Forrest

# Deviding data in to test and train
forTrain <- createDataPartition(y = sampleTrain$fare_amount, p = 0.95, list = FALSE)
sampleTrainTrain <- sampleTrain[forTrain,]
sampleTrainTest <- sampleTrain[-forTrain,]


# Defult ntree = 500.
# Finding a better mtry than standard p/3 takes a long time, will use defult.
tic()
fitRF <- randomForest(formula = fare_amount ~ ., data = sampleTrainTrain, ntree = 15)
toc()

# Checking model by predicting on out of sample data
predictRF <- predict(fitRF, sampleTrainTest)

# Using root mean squared as error function
rmseRF <- sqrt(sum((predictRF - sampleTrainTest$fare_amount)^2) / nrow(sampleTrainTest))
print(rmseRF)


# Completing levels in testData
levels(testData$month) <- levels(sampleTrain$month)
levels(testData$year) <- levels(sampleTrain$year)
levels(testData$dayOfWeek) <- levels(sampleTrain$dayOfWeek)
levels(testData$partOfDay) <- levels(sampleTrain$partOfDay)
levels(testData$pickup_geoCluster) <- levels(sampleTrain$pickup_geoCluster)
levels(testData$dropoff_geoCluster) <- levels(sampleTrain$dropoff_geoCluster)

# Predicting on testData
predictTestData <- predict(fitRF, testData)


submission <- bind_cols(as_tibble(testDataKey), as_tibble(predictTestData)) %>%
  rename(key = value, fare_amount = value1)

write.csv(submission, file = "submission3.csv",row.names=FALSE, quote = FALSE)

# Score: 3.26507
