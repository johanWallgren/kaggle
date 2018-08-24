library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(tictoc)
library(flexclust)

# allData <- as_tibble(read.csv('train.csv', sep = ','))
# save(allData, file = "trainData.RData")
load('trainData.RData')
sampleTrain <- sample_n(allData, 1e6, replace = FALSE)
rm(allData)
save(sampleTrain, file = "sampleTrain.RData")
# load('sampleTrain.Rdata')
testData <- as_tibble(read.csv('test.csv', sep = ','))

td <- sampleTrain

# filter out outliers

td <- filter(td,
             abs(pickup_longitude) <= 180 &
               abs(pickup_latitude) <= 90 &
               abs(dropoff_longitude) <= 180 &
               abs(dropoff_latitude) <= 90 &
               fare_amount >= 0 &
               fare_amount <= 500 &
               passenger_count < 10)


# Splitting up pickup_datetime 
td <- mutate(td,
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
pickup_geoData <- select(td,pickup_longitude, pickup_latitude)
pickup_clusters <- flexclust::kcca(pickup_geoData, k = 25, kccaFamily("kmeans"))
pickup_geoData$pickup_cluster <- as.factor(pickup_clusters@second)

pickup_geoDataPlot <- ggplot(pickup_geoData,aes(pickup_longitude, pickup_latitude, color = pickup_cluster))
pickup_geoDataPlot + geom_point(shape = 16, size = 3) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

td$pickup_geoCluster <- pickup_geoData$pickup_cluster

pickup_geoData_test <- select(testData, pickup_longitude, pickup_latitude)
testData$pickup_geoCluster <- as.factor(flexclust::predict(pickup_clusters, newdata = pickup_geoData_test))


# Creating clusters for dropoff location
dropoff_geoData <- select(td, dropoff_longitude, dropoff_latitude)
dropoff_clusters <- flexclust::kcca(dropoff_geoData, k = 25, kccaFamily("kmeans"))
dropoff_geoData$dropoff_cluster <- as.factor(dropoff_clusters@second)

dropoff_geoDataPlot <- ggplot(dropoff_geoData,aes(dropoff_longitude, dropoff_latitude, color = dropoff_cluster))
dropoff_geoDataPlot + geom_point(shape = 16, size = 3) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

td$dropoff_geoCluster <- dropoff_geoData$dropoff_cluster

dropoff_geoData_test <- select(testData, dropoff_longitude, dropoff_latitude)
testData$dropoff_geoCluster <- as.factor(flexclust::predict(dropoff_clusters, newdata = dropoff_geoData_test))


# drop unwanted columns
td <- select(td, -key, -pickup_datetime, -hour)

testDataKey <- testData$key
testData <- select(testData, -key, -pickup_datetime, -hour)



# Random Forrest

# Deviding data in to test and train
forTrain <- createDataPartition(y = td$fare_amount, p = 0.85, list = FALSE)
tdTrain <- td[forTrain,]
tdTest <- td[-forTrain,]


# Defult ntree = 500.
# Finding a better mtry than standard p/3 takes a long time, will use defult.
tic()
fitRF <- randomForest(formula = fare_amount ~ ., data = tdTrain, ntree = 10)
toc()

# Checking model by predicting on out of sample data
tdTest <- select(tdTest, -fare_amount)
predictRF <- predict(fitRF, tdTest)

# Using root mean squared as error function
rmseRF <- sqrt(sum((predictRF - tdTest$fare_amount)^2) / nrow(tdTest))
print(rmseRF)



# Completing levels in testData
levels(testData$month) <- levels(td$month)
levels(testData$year) <- levels(td$year)
levels(testData$dayOfWeek) <- levels(td$dayOfWeek)
levels(testData$partOfDay) <- levels(td$partOfDay)
levels(testData$pickup_geoCluster) <- levels(td$pickup_geoCluster)
levels(testData$dropoff_geoCluster) <- levels(td$dropoff_geoCluster)

# Predicting on testData
predictTestData <- predict(fitRF, testData)


submission <- bind_cols(as_tibble(testDataKey), as_tibble(predictTestData)) %>%
  rename(key = value, fare_amount = value1)

write.csv(submission, file = "submission.csv",row.names=FALSE, quote = FALSE)

# Simple approach to assigning clusters for new data after k-means clustering
# https://stackoverflow.com/questions/20621250/simple-approach-to-assigning-clusters-for-new-data-after-k-means-clustering











