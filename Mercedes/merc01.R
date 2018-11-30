library(data.table)
library(tidyverse)
library(randomForest)
library(tictoc)

convertCharToFactor <- function(df){
  # Convert char cols to factors
  for (i in 1:dim(df)[2]) {
    if(class(df[[i]]) == "character"){
      df[[i]] = as.factor(df[[i]])
    }
  }
  return(df)
}

findColsFactor <- function(df){
  # Convert char cols to factors
  listWithCols <- c()
  namesInDf <- names(df)
  
  for (i in 1:dim(df)[2]) {
    if(class(df[[i]]) == "factor"){
      listWithCols <- c(listWithCols, i)
    }
  }
  return(listWithCols)
}

completeLevels <- function(col1, col2){
  levels1 <- levels(col1)
  levels2 <- levels(col2)
  allLevels <- unique(c(levels1, levels2))
  
  levels(col1) <- allLevels
  levels(col2) <- allLevels
  
  return(list(col1, col2))
}

trainData <- convertCharToFactor(as_tibble(fread("train.csv")))
predData <- convertCharToFactor(as_tibble(fread("test.csv")))

colswithFactorsTrain <- findColsFactor(trainData)
colswithFactorsPred <- findColsFactor(predData)

for(i in 1:length(colswithFactorsTrain)){
  temp <- completeLevels(trainData[[colswithFactorsTrain[i]]], predData[[colswithFactorsPred[i]]])
  trainData[[colswithFactorsTrain[i]]] <- temp[[1]]
  predData[[colswithFactorsPred[i]]] <- temp[[2]]
}


# Drop ID, save ID for predData
trainData <- select(trainData, -ID)
predID <- predData$ID
predData <- select(predData, -ID)

# Devid into test and train
smp_size <- floor(0.90 * nrow(trainData))

set.seed(123)
train_ind <- sample(seq_len(nrow(trainData)), size = smp_size)

train <- trainData[train_ind, ]
test <- trainData[-train_ind, ]

tic()
fitRF <- randomForest(formula = y ~ ., data = train, 
                      ntree = 150, 
                      mtry = dim(train)[2] / 2, 
                      nodesize = 5, 
                      importance = TRUE)
toc()

# Checking model by predicting on out of sample data
predictTest <- predict(fitRF, test)

# Using root mean squared as error function
rmseRF <- sqrt(sum((predictTest - test$y)^2) / nrow(test))
print(rmseRF)

predictPredictData <- predict(fitRF, predData)


submission <- bind_cols(as_tibble(predID), as_tibble(predictPredictData)) %>%
  rename(ID = value, y = value1)

write.csv(submission, file = "submission01.csv",row.names=FALSE, quote = FALSE)



