library(data.table)
library(tidyverse)
library(tictoc)
library(rjson)
library(xgboost)
library(Metrics)

#######################################################################
# Function def

fixLevels <- function(df1, df2){
  # Making sure that the levels are the same in train and test
  # Assuming that colnames are same for df1 and df2
  for(i in 1:dim(df2)[2]){
    if(class(df1[[i]]) == 'factor'){
      allLevels <- unique(c(levels(df1[[i]]),  levels(df2[[i]])))
      levels(df1[[i]]) <- allLevels
      levels(df2[[i]]) <- allLevels
    }
  }
  return(list(df1, df2))
}

# Function from https://www.kaggle.com/jaseziv83/pet-finder-xgb
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- ScoreQuadraticWeightedKappa(labels,round(preds))
  return(list(metric = "kappa", value = err))}

#######################################################################
# Load data

train <- as_tibble(fread("train.csv"))
test <- as_tibble(fread('test.csv'))

# train <- as_tibble(fread("../input/train/train.csv"))
# test <- as_tibble(fread('../input/test/test.csv'))

print('Data loaded')

#######################################################################
# Data prep

# Drop columns: 
# -RescuerID, -Description
train <- select(train, -RescuerID, -Description)
test <- select(test, -RescuerID, -Description)

# Convert to factor: 
# Type, Breed1, Breed2, Breed3, Gender, Color1, Color2, Vaccinated, Dewormed, Sterilized, Health 
# AdoptionSpeed

# As interger:
# Age, MaturitySize, FurLength, Quantity, PetID, PhotoAmt, VideoAmt

# As character:
# PetID
factorCols <- c("Type", "Breed1", "Breed2", "Gender",
                "Color1", "Color2", "Color3", "Vaccinated", "Dewormed",
                "Sterilized", "Health", "State")

train[factorCols] <- lapply(train[factorCols], factor)
train <- mutate(train, PhotoAmt = as.integer(PhotoAmt))
train <- mutate(train, AdoptionSpeed = as.factor(AdoptionSpeed))

test[factorCols] <- lapply(test[factorCols], factor)
test <- mutate(test, PhotoAmt = as.integer(PhotoAmt))


# Adding meta data
# allFilesMetaTest <- list.files('test_metadata/')
# allFilesMetaTrain <- list.files('train_metadata/')

# Adding sentement analysis
# allFilesSentTest <- list.files('test_sentiment/')
# allFilesSentTrain <- list.files('train_sentiment/')

# Making sure that the levels are the same in train and test
trainAndTest <- fixLevels(train, test)
# save(trainAndTest, file = "trainAndTest.RData")
# load("trainAndTest.RData")

#######################################################################
# EDA for train
train <- trainAndTest[[1]]
test <- trainAndTest[[2]]

# Number of colors
train <- mutate(train, nColors = ifelse(Color2 == 0, 1, 
                ifelse(Color3 == 0, 2, 3)))

rateNColor <- ggplot(train, aes(as.factor(nColors), fill = AdoptionSpeed))
rateNColor + geom_bar(position = 'fill')

# Is pure breed
train <- mutate(train, pureBreed = as.factor(ifelse(Breed2 == 0, 1, 0)))

ratePureBreed <- ggplot(train, aes(AdoptionSpeed, fill = pureBreed))
ratePureBreed + geom_bar(position = 'fill')

# Has name
train <- mutate(train, hasName = as.factor(ifelse(Name != '', 1, 0))) 

rateHasName <- ggplot(train, aes(AdoptionSpeed, fill = hasName))
rateHasName + geom_bar(position = 'fill')

# Helth
rateHealth <- ggplot(train, aes(AdoptionSpeed, fill = Health))
rateHealth + geom_bar(position = 'fill')

#######################
# EDA for test

# Number of colors
test <- mutate(test, nColors = ifelse(Color2 == 0, 1, 
                                        ifelse(Color3 == 0, 2, 3)))

# Is pure breed
test <- mutate(test, pureBreed = as.factor(ifelse(Breed2 == 0, 1, 0)))

# Has name
test <- mutate(test, hasName = as.factor(ifelse(Name != '', 1, 0)))

#######################################################################
# Splitting train

train <- select(train, -PetID, -State, -Breed1, -Breed2, -Name)

testPetID <- select(test, PetID)
test <- select(test, -PetID, -State, -Breed1, -Breed2, -Name)

smp_size <- floor(0.95 * nrow(train))

train_ind <- sample(seq_len(nrow(train)), size = smp_size)

trainTrain <- train[train_ind, ]
testTrain <- train[-train_ind, ]


# xgboost
tic()
xgb <- xgboost(data = data.matrix(trainTrain[-17]), 
               label = data.matrix(trainTrain[17])-1,
               eta = 0.1, # step size of each boosting step
               nround = 200,
               eval_metric = evalerror,
               objective = 'multi:softmax',
               num_class = 6,
               tree_method = 'exact',
               max_depth = 5,
               subsample = 0.8,
               colsample_bytree = 0.8,
               gamma = 1, 
               maximize = TRUE
)
toc()

# Checking model by predicting on out of sample data
predictTrainTest <- as_tibble(predict(xgb, data.matrix(testTrain[-17]))) 
  
ScoreQuadraticWeightedKappa(predictTrainTest$value, testTrain$AdoptionSpeed)
  
# importanceRF <- bind_cols(as_tibble(rownames(as.data.frame(fitRF$importance[ ,7]))), 
#                     as_tibble((as.data.frame(fitRF$importance[ ,7])))) %>%
#   rename(predictor = value, MeanDecreaseGini = `fitRF$importance[, 7]`) %>%
#   arrange(by = desc(MeanDecreaseGini)) %>%
#   mutate(predictor = as.factor(predictor))
# 
# importanceRF$predictor <- factor(importanceRF$predictor, importanceRF$predictor) 
# 
# impPlot <- ggplot(importanceRF, aes(as.factor(predictor), MeanDecreaseGini))
# impPlot + geom_bar(stat = 'Identity') +
#   theme(axis.text.x=element_text(angle=45,hjust=1))

predictTest <- predict(xgb, data.matrix(test))

#######################################################################
# Create submission
sub <- bind_cols(testPetID, as_tibble(predictTest))%>%
  rename(AdoptionSpeed = value)


write.csv(sub, file = "submission.csv",row.names=FALSE, quote = FALSE)


