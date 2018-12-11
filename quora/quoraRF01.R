library(data.table)
library(tidyverse)
library(tictoc)
library(tm)
library(tidytext)
library(stringr)
library(randomForest)
library(caret)

trainData <- as_tibble(fread("../input/train.csv")) %>%
  mutate(target = as.factor(target))

test <- as_tibble(fread('../input/test.csv'))

# Balancing the train data
trainDataTrue <- filter(trainData, target == 1)
trainDataFalse <- filter(trainData, target == 0)
trainDataTrueSample <- sample_n(trainDataFalse, nrow(trainDataTrue), replace = FALSE)
trainBalanced <- bind_rows(trainDataTrue, trainDataTrueSample)

# train <- sample_n(trainBalanced, 1e4, replace = FALSE)

train <- trainBalanced

getAllSents <- function(df){
  
  oneTrainWords <- df[1, ] %>%
    unnest_tokens(word, question_text)
  
  oneSent <- oneTrainWords %>% 
    inner_join(get_sentiments("nrc"), by = "word")
  
  allSents <- spread(as_tibble(table(oneSent$sentiment)), Var1, n)
  
  for(i in 2:dim(df)[1]){
    oneTrainWords <- df[i, ] %>%
      unnest_tokens(word, question_text) %>% 
      anti_join(stop_words, by = 'word')
    
    oneSent <- oneTrainWords %>% 
      inner_join(get_sentiments("nrc"), by = "word")
    
    if(nrow(oneSent) > 0){
      tmp <- spread(as_tibble(table(oneSent$sentiment)), Var1, n)
    } else {
      tmp = tibble(noSent = 1)
    }
    allSents <- bind_rows(allSents,tmp)
  }
  return(allSents)
}

allSentsTrain <- getAllSents(train)
train <- bind_cols(train, allSentsTrain)

allSentsTest <- getAllSents(test)
test <- bind_cols(test, allSentsTest)

question_text_train <- select(train, qid, question_text)

train <- select(train, -qid, -question_text) %>% 
  replace(is.na(.), 0)

question_text_test <- select(test, qid, question_text)

test <- select(test, -qid, -question_text) %>% 
  replace(is.na(.), 0)


forTrain <- createDataPartition(y = train$target, p = 0.95, list = FALSE)
trainTrain <- train[forTrain,]
trainTest <- train[-forTrain,]

tic()
fitRF <- randomForest(formula = target ~ ., data = trainTrain, ntree = 1000)
toc()

predictRF <- as_tibble(predict(fitRF, trainTest))

evalTrainTest = bind_cols(as_tibble(trainTest$target), predictRF) %>%
  mutate(target = as.integer(value), prediction = as.integer(value1), diff = abs(prediction - target)) %>%
  select(-value, -value1) %>%
  summarize(accuracy = (nrow(predictRF) - sum(diff)) / nrow(predictRF))

evalTrainTest

# Predicting on testData
predictTest <- predict(fitRF, test)

submission <- bind_cols(as_tibble(question_text_test$qid), as_tibble(predictTest)) %>%
  rename(qid = value, prediction = value1)

write.csv(x = test, file = 'submission.csv', row.names = FALSE, quote = FALSE)


