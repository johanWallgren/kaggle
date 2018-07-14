library(tidyverse)
library(h2o)

# Variable	  Definition	                 Key
# survival	  Survival	                   0 = No, 1 = Yes
# pclass	    Ticket class                 1 = 1st, 2 = 2nd, 3 = 3rd
# sex	        Sex	
# Age	        Age in years	
# sibsp	      # of siblings / spouses aboard the Titanic	
# parch	      # of parents / children aboard the Titanic	
# ticket	    Ticket number	
# fare	      Passenger fare	
# cabin	      Cabin number	
# embarked	  Port of Embarkation	         C = Cherbourg, Q = Queenstown, S = Southampton

allData <- as_tibble(read.csv('train.csv', sep = ',')) 

td <- separate(allData, Name, c('lastName', 'restName'), sep = ', ', remove = TRUE) %>%
  separate(restName, c('titleName', 'trashName'), sep = '. ',  remove = TRUE) %>%
  mutate(lastName = as.factor(lastName)) %>%
  select(-trashName, -PassengerId) %>%
  mutate(Survived = as.factor(Survived)) %>% 
  mutate(Age = ifelse(is.na(Age), round(mean(Age, na.rm = TRUE)), Age)) %>% 
  mutate(Embarked = as.character(Embarked)) %>%
  mutate(Embarked = ifelse(Embarked == '', 'S', Embarked)) %>%
  mutate(Embarked = as.factor(Embarked))

td$titleName[td$titleName == 'Mlle']  <- 'Miss' 
td$titleName[td$titleName == 'Ms'] <- 'Miss'
td$titleName[td$titleName == 'Mme'] <- 'Mrs' 
td$titleName[td$titleName == 'Lady'] <- 'Miss'
td$titleName[td$titleName == 'Dona'] <- 'Miss'

td$titleName[td$titleName == 'Capt'] <- 'Military'
td$titleName[td$titleName == 'Col'] <- 'Military'
td$titleName[td$titleName == 'Don'] <- 'Mr'
td$titleName[td$titleName == 'Jonkheer'] <- 'Military'
td$titleName[td$titleName == 'Major'] <- 'Military'
td$titleName[td$titleName == 'Sir'] <- 'Mr'
td$titleName[td$titleName == 'th'] <- 'Mr'

td <- mutate(td, titleName = as.factor(titleName))

td <- mutate(td, famSize = SibSp + Parch + 1)
td$famSize[class(td$famSize) == 'numeric' & td$famSize > 5]  <- 'Large'
td$famSize[td$famSize == 1]  <- 'Small'
td$famSize[td$famSize > 1 & td$famSize <=  5]  <- 'Medium'
td <- mutate(td, famSize = as.factor(famSize))

# extractDeck <- function(Cabin){
#   if (Cabin == ''){
#     deck <- 'NA'
#   }else{
#     deck <- substring(as.character(Cabin), 1, 1)
#   }
#   return(deck)
# }
# 
# td$deck <- 'NA'
# for (i in 1:dim(td)[1]){
#   td[i, ]$deck <- extractDeck(td[i, ]$Cabin)
# }
# 
# td <- mutate(td, deck = as.factor(deck))


# NN training

predict <- 'Survived'
variables <- setdiff(names(td), predict) 

h2o.init(nthreads=-1, max_mem_size="1G")
h2o.removeAll()

td <- as.h2o(td)

splits <- h2o.splitFrame(td, c(0.7,0.15), seed=1234)
train  <- h2o.assign(splits[[1]], "train.hex") # 60%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
test   <- h2o.assign(splits[[3]], "test.hex")  # 20%

m1 <- h2o.deeplearning(
  model_id = "titanicDL-200-200-200-e10",
  training_frame = train,
  validation_frame = valid,
  x = variables,
  y = predict,
  activation = "Rectifier",
  hidden = c(220,220,220),
  epochs = 15,
  variable_importances = TRUE)


summary(m1)

pred <- h2o.predict(m1, test)
pred
test$Accuracy <- pred$predict == test$Survived
mean(test$Accuracy)

# http://localhost:54321/flow/index.html
