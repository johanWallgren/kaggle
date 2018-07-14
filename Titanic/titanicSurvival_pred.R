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

allData <- as_tibble(read.csv('test.csv', sep = ',')) 

td <- separate(allData, Name, c('lastName', 'restName'), sep = ', ', remove = TRUE) %>%
  separate(restName, c('titleName', 'trashName'), sep = '. ',  remove = TRUE) %>%
  mutate(lastName = as.factor(lastName)) %>%
  mutate(Age = ifelse(is.na(Age), round(mean(Age, na.rm = TRUE)), Age)) %>% 
  mutate(Embarked = as.character(Embarked)) %>%
  mutate(Embarked = ifelse(Embarked == '', 'S', Embarked))

tdID <- select(allData, PassengerId)

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

td <- as.h2o(td)

pred <- h2o.predict(m1, td)
pred

entryData <- bind_cols(as_tibble(tdID$PassengerId), as_tibble(pred$predict)) %>%
  rename(PassengerId = value, Survived = predict) %>%
  mutate(Survived = as.integer(as.character(Survived)))

write.csv(entryData, file = "entryData.csv",row.names=FALSE, quote = FALSE)

