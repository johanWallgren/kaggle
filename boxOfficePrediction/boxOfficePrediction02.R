library(data.table)
library(tidyverse)
library(stringi)
library(lubridate)
library(randomForest)
library(tictoc)

train_raw = read_csv("train.csv") 
test_raw = read_csv("test.csv")

mBudgetTrain <- median(train_raw$budget, na.rm = TRUE)
mRuntimeTrain <- median(train_raw$runtime, na.rm = TRUE)

train <- train_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE) %>%
  mutate(collectionID = ifelse(is.na(idPart) == FALSE, gsub("\\D", "", idPart), idPart),
         collectionID = ifelse(is.na(collectionID) == TRUE, 0, collectionID),
         mainSpokenLanguage = substr(spoken_languages,17,18),
         mainSpokenLanguage = ifelse(is.na(mainSpokenLanguage), 'en', mainSpokenLanguage),
         spokenEn = ifelse(mainSpokenLanguage == 'en', TRUE, FALSE),
         partOfCollection = ifelse(is.na(idPart) == FALSE, TRUE, FALSE),
         hasHomePage = ifelse(is.na(homepage) == TRUE, TRUE, FALSE),
         hasTagline = ifelse(is.na(tagline) == TRUE, TRUE, FALSE),
         genres = ifelse(is.na(genres) == TRUE, 'NoGen', genres),
         genComedy = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genDrama = ifelse(stri_detect_fixed(genres, 'Drama'),TRUE, FALSE),
         genThriller = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genAction = ifelse(stri_detect_fixed(genres, 'Action'),TRUE, FALSE),
         genAnimation = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genHorror = ifelse(stri_detect_fixed(genres, 'Horror'),TRUE, FALSE),
         genDocumentary = ifelse(stri_detect_fixed(genres, 'Documentary'),TRUE, FALSE),
         genAdventure = ifelse(stri_detect_fixed(genres, 'Adventure'),TRUE, FALSE),
         genCrime = ifelse(stri_detect_fixed(genres, 'Crime'),TRUE, FALSE),
         genMystery = ifelse(stri_detect_fixed(genres, 'Mystery'),TRUE, FALSE),
         genFantasy = ifelse(stri_detect_fixed(genres, 'Fantasy'),TRUE, FALSE),
         genWar = ifelse(stri_detect_fixed(genres, 'War'),TRUE, FALSE),
         genScienceFiction = ifelse(stri_detect_fixed(genres, 'Science Fiction'),TRUE, FALSE),
         genRomance = ifelse(stri_detect_fixed(genres, 'Romance'),TRUE, FALSE),
         genMusic = ifelse(stri_detect_fixed(genres, 'Music'),TRUE, FALSE),
         genWestern = ifelse(stri_detect_fixed(genres, 'Western'),TRUE, FALSE),
         genFamily = ifelse(stri_detect_fixed(genres, 'Family'),TRUE, FALSE),
         genHistory = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genForeign = ifelse(stri_detect_fixed(genres, 'Foreign'),TRUE, FALSE),
         genTVMovie = ifelse(stri_detect_fixed(genres, 'TV Movie'),TRUE, FALSE),
         releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)),
         release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')),
         age = as.integer(today() - release_date) / 365,
         quarterRelease = quarter(release_date),
         weekRelease = week(release_date),
         dayRelease = wday(release_date),
         budget = ifelse(budget < 1000, mBudgetTrain, budget),
         runtime = ifelse(is.na(runtime) == TRUE, mRuntimeTrain, runtime),
         sizeOfCast = str_count(cast, 'cast_id'),
         sizeOfCrew = str_count(crew, 'name'),
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         numberOfKeywords = str_count(Keywords, 'name'),
         numberOfKeywords = ifelse(is.na(numberOfKeywords) == TRUE, 0, numberOfKeywords),
         numberOfProductionCompanies = str_count(production_companies, 'name'),
         numberOfProductionCompanies = ifelse(is.na(numberOfProductionCompanies) == TRUE, 0, numberOfProductionCompanies),
         numberOfProductionCountries = str_count(production_countries, 'name'),
         numberOfProductionCountries = ifelse(is.na(numberOfProductionCountries) == TRUE, 0, numberOfProductionCountries),
         numberOfGenres = str_count(genres, 'name'),
         collectionID = as.factor(collectionID)) %>%  
  group_by(collectionID) %>%
  mutate(sizeOfCollection = n()) %>%
  ungroup() %>%
  mutate(sizeOfCollection = ifelse(sizeOfCollection > 1000, 0, sizeOfCollection)) %>%
  select(-idPart, -homepage, -imdb_id, -poster_path, -original_title, -genres, -overview, 
         -tagline, -production_companies, -spoken_languages, -cast, -crew, -Keywords, 
         -production_countries, -status, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID, -mainSpokenLanguage)


medianReleaseDate <- median(train$release_date, na.rm = TRUE)
train <- select(train, -release_date)

mBudgetTest <- median(test_raw$budget, na.rm = TRUE)
mRuntimeTest <- median(test_raw$runtime, na.rm = TRUE)

test <- test_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE) %>%
  mutate(collectionID = ifelse(is.na(idPart) == FALSE, gsub("\\D", "", idPart), idPart),
         collectionID = ifelse(is.na(collectionID) == TRUE, 0, collectionID),
         mainSpokenLanguage = substr(spoken_languages,17,18),
         mainSpokenLanguage = ifelse(is.na(mainSpokenLanguage), 'en', mainSpokenLanguage),
         spokenEn = ifelse(mainSpokenLanguage == 'en', TRUE, FALSE),
         partOfCollection = ifelse(is.na(idPart) == FALSE, TRUE, FALSE),
         hasHomePage = ifelse(is.na(homepage) == TRUE, TRUE, FALSE),
         hasTagline = ifelse(is.na(tagline) == TRUE, TRUE, FALSE),
         genres = ifelse(is.na(genres) == TRUE, 'NoGen', genres),
         genComedy = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genDrama = ifelse(stri_detect_fixed(genres, 'Drama'),TRUE, FALSE),
         genThriller = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genAction = ifelse(stri_detect_fixed(genres, 'Action'),TRUE, FALSE),
         genAnimation = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genHorror = ifelse(stri_detect_fixed(genres, 'Horror'),TRUE, FALSE),
         genDocumentary = ifelse(stri_detect_fixed(genres, 'Documentary'),TRUE, FALSE),
         genAdventure = ifelse(stri_detect_fixed(genres, 'Adventure'),TRUE, FALSE),
         genCrime = ifelse(stri_detect_fixed(genres, 'Crime'),TRUE, FALSE),
         genMystery = ifelse(stri_detect_fixed(genres, 'Mystery'),TRUE, FALSE),
         genFantasy = ifelse(stri_detect_fixed(genres, 'Fantasy'),TRUE, FALSE),
         genWar = ifelse(stri_detect_fixed(genres, 'War'),TRUE, FALSE),
         genScienceFiction = ifelse(stri_detect_fixed(genres, 'Science Fiction'),TRUE, FALSE),
         genRomance = ifelse(stri_detect_fixed(genres, 'Romance'),TRUE, FALSE),
         genMusic = ifelse(stri_detect_fixed(genres, 'Music'),TRUE, FALSE),
         genWestern = ifelse(stri_detect_fixed(genres, 'Western'),TRUE, FALSE),
         genFamily = ifelse(stri_detect_fixed(genres, 'Family'),TRUE, FALSE),
         genHistory = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genForeign = ifelse(stri_detect_fixed(genres, 'Foreign'),TRUE, FALSE),
         genTVMovie = ifelse(stri_detect_fixed(genres, 'TV Movie'),TRUE, FALSE),
         releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)),
         release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')),
         release_date = if_else(is.na(release_date) == TRUE, medianReleaseDate, release_date),
         age = as.integer(today() - release_date) / 365,
         quarterRelease = quarter(release_date),
         weekRelease = week(release_date),
         dayRelease = wday(release_date),
         budget = ifelse(budget < 1000, mBudgetTest, budget),
         runtime = ifelse(is.na(runtime) == TRUE, mRuntimeTest, runtime),
         sizeOfCast = str_count(cast, 'cast_id'),
         sizeOfCrew = str_count(crew, 'name'),
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         numberOfKeywords = str_count(Keywords, 'name'),
         numberOfKeywords = ifelse(is.na(numberOfKeywords) == TRUE, 0, numberOfKeywords),
         numberOfProductionCompanies = str_count(production_companies, 'name'),
         numberOfProductionCompanies = ifelse(is.na(numberOfProductionCompanies) == TRUE, 0, numberOfProductionCompanies),
         numberOfProductionCountries = str_count(production_countries, 'name'),
         numberOfProductionCountries = ifelse(is.na(numberOfProductionCountries) == TRUE, 0, numberOfProductionCountries),
         numberOfGenres = str_count(genres, 'name'),
         collectionID = as.factor(collectionID)) %>%  
  group_by(collectionID) %>%
  mutate(sizeOfCollection = n()) %>%
  ungroup() %>%
  mutate(sizeOfCollection = ifelse(sizeOfCollection > 1000, 0, sizeOfCollection)) %>%
  select(-idPart, -homepage, -imdb_id, -poster_path, -original_title, -genres, -overview, 
         -tagline, -production_companies, -spoken_languages, -cast, -crew, -Keywords, 
         -production_countries, -status, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID, -release_date, -mainSpokenLanguage)

###################################
# Log on all variables that have a log-normal dist

train <- mutate(train, 
                budget = log10(budget + 1),
                runtime = log10(runtime + 1),
                popularity = log10(popularity + 1),
                sizeOfCrew = log10(sizeOfCrew + 1),
                sizeOfCast = log10(sizeOfCast + 1),
                revenue = log10(revenue + 1))

test <- mutate(test,
               budget = log10(budget + 1),
               runtime = log10(runtime + 1),
               sizeOfCrew = log10(sizeOfCrew + 1),
               sizeOfCast = log10(sizeOfCast + 1),
               popularity = log10(popularity + 1))

###################################
# Fix levels

languageAll <- bind_rows(select(train, original_language) %>%
                           mutate(testOrTrain = 'train'), select(test, original_language) %>%
                           mutate(testOrTrain = 'test')) %>% 
  mutate(original_language = as.factor(original_language))

train <- select(train, -original_language) %>%
  bind_cols(filter(languageAll, testOrTrain == 'train')) %>%
  select(-testOrTrain)


test <- select(test, -original_language) %>%
  bind_cols(filter(languageAll, testOrTrain == 'test')) %>%
  select(-testOrTrain)

###################################
train <- select(train, -id)

testID <- select(test, id)
test <- select(test, -id)

smp_size <- floor(0.70 * nrow(train))

train_ind <- sample(seq_len(nrow(train)), size = smp_size)

trainTrain <- train[train_ind, ]
testTrain <- train[-train_ind, ]


##################################################
# Find "best" parameters for RF
# 
# mtryList <- c(4,8,16,32)
# nodeSizeList <- c(4,8,16,32)
# 
# rfList <- as_tibble(expand.grid(mtryList, nodeSizeList)) %>%
#   rename(mtry = Var1, nodeSize = Var2) %>%
#   mutate(rmsle = 0)
# 
# for(i in 1:dim(rfList)[1]){
# 
#   fitRF <- randomForest(formula = revenue ~ ., data = trainTrain,
#                         ntree = 500,
#                         mtry = rfList[i, ]$mtry,
#                         nodesize = rfList[i, ]$nodeSize,
#                         importance = FALSE)
# 
#   predictTestTrain <- predict(fitRF, testTrain)
# 
#   rfList[i, ]$rmsle <- sqrt(sum((log(10^predictTestTrain + 1) - log(10^testTrain$revenue + 1))^2)
#                  / nrow(testTrain))
# 
#   print(head(rfList, i))
# }
# 
# rfList %>%
#   arrange(rmsle)


##################################################
# Train using "best" parameters
# Calculate rmsle
# Plot importance

tic()
fitRF <- randomForest(formula = revenue ~ ., data = trainTrain, 
                      ntree = 2000,
                      mtry = 8,
                      nodesize = 4, 
                      importance = TRUE)
toc()

# Checking model by predicting on out of sample data
predictTestTrain <- predict(fitRF, testTrain)


rmseRF <- sqrt(sum((log(10^predictTestTrain + 1) - log(10^testTrain$revenue + 1))^2)
               / nrow(testTrain))

print(rmseRF)


importanceRF <- bind_cols(as_tibble(rownames(as.data.frame(fitRF$importance[ ,2]))), 
                          as_tibble((as.data.frame(fitRF$importance[ ,2])))) %>%
  rename(predictor = value, RMSE = `fitRF$importance[, 2]`) %>%
  arrange(by = desc(RMSE)) %>%
  mutate(predictor = as.factor(predictor))

importanceRF$predictor <- factor(importanceRF$predictor, importanceRF$predictor) 

impPlot <- ggplot(importanceRF, aes(as.factor(predictor), RMSE))
impPlot + geom_bar(stat = 'Identity') +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(y = 'rmsle', x = 'predictor')


##########################################################################
# Re run using all of training data
# The predict on test and create submission

tic()
fitRF <- randomForest(formula = revenue ~ ., data = train, 
                      ntree = 2000,
                      mtry = 8,
                      nodesize = 4, 
                      importance = TRUE)
toc()



predictTest <- predict(fitRF, test)

predictTest <- as_tibble(predictTest) %>% 
  mutate(value = 10^value)

#######################################################################
# Create submission
sub <- bind_cols(testID, predictTest)%>%
  rename(revenue = value) %>%
  mutate(revenue = round(revenue))

write.csv(sub, file = "submission.csv",row.names=FALSE, quote = FALSE)

