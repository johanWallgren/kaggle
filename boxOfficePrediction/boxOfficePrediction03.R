library(data.table)
library(tidyverse)
library(stringi)
library(lubridate)
library(randomForest)
library(tictoc)
library(scales)

theme_set(theme_light())

train_raw = read_csv("train.csv") 
test_raw = read_csv("test.csv")

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
         genNoGen = ifelse(genres == 'NoGen', TRUE, FALSE),
         production_companies = ifelse(is.na(production_companies) == TRUE, 'NoProd', production_companies),
         prodUniversal = ifelse(stri_detect_fixed(production_companies, 'Universal Pictures'),TRUE, FALSE),
         prodParamount = ifelse(stri_detect_fixed(production_companies, 'Paramount Pictures'),TRUE, FALSE),
         prodTCF = ifelse(stri_detect_fixed(production_companies, 'Twentieth Century Fox Film Corporation'),TRUE, FALSE),
         prodColumbia = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures'),TRUE, FALSE),
         prodWarner = ifelse(stri_detect_fixed(production_companies, 'Warner Bros.'),TRUE, FALSE),
         prodNLC = ifelse(stri_detect_fixed(production_companies, 'New Line Cinema'),TRUE, FALSE),
         prodDisney = ifelse(stri_detect_fixed(production_companies, 'Walt Disney Pictures'),TRUE, FALSE),
         prodColumbiaPictures = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures Corporation'),TRUE, FALSE),
         prodTriStar = ifelse(stri_detect_fixed(production_companies, 'TriStar Pictures'),TRUE, FALSE),
         prodMGM = ifelse(stri_detect_fixed(production_companies, 'Metro-Goldwyn-Mayer (MGM)'),TRUE, FALSE),
         prodUnitedArtists = ifelse(stri_detect_fixed(production_companies, 'United Artists'),TRUE, FALSE),
         prodMiramax = ifelse(stri_detect_fixed(production_companies, 'Miramax Films'),TRUE, FALSE),
         prodTouchstone = ifelse(stri_detect_fixed(production_companies, 'Touchstone Pictures  '),TRUE, FALSE),
         prodFoxSearchlight = ifelse(stri_detect_fixed(production_companies, 'Fox Searchlight Pictures'),TRUE, FALSE),
         releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)),
         release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')),
         age = as.integer(today() - release_date) / 365,
         quarterRelease = quarter(release_date),
         weekRelease = week(release_date),
         dayRelease = wday(release_date),
         runtime = ifelse(is.na(runtime) == TRUE, 0, runtime),
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

# Log on all variables that have a log-normal dist
train <- mutate(train,
                budget = log10(budget + 1), #2.209458
                sizeOfCollection = log10(sizeOfCollection + 1),
                age = log10(age),
                revenue = log10(revenue + 1))


# Fix movies without budget, runtime, sizeOfCast and sizeOfCrew
budgetNotZero <- filter(train, budget > 0 & sizeOfCrew > 0 & sizeOfCast > 0 
                        & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

runtimeNotZero <- filter(train, runtime > 0 & sizeOfCrew > 0 & sizeOfCast > 0 
                         & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

crewNotZero <- filter(train, sizeOfCrew > 0 & sizeOfCast > 0 
                      & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

castNotZero <- filter(train, sizeOfCrew > 0 & sizeOfCast > 0 
                      & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

lmMod <- lm(budget ~ sizeOfCast + sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = budgetNotZero)

train <- mutate(train, budget = ifelse(budget == 0, lmMod$coefficients[[1]] + 
                                         lmMod$coefficients[[2]] * sizeOfCast +
                                         lmMod$coefficients[[3]] * sizeOfCrew +
                                         lmMod$coefficients[[4]] * numberOfProductionCountries +
                                         lmMod$coefficients[[5]] * numberOfProductionCompanies +
                                         lmMod$coefficients[[6]] * age, budget))

lmMod <- lm(runtime ~ sizeOfCast + sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = runtimeNotZero)

train <- mutate(train, runtime = ifelse(runtime == 0, lmMod$coefficients[[1]] + 
                                          lmMod$coefficients[[2]] * sizeOfCast +
                                          lmMod$coefficients[[3]] * sizeOfCrew +
                                          lmMod$coefficients[[4]] * numberOfProductionCountries +
                                          lmMod$coefficients[[5]] * numberOfProductionCompanies +
                                          lmMod$coefficients[[6]] * age, runtime))

lmMod <- lm(sizeOfCrew ~ sizeOfCast + numberOfProductionCountries + numberOfProductionCompanies + age, data = crewNotZero)

train <- mutate(train, sizeOfCrew = ifelse(sizeOfCrew == 0, lmMod$coefficients[[1]] + 
                                             lmMod$coefficients[[2]] * sizeOfCast +
                                             lmMod$coefficients[[3]] * numberOfProductionCountries +
                                             lmMod$coefficients[[4]] * numberOfProductionCompanies +
                                             lmMod$coefficients[[5]] * age, sizeOfCrew))

lmMod <- lm(sizeOfCast ~ sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = castNotZero)

train <- mutate(train, sizeOfCast = ifelse(sizeOfCast == 0, lmMod$coefficients[[1]] + 
                                             lmMod$coefficients[[2]] * sizeOfCrew +
                                             lmMod$coefficients[[3]] * numberOfProductionCountries +
                                             lmMod$coefficients[[4]] * numberOfProductionCompanies +
                                             lmMod$coefficients[[5]] * age, sizeOfCast))

###################################
# Some EDA

train %>%
  select(revenue, genComedy:genNoGen) %>%
  gather(genComedy:genNoGen, key = genName, value = 'value') %>%
  filter(value == TRUE) %>%
  group_by(genName) %>%
  summarize(mRevenue = mean(10^revenue)) %>%
  arrange(desc(mRevenue)) %>%
  mutate(genName = gsub('gen', '', genName),
         genName = fct_reorder(genName, mRevenue)) %>%
  ggplot(aes(genName, mRevenue)) +
  geom_col() + 
  coord_flip() +
  labs(title = 'Mean revenue per genre', y = 'Mean revenue', x = '') +
  scale_y_continuous(labels = dollar_format())

train %>%
  select(revenue, prodUniversal:prodFoxSearchlight) %>%
  gather(prodUniversal:prodFoxSearchlight, key = prodName, value = 'value') %>%
  filter(value == TRUE) %>%
  group_by(prodName) %>%
  summarize(mRevenue = mean(10^revenue)) %>%
  arrange(desc(mRevenue)) %>%
  mutate(prodName = gsub('prod', '', prodName),
         prodName = fct_reorder(prodName, mRevenue)) %>%
  ggplot(aes(prodName, mRevenue)) +
  geom_col() + 
  coord_flip() +
  labs(title = 'Mean revenue per production company', y = 'Mean revenue', x = '') +
  scale_y_continuous(labels = dollar_format())


train %>%
  ggplot(aes(10^budget, 10^revenue)) +
  geom_point() +
  labs(title = 'Budgets effect on revenue', y = 'Revenue', x = 'Budget') +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm")

train %>%
  ggplot(aes(10^sizeOfCollection, 10^revenue)) +
  geom_point() +
  labs(title = 'The effect the size of collection has on revenue', y = 'Revenue', x = 'Size of collection') +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm")


train %>%
  ggplot(aes(sizeOfCast + sizeOfCrew, 10^revenue)) +
  geom_point() +
  labs(title = 'The effect the size of cast and crew has on revenue', y = 'Revenue', x = 'Size of cast and crew') +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm")

train %>%
  group_by(dayRelease) %>%
  summarize(mRev = mean(10^revenue)) %>% 
  mutate(dayRelease = as.factor(dayRelease),
         dayRelease = fct_recode(dayRelease, 'Monday' = '1', 'Tuesday' = '2', 'Wednesday' = '3', 
                                 'Thursday' = '4', 'Friday' = '5', 'Saturday' = '6', 'Sunday' = '7')) %>%
  ggplot(aes(dayRelease, mRev)) + 
  geom_col() +
  labs(title = 'Revenue depending on relsese weekday', y = '', x = '') +
  scale_y_continuous(labels = dollar_format())


###################################
# Same thing for test data
medianReleaseDate <- median(train$release_date, na.rm = TRUE)
train <- select(train, -release_date)

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
         genNoGen = ifelse(genres == 'NoGen', TRUE, FALSE),
         production_companies = ifelse(is.na(production_companies) == TRUE, 'NoProd', production_companies),
         prodUniversal = ifelse(stri_detect_fixed(production_companies, 'Universal Pictures'),TRUE, FALSE),
         prodParamount = ifelse(stri_detect_fixed(production_companies, 'Paramount Pictures'),TRUE, FALSE),
         prodTCF = ifelse(stri_detect_fixed(production_companies, 'Twentieth Century Fox Film Corporation'),TRUE, FALSE),
         prodColumbia = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures'),TRUE, FALSE),
         prodWarner = ifelse(stri_detect_fixed(production_companies, 'Warner Bros.'),TRUE, FALSE),
         prodNLC = ifelse(stri_detect_fixed(production_companies, 'New Line Cinema'),TRUE, FALSE),
         prodDisney = ifelse(stri_detect_fixed(production_companies, 'Walt Disney Pictures'),TRUE, FALSE),
         prodColumbiaPictures = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures Corporation'),TRUE, FALSE),
         prodTriStar = ifelse(stri_detect_fixed(production_companies, 'TriStar Pictures'),TRUE, FALSE),
         prodMGM = ifelse(stri_detect_fixed(production_companies, 'Metro-Goldwyn-Mayer (MGM)'),TRUE, FALSE),
         prodUnitedArtists = ifelse(stri_detect_fixed(production_companies, 'United Artists'),TRUE, FALSE),
         prodMiramax = ifelse(stri_detect_fixed(production_companies, 'Miramax Films'),TRUE, FALSE),
         prodTouchstone = ifelse(stri_detect_fixed(production_companies, 'Touchstone Pictures  '),TRUE, FALSE),
         prodFoxSearchlight = ifelse(stri_detect_fixed(production_companies, 'Fox Searchlight Pictures'),TRUE, FALSE),
         releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)),
         release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')),
         release_date = if_else(is.na(release_date) == TRUE, medianReleaseDate, release_date),
         age = as.integer(today() - release_date) / 365,
         quarterRelease = quarter(release_date),
         weekRelease = week(release_date),
         dayRelease = wday(release_date),
         runtime = ifelse(is.na(runtime) == TRUE, 0, runtime),
         sizeOfCast = str_count(cast, 'cast_id'),
         sizeOfCrew = str_count(crew, 'name'),
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         sizeOfProduction = sizeOfCast + sizeOfCrew,
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


# Log on all variables that have a log-normal dist

test <- mutate(test,
               budget = log10(budget + 1),
               sizeOfCollection = log10(sizeOfCollection + 1),
               age = log10(age))


# Fix movies without budget, runtime, sizeOfCast and sizeOfCrew
budgetNotZero <- filter(test, budget > 0 & sizeOfCrew > 0 & sizeOfCast > 0 
                        & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

runtimeNotZero <- filter(test, runtime > 0 & sizeOfCrew > 0 & sizeOfCast > 0 
                         & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

crewNotZero <- filter(test, sizeOfCrew > 0 & sizeOfCast > 0 
                      & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)

castNotZero <- filter(test, sizeOfCrew > 0 & sizeOfCast > 0 
                      & numberOfProductionCountries > 0  & numberOfProductionCompanies > 0)


lmMod <- lm(budget ~ sizeOfCast + sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = budgetNotZero)

test <- mutate(test, budget = ifelse(budget == 0, lmMod$coefficients[[1]] + 
                                       lmMod$coefficients[[2]] * sizeOfCast +
                                       lmMod$coefficients[[3]] * sizeOfCrew +
                                       lmMod$coefficients[[4]] * numberOfProductionCountries +
                                       lmMod$coefficients[[5]] * numberOfProductionCompanies +
                                       lmMod$coefficients[[6]] * age, budget))

lmMod <- lm(runtime ~ sizeOfCast + sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = runtimeNotZero)

test <- mutate(test, runtime = ifelse(runtime == 0, lmMod$coefficients[[1]] + 
                                        lmMod$coefficients[[2]] * sizeOfCast +
                                        lmMod$coefficients[[3]] * sizeOfCrew +
                                        lmMod$coefficients[[4]] * numberOfProductionCountries +
                                        lmMod$coefficients[[5]] * numberOfProductionCompanies +
                                        lmMod$coefficients[[6]] * age, runtime))

lmMod <- lm(sizeOfCrew ~ sizeOfCast + numberOfProductionCountries + numberOfProductionCompanies + age, data = crewNotZero)

test <- mutate(test, sizeOfCrew = ifelse(sizeOfCrew == 0, lmMod$coefficients[[1]] + 
                                           lmMod$coefficients[[2]] * sizeOfCast +
                                           lmMod$coefficients[[3]] * numberOfProductionCountries +
                                           lmMod$coefficients[[4]] * numberOfProductionCompanies +
                                           lmMod$coefficients[[5]] * age, sizeOfCrew))

lmMod <- lm(sizeOfCast ~ sizeOfCrew + numberOfProductionCountries + numberOfProductionCompanies + age, data = castNotZero)

test <- mutate(test, sizeOfCast = ifelse(sizeOfCast == 0, lmMod$coefficients[[1]] + 
                                           lmMod$coefficients[[2]] * sizeOfCrew +
                                           lmMod$coefficients[[3]] * numberOfProductionCountries +
                                           lmMod$coefficients[[4]] * numberOfProductionCompanies +
                                           lmMod$coefficients[[5]] * age, sizeOfCast))

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

set.seed(1111111)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

trainTrain <- train[train_ind, ]
testTrain <- train[-train_ind, ]


##################################################
# Find "best" parameters for RF

mtryList <- c(16,24,32,40)
nodeSizeList <- c(4,8,16)

rfList <- as_tibble(expand.grid(mtryList, nodeSizeList)) %>%
  rename(mtry = Var1, nodeSize = Var2) %>%
  mutate(rmsle = 0)

for(i in 1:dim(rfList)[1]){

  fitRF <- randomForest(formula = revenue ~ ., data = trainTrain,
                        ntree = 500,
                        mtry = rfList[i, ]$mtry,
                        nodesize = rfList[i, ]$nodeSize,
                        importance = FALSE)

  predictTestTrain <- predict(fitRF, testTrain)

  rfList[i, ]$rmsle <- sqrt(sum((log(10^predictTestTrain + 1) - log(10^testTrain$revenue + 1))^2)
                            / nrow(testTrain))

  # print(head(rfList, i))
}

rfList %>%
  arrange(rmsle)


##################################################
# Train using "best" parameters
# Calculate rmsle
# Plot importance

tic()
fitRF <- randomForest(formula = revenue ~ ., data = trainTrain,
                      ntree = 500,
                      replace = TRUE,
                      mtry = 32,
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

# ##########################################################################
# Re run using all of training data
# The predict on test and create submission

tic()
fitRF <- randomForest(formula = revenue ~ ., data = train,
                      ntree = 1000,
                      replace = TRUE,
                      mtry = 32,
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
