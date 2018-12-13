library(data.table)
library(tidyverse)

df <- as_tibble(fread("input/cities.csv")) 

###############################################################

# Function for returning closest city
findClosestPoint <- function(pX, pY, X, Y){
  
  distances <- sqrt((X - pX)^2 + (Y - pY)^2)
  closestPoit = which(distances == min(distances))[1]
  if(is.na(closestPoit) == FALSE){
    return(closestPoit)
  } else{
    return(-1)
  }
}

# City names
cityNames <- df[-1, ]$CityId

# Starting at north pole
curretPointX <- df[1,2]
curretPointY <- df[1,3]

remX <- df[-1, ]$X
remY <- df[-1, ]$Y


tick <- 2
visitedCities <- 0


tic()
while(length(cityNames) > 0){
  
  closestPoint <- findClosestPoint(curretPointX, curretPointY, remX, remY)
  curretPointX = remX[closestPoint]
  curretPointY = remY[closestPoint]
  
  remX <- remX[-closestPoint]
  remY <- remY[-closestPoint]
  visitedCities[tick] <- cityNames[closestPoint]
  cityNames <- cityNames[-closestPoint]
    
  tick = tick + 1
  
  if(tick %% 1000 == 0){
    print(tick)
  }
}
toc()

visitedCities[length(visitedCities) + 1] <- 0

visitedCities <- flipud(visitedCities)

submission <- as_tibble(as.integer(visitedCities)) %>%
  rename(path = value)

write.csv(x = submission, file = 'submissionNoPrime.csv', row.names = FALSE, quote = FALSE)





