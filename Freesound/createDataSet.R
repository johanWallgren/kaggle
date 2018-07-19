
library(tidyverse)
library(OpenImageR)

td <- as_tibble(read.csv('train.csv'))
tdNum <- mutate(td, label = as.integer(label))

pathToAllImages <- 'c:/temp/audio_train_pics/'
images <- list.files(pathToAllImages)

df <- tibble()

for(i in 1:length(images)){
  
  label <- c(1:max(tdNum$label)) * 0
  label[tdNum[i, ]$label] <- 1
  
  
  pathToImage <- paste0(pathToAllImages,images[i])
  img <- readImage(pathToImage)
  imgG <- rgb_2gray(img)
  imgGS <- resizeImage(imgG, width = 50, height = 50, method = 'bilinear')
  
  img_vector <- as.vector(imgGS)

  vec <- t(c(label, img_vector))

  df <- rbind(df,vec)
  
  print(i)
}

write.csv(df,'trainOnPictures.csv')
