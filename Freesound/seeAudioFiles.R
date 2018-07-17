library(tidyverse)
library(phonTools)
library(magick)


wd <- getwd()

td <- as_tibble(read.csv('train.csv'))
# td <- filter(td, label == 'Gunshot_or_gunfire')


for(i in 1:dim(td)[1]){
  
  fileName <- paste0(wd, '/audio_train/', td[i, ]$fname)
  picName <- paste0('c:/temp/audio_train_pics/', td[i, ]$fname, '.png')
  
  soundData <- loadsound(fileName)
  specSlice <- spectralslice(soundData, show = FALSE)
  
  png(picName, width = 100, height = 100, units = 'px', res = 20)
  
  specData <- spectrogram(soundData, 
                          fs = soundData$fs,
                          quality = FALSE,
                          show = TRUE,
                          padding = 0,
                          windowlength = soundData$duration/1000, 
                          dynamicrange = abs(range(specSlice[ ,2])[1] - range(specSlice[ ,2])[2]))
  dev.off()
  img <- image_read(picName)
  imgcrop <- image_crop(img, "65x65+15+15")
  image_write(imgcrop, path = picName, format = "png")
  
  print(i / dim(td)[1])
}




