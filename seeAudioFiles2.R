library(tidyverse)
library(tuneR)
library(magick)
library(signal)
library(oce)

wd <- getwd()

td <- as_tibble(read.csv('train.csv'))



######################################################################################

spectro = function(data, nfft=32, window=32, overlap=16, t0=0, plot_spec = T, normalize = F, return_data = F,...){
  # Function from https://hansenjohnson.org/post/spectrograms-in-r/
  
  # extract signal
  snd = data@left
  
  # demean to remove DC offset
  snd = snd-mean(snd)
  
  # determine duration
  dur = length(snd)/data@samp.rate
  
  # create spectrogram
  spec = specgram(x = snd,
                  n = nfft,
                  Fs = data@samp.rate,
                  window = window,
                  overlap = overlap
  )
  
  # discard phase info
  P = abs(spec$S)
  
  # normalize
  if(normalize){
    P = P/max(P)  
  }
  
  # convert to dB
  P = 10*log10(P)
  
  # config time axis
  if(t0==0){
    t = as.numeric(spec$t)
  } else {
    t = as.POSIXct(spec$t, origin = t0)
  }
  
  # rename freq
  f = spec$f
  
  if(plot_spec){
    
    # change plot colour defaults
    par(bg = "white")
    par(col.lab="white")
    par(col.axis="white")
    par(col.main="white")
    
    # plot spectrogram
    imagep(t, f, t(P), col = oce.colorsViridis, drawPalette = FALSE,
           ylab = 'Frequency [Hz]', axes = FALSE, decimate=FALSE,...)
    
    box(col = 'white')
    axis(2, labels = T, col = 'white')
    
    # add x axis
    if(t0==0){
      
      axis(1, labels = T, col = 'white')
      
    }else{
      
      axis.POSIXct(seq.POSIXt(t0, t0+dur, 10), side = 1, format = '%H:%M:%S', col = 'white', las = 1)
      mtext(paste0(format(t0, '%B %d, %Y')), side = 1, adj = 0, line = 2, col = 'white')
      
    }
  }
  
  if(return_data){
    
    # prep output
    spec = list(
      t = t,
      f = f,
      p = t(P)
    )
    
    return(spec)  
  }
}
######################################################################################

for(i in 2266:dim(td)[1]){
# for(i in 1:dim(td)[1]){
  
  fileName <- paste0(wd, '/audio_test/', td[i, ]$fname)
  picName <- paste0('c:/temp/audio_test_pics/', td[i, ]$fname, '.png')
  
  soundData <- readWave(fileName)
  soundDataTemp <- soundData
  
  # Removes scilence
  soundData <- noSilence(soundData, zero = 0, level = 75, where = c("both", "start", "end"))
  
  if (length(soundData@left) == 0){
    soundData = soundDataTemp
  }
    
  # Cutting sound after 5 seconds
  maxTimeOfSound <- 3 # seconds
  if(length(soundData)/soundData@samp.rate > maxTimeOfSound){
    soundData <- extractWave(soundData, from = 1, to = maxTimeOfSound * soundData@samp.rate)
  }
  
  # Creating spectrogram
  png(picName, width = 100, height = 100, units = 'px', res = 20)
  spectro(soundData, plot_spec = TRUE, normalize = FALSE, return_data = FALSE)
  dev.off()
  
  # Cropping picture
  img <- image_read(picName)
  imgcrop <- image_crop(img, "65x65+15+15")
  image_write(imgcrop, path = picName, format = "png")
  
  print(round(i / dim(td)[1] * 100))
}




