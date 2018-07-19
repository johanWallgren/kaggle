
library(tidyverse)
library(h2o)

h2o.init(ip = "localhost",
         port = 54321,
         nthreads = -1,
         min_mem_size = "4g")






h2o.shutdown(prompt = FALSE)
