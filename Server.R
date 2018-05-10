
library(plumber)
#setwd("/Users/anshul/Downloads/")
r <- plumb("/Users/anshul/Downloads/Git Repos/ML-Engineering-module/Bath_temperature-api - validated.R")
#r <- plumb("/Users/anshul/Downloads/Bath_temperature-api.R")
r$run(port=8000)

