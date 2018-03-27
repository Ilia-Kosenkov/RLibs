library(rjags)
library(tidyverse)
modelStr <- paste("model", "{", "a ~ dnorm(0, 1)", "}", sep = "\r\n")
source(file.path("..", "package", "R", "BSTools.R"))
