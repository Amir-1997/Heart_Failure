library(MASS)
library(mice)
library(VGAM)
library(tidyverse)
library(caret)
library(jsonlite)
library(plumber)

weight_factor = c(1.00381222, 1.16213696, 1.55953897, 1.55953897, 2.47561381)
final_model <- read_rds(file = "model_heart_failure.rds")

p <- plumb("plumber.R")
p$run(host = "0.0.0.0", port = 8000)


