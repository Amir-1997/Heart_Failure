library(MASS)
library(mice)
library(VGAM)
library(tidyverse)
library(caret)
library(jsonlite)
library(plumber)

weight_factor = c(1.00381222, 1.16213696, 1.55953897, 1.55953897, 2.47561381)
final_model <- read_rds(file = "model_heart_failure.rds")

#* @apiDescription Heart Disease Classifier
#* @param req the json file including all the params
#* @post /predict
#* @serializer json


function(req = NULL){
  
  #get request
  tryCatch({
    input_data <- fromJSON(req$postBody)
  }, error =  function(e)
    {
    error_message = "Error in getting the request"
    return(error_message)
  })
  
  dat <- data.frame(
    age = as.numeric(input_data$age),
    sex = factor(input_data$sex, levels = c("0", "1")),
    cp = as.factor(input_data$cp),
    trestbps = as.numeric(input_data$trestbps),
    fbs = as.factor(input_data$fbs),
    restecg = as.factor(input_data$restecg),
    thalach = as.numeric(input_data$thalach),
    exang = factor(input_data$exang, levels = c("0", "1")),
    oldpeak = as.numeric(input_data$oldpeak)
  )
  #predict
  tryCatch({
    pred_probs <- VGAM::predictvglm(final_model, newdata = dat, type = "response")
    weighted_probs <- sweep(pred_probs, MARGIN = 2, STATS = weight_factor, FUN = "*")
    
    
    predicted_classes <- unname(apply(pred_probs, 1, which.max) - 1)
    weighted_classes <- unname(apply(weighted_probs, 1, which.max) - 1)
    
    clean_output = list(
      prediction = weighted_classes,
      status = "success"
    )
     return(clean_output)
  }, error = function(e){
    error_output = list(
      error = "Prediction failed",
      message = e$message,
      status = "error"
    )
    return(error_output)
  }
  )
}