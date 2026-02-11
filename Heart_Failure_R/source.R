library(MASS)
library(naniar)
library(mice)
library(brant)
library(VGAM)
library(tidyverse)
library(caret)
library(kableExtra)
library(patchwork)
library(gtsummary)
library(jsonlite)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::arrange)

PATH = "CSV_datasets/Heart_failure.csv"

df <- read.csv(PATH)

df <- df %>% 
  mutate(across(
    .cols = everything(),
    function(x){
      case_when(x %in% c("?", "-9") ~ NA,
                TRUE ~ x)
    }
  )) %>% 
  mutate(across(
    .cols = everything(), as.numeric
  ))

df <- df %>% 
  mutate(
    across(
      .cols = c("sex", "fbs", "restecg", "exang", "cp"),
      ~ factor(.)
    )
  )

df$num = factor(df$num, levels = c(0, 1, 2, 3, 4), ordered = T)

df <- df %>% 
  mutate(
    restecg = factor(case_when(
      is.na(restecg) ~ "0",
      TRUE ~ restecg
    )
    ))

df_naplot <- vis_miss(df)


wrangle <- function(path = PATH){
  
  df2 <- read.csv(PATH)
  

  df2 <- df2 %>% 
    select(-all_of(c("thal", "ca", "slope", "chol")))
  
  df2 <- df2 %>% 
    mutate(across(
      .cols = everything(),
      function(x){
        case_when(x %in% c("?", "-9") ~ NA,
                  TRUE ~ x)
      }
    )) %>% 
    mutate(across(
      .cols = everything(), as.numeric
    ))
  
  df2 <- df2 %>% 
    mutate(
      across(
        .cols = c("sex", "fbs", "restecg", "exang", "cp"),
        ~ factor(.)
      )
    )
  
  df2$num = factor(df2$num, levels = c(0, 1, 2, 3, 4), ordered = T)
  
  df2 <- df2 %>% 
    mutate(
      restecg = factor(case_when(
        is.na(restecg) ~ "0",
        TRUE ~ restecg
      )
      ))
  
  ####
  
  ### 
  # --- splitting data
  
  ###
  set.seed(1000)
  df2_scramble <- sample(c(1:920), 920)
  
  df2 <- df2[df2_scramble,]
  rownames(df2) <- c(1:nrow(df2))
  return(df2)
}

df2 <- wrangle(PATH)
train_dat <- df2[121:nrow(df2),]
test_dat <- df2[1:120, ]

save(test_dat, file = "test.RData")
save(train_dat, file = "train.RData")


df2 <- train_dat
my_colnames = colnames(df2)

par(mfrow = c(1, 1))

unique(df2$restecg)

df2$restecg <- factor(df2$restecg,
                      levels = c(0, 1, 2))

df2 <- df2 %>% 
  mutate(
    restecg = factor(case_when(
      is.na(restecg) ~ "0",
      TRUE ~ restecg
    )
    ))


features = c( "trestbps", "thalach", "exang", "oldpeak")

predictors = colnames(df2)[!(colnames(df2) %in% features)]

imputed_dat <- df2

my_colnames = colnames(imputed_dat)  

my_colnames[sapply(my_colnames, function(x)(is.factor(df2[[x]])))]
mtd = c("pmm", "rf")

methods_vector = c(
  age = mtd[1],
  sex = mtd[2],
  cp = mtd[2],
  trestbps = mtd[1],
  fbs = mtd[2],
  restecg = mtd[2],
  thalach = mtd[1],
  exang = mtd[2],
  oldpeak = mtd[1],
  num = mtd[2]
)

na_cols <- colSums(is.na(imputed_dat))
my_sequence = names(sort(na_cols))


imputed_rf <- mice(imputed_dat, m = 5,
                   method = methods_vector,
                   maxit = 15,
                   visitSequence = my_sequence, seed = 1000)

mtds = c("pmm", "logreg", "polyreg", "polyr")

methods_vector_lr <- c(
  age = mtds[1],
  sex = mtds[2],
  cp = mtds[3],
  trestbps = mtds[1],
  fbs = mtds[2],
  restecg = mtds[3],
  thalach = mtds[1],
  exang = mtds[2],
  oldpeak = mtds[1],
  num = mtds[4]
)


imputed_lr <- mice(imputed_dat, m = 5,
                   method = methods_vector_lr,
                   maxit = 15,
                   visitSequence = my_sequence, seed = 1000)



imputed_dat_lr <- mice::complete(imputed_lr, 1)


fit_ppo <- with(imputed_lr, 
                vglm(num ~ age + sex + cp +
                       trestbps + fbs + restecg +
                       thalach + exang + oldpeak, 
                     family = cumulative(parallel = FALSE ~ age + restecg + exang)))



analyses <- fit_ppo$analyses

all_coefs <- sapply(analyses, coef)
all_var_est <- lapply(analyses, vcov)
imp_m <- imputed_lr$m

# pooled estimate
pooled_coefs <- rowMeans(all_coefs)

final_model = analyses[[1]]
final_model@coefficients <- pooled_coefs



write_rds(final_model, file = "model_heart_failure.rds")



############## Finished building the model###########


          #### Testing #############

load(file = "test.RData")
test_dat %>% str()

target_test = test_dat$num %>% unlist()
test_dat = test_dat[, 1:9]

weight_factor <- df2 %>% 
  group_by(num) %>% 
  summarise(
    perc. = round(n()/nrow(df2), 2)
  ) %>% 
  mutate(
    K = (1/(perc.)^(1/3))/1.3
  ) %>% select(K) %>% unlist()

test_imp <- mice(test_dat, m = 5, method = methods_vector_lr[1:9],
                 maxit = 15,
                 visitSequence = my_sequence, seed = 1000)

test_dat <- mice::complete(test_imp, 1)

pred_probs <- VGAM::predictvglm(final_model, newdata = test_dat, type = "response")
weighted_probs <- sweep(pred_probs, MARGIN = 2, STATS = weight_factor, FUN = "*")




predicted_classes <- apply(pred_probs, 1, which.max) - 1
weighted_classes <- unname(apply(weighted_probs, 1, which.max) - 1)
actual_classes <- target_test


Diagnost = data.frame(
  pred = weighted_classes,
  real = as.numeric(actual_classes) - 1
)

mean(Diagnost$pred == Diagnost$real)

Diagnost <- Diagnost %>% 
  mutate(
    success = if_else(pred == real, T, F),
    bin_real = if_else(real == 0, T, F),
    bin_pred = if_else(pred == 0, T, F),
    abs_err = abs(pred - real),
    bin_accuracy = if_else(bin_real == bin_pred, "Correct", "Wrong")
  )

print(mean(Diagnost$bin_accuracy == "Correct"))
###
Diagnost2 = data.frame(
  pred = as.numeric(weighted_classes),
  real = as.numeric(actual_classes) - 1
)

Diagnost2 <- Diagnost2 %>% 
  mutate(
    success = if_else(pred == real, T, F),
    bin_real = if_else(real == 0, T, F),
    bin_pred = if_else(pred == 0, T, F),
    abs_err = abs(pred - real),
    bin_accuracy = if_else(bin_real == bin_pred, "Correct", "Wrong")
  )

OR <- exp(-pooled_coefs)
## Diagnostics of the model

# Confusing matrix
# 


real = factor(Diagnost$real,
              levels = c(0, 1, 2, 3, 4),
              labels = c(0, 1, 2, 3, 4))

pred = factor(Diagnost$pred,
              levels = c(0, 1, 2, 3, 4),
              labels = c(0, 1, 2, 3, 4))

pred2 = factor(Diagnost$pred,
               levels = c(0, 1, 2, 3, 4),
               labels = c(0, 1, 2, 3, 4))


model_cm <- confusionMatrix(real, pred)
results_diag <- round(model_cm$byClass, 2)

model_cm2 <- confusionMatrix(real, pred2)
results_diag2 <- round(model_cm2$byClass, 2)


# Source - https://stackoverflow.com/a/8189441
# Posted by Ken Williams, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-01, License - CC BY-SA 4.0

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


replace_vals = list()
for(c in colnames(train_dat)[colnames(train_dat) != "num"]){
  if(class(train_dat[,c]) == "factor"){
    replace_vals[c] = Mode(train_dat[,c])
  }else{
    replace_vals[c] = median(train_dat[,c], na.rm = T)
    }
}

replace_vals[["weight_factor"]] = c(1.00381222, 1.16213696, 1.55953897, 1.55953897, 2.47561381)

write_json(replace_vals, "replace_vals.json", auto_unbox = T, digits = 8, pretty = T)

