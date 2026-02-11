library(MASS)
library(naniar)
library(mice)
library(brant)
library(VGAM)
library(tidyverse)
library(caret)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::group_by)
### 
PATH = "../Heart_Failure_py/CSV_datasets/Heart_failure.csv"

wrangle <- function(path = PATH){

  df2 <- read.csv(PATH)

###
#---
### wrangling

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
        .cols = c("sex", "fbs", "restecg", "exang"),
        ~ factor(.)
      )
    )

  df2$cp = factor(df2$cp, levels = c(1, 2, 3, 4), ordered = T)
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
  
df2 <- wrangle(path = PATH)

train_dat <- df2[121:nrow(df2),]
test_dat <- df2[1:120, ]

write.csv2(test_dat, file = "../Heart_Failure_py/CSV_datasets/test.csv", row.names = F)
write.csv2(train_dat, file = "../Heart_Failure_py/CSV_datasets/train.csv", row.names = F)

save(test_dat, file = "test.RData")
save(train_dat, file = "train.RData")

###

df2 <- train_dat
my_colnames = colnames(df2)


unique(df2$restecg)

df2$restecg <- factor(df2$restecg,
                      levels = c(0, 1, 2))
df2 %>% str()
plot(df2[["restecg"]])

#There are two NA values just impute both of them with 0

df2 <- df2 %>% 
  mutate(
    restecg = factor(case_when(
      is.na(restecg) ~ "0",
      TRUE ~ restecg
    )
  ))

plot(df2[["restecg"]])

vis_miss(df2)


#### --- Impute: `thalach, exang, trestbps, oldpeak` --####

#Data visualization to check

# For categorical variables

cat_cols <- my_colnames[sapply(my_colnames, function(x)(is.factor(df2[[x]])))]

par(mfrow = c(3, 2))
for (cat_col in cat_cols){
  plot(df2[[cat_col]],
       main = paste0(cat_col))
}
# - it is fine


# For numerical variables
par(mfrow = c(3, 2))
num_cols <- my_colnames[sapply(my_colnames, function(x)!(is.factor(df2[[x]])))]

for (num_col in num_cols){
  hist(
    df2[[num_col]],
    main = paste0(num_col))
}


###

rm(cat_col, cat_cols, train_dat, test_dat, num_col, num_cols)

#--- Imputation
###

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

attributes(imputed_rf)

### w/ logreg - for binomial, polyreg -for nominal and polyr -- for ordinal

mtds = c("pmm", "logreg", "polyreg", "polyr")

methods_vector_lr <- c(
  age = mtds[1],
  sex = mtds[2],
  cp = mtds[4],
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


###  Test the validity of your tested values.

# For the three numerical variables

densityplot(imputed_rf, main = "imputed_rf")
densityplot(imputed_lr, main = "imputed_lr")

# For the two categorical variables

# Test logical plausibility
stripplot(imputed_rf, fbs + exang ~ .imp, main = "imputed rf")
stripplot(imputed_lr, fbs + exang ~ .imp, main = "imputed lr")

#For all
plot(imputed_rf, main = "imputed_rf")
plot(imputed_lr, main = "imputed_lr")

# check proportion

imputed_dat_rf <- complete(imputed_rf, 1)

imp_fbs <- prop.table(table(imputed_dat_rf$fbs))
orig_fbs <- prop.table(table(imputed_dat$fbs))


imp_exang <- prop.table(table(imputed_dat_rf$exang))
orig_exang<- prop.table(table(imputed_dat$exang))

imputed_dat_lr <- complete(imputed_lr, 1)

imp_fbs_l <- prop.table(table(imputed_dat_lr$fbs))
orig_fbs_l <- prop.table(table(imputed_dat$fbs))


imp_exang_l <- prop.table(table(imputed_dat_lr$exang))
orig_exang_l <- prop.table(table(imputed_dat$exang))

cat("RF\n fbs\n", abs(imp_fbs - orig_fbs),"\n", "Exang\n", abs(imp_fbs - orig_fbs))
cat("LR\n fbs\n", abs(imp_fbs_l - orig_fbs_l),"\n", "Exang\n", abs(imp_fbs_l - orig_fbs_l))

rm(imputed_dat_lr,  imputed_dat_rf,
   orig_exang, orig_exang_l, orig_fbs, orig_fbs_l,
   imp_exang, imp_fbs, imp_fbs_l, imp_exang_l)

###

# lr was better
#####---- Logistic regression ----######

 ## Checking assumptions if we can use Ordinal logistic regression - polyr

dat <- mice::complete(imputed_lr, 1)
vis_miss(dat)
#1. Proportional Odds / Parallel Lines Assumption

test_fit <- MASS::polr(num ~ age + sex + cp + trestbps +
                   fbs + restecg + thalach + exang + oldpeak, 
                 data = dat, Hess = TRUE)

###

brant(test_fit)

dat %>% 
  filter(!(is.na(exang) | is.na(num))) %>% 
  count(exang, num) %>% 
  ggplot(aes(exang, n, fill = num)) + 
  geom_col(position = "fill")

dat %>% 
  filter(!(is.na(sex) | is.na(num))) %>% 
  count(sex, num) %>% 
  ggplot(aes(sex, n, fill = num)) + 
  geom_col(position = "fill")

dat %>% 
  ggplot(aes(x = num, y = age)) +
  geom_boxplot(varwidth = T) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = max(x, na.rm = TRUE),
        label = paste0("n = ", length(x))
      )
    },
    geom = "text",
    vjust = 0.5
  )

#To compare with a pval > 0.05 var

dat %>% 
  ggplot(aes(x = num, y = trestbps)) +
  geom_boxplot(varwidth = T) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = max(x, na.rm = TRUE),
        label = paste0("n = ", length(x))
      )
    },
    geom = "text",
    vjust = 0.5
  )

### So, I can't use polr, my second more accurate option is PPOM

fit_ppo <- with(imputed_lr, 
                vglm(num ~ age + sex + cp +
                           trestbps + fbs + restecg +
                           thalach + exang + oldpeak, 
                     family = cumulative(parallel = FALSE ~ age + restecg + exang)))




# Use Rubin's Rule to pool our model


analyses <- fit_ppo$analyses

all_coefs <- sapply(analyses, coef)
all_var_est <- lapply(analyses, vcov)


# pooled estimate
pooled_coefs <- rowMeans(all_coefs)

# within variance
u_var <- Reduce("+", all_var_est) / length(all_var_est)
u_var <- diag(u_var)

# between variance
b_var <- apply(all_coefs, 1, var)

#Vt = Vw + Vb + Vb/m

t_var = u_var + b_var + b_var/imputed_lr$m

###
pooled_se = sqrt(t_var)
z_score = pooled_coefs/ pooled_se
z_score


### Use analyses-1 to build our model

final_model = analyses[[1]]
final_model@coefficients <- pooled_coefs


### Test your model

load(file = "test.RData")
test_dat %>% str()
#view(test_dat)

test_imp <- mice(test_dat, m = 5, method = methods_vector_lr,
     maxit = 15,
     visitSequence = my_sequence, seed = 1000)

test_dat <- mice::complete(test_imp, 1)

pred_probs <- predict(final_model, newdata = test_dat, type = "response")

weight_factor <- df %>% 
  group_by(num) %>% 
  summarise(
    perc. = round(n()/nrow(df), 2)
  ) %>% 
  mutate(
    K = (1/(perc.)^(1/3))/1.3
  ) %>% select(K) %>% unlist()


predicted_classes <- apply(pred_probs, 1, which.max) - 1
actual_classes <- test_dat$num %>% unlist()
predicted_classes


Diagnost = data.frame(
  pred = predicted_classes,
  real = as.numeric(actual_classes) - 1
)

Diagnost <- Diagnost %>% 
  mutate(
    success = if_else(pred == real, T, F),
    bin_real = if_else(real == 0, T, F),
    bin_pred = if_else(pred == 0, T, F),
    abs_err = abs(pred - real),
    bin_accuracy = if_else(bin_real == bin_pred, "Correct", "Wrong")
  )

cat("Absolute success Rate: ", mean(Diagnost$success),
    "\nBinary Accuracy Rate: ", mean(Diagnost$bin_accuracy == "Correct"),
    "\nMean Predictive Difference: ", mean(Diagnost$abs_err),
    "\nOptimist(+) or Pessimist(-): ", -1 * mean(as.numeric(Diagnost$pred) - as.numeric(Diagnost$real)
                                    )
)

OR <- exp(-pooled_coefs)
## Diagnostics of the model

  # Confusing matrix
  # 


pred = factor(Diagnost$pred,
              levels = c(0, 1, 2, 3, 4),
              labels = c(0, 1, 2, 3, 4))

real = factor(Diagnost$real,
              levels = c(0, 1, 2, 3, 4),
              labels = c(0, 1, 2, 3, 4))

confusionMatrix(real, pred)

# Results show us that we should factor in Prevalance when deciding a 
# cut point for our model.


#Analysis is over you have got the results:
 # - Prepare a report 
 # - Create a dashboard to report your work
 # - Convert your model to a standard  form to use python for the dashboard




############## Save the model in json format

## Used AI to convert into json format
library(jsonlite)

cp_contrats <- contrasts(imputed_dat_lr$cp)

cf_mat <- coef(final_model, matrix = TRUE)

my_model <- list(
  
  thresholds = list(
    intercepts = as.numeric(cf_mat["(Intercept)", ]),
    age        = as.numeric(cf_mat["age", ]),
    restecg1   = as.numeric(cf_mat["restecg1", ]),
    restecg2   = as.numeric(cf_mat["restecg2", ]),
    exang1     = as.numeric(cf_mat["exang1", ])
  ),
  
  parallel = list(
    sex1     = as.numeric(cf_mat["sex1", 1]),
    cp_L     = as.numeric(cf_mat["cp.L", 1]),
    cp_Q     = as.numeric(cf_mat["cp.Q", 1]),
      cp_C     = as.numeric(cf_mat["cp.C", 1]),
    trestbps = as.numeric(cf_mat["trestbps", 1]),
    fbs1     = as.numeric(cf_mat["fbs1", 1]),
    thalach  = as.numeric(cf_mat["thalach", 1]),
    oldpeak  = as.numeric(cf_mat["oldpeak", 1])
  ),
  
  cp_mapping = list(
    level_1 = cp_contrats[1, ],
    level_2 = cp_contrats[2, ],
    level_3 = cp_contrats[3, ],
    level_4 = cp_contrats[4, ]
)
)

write_json(my_model, "../Heart_Failure_py/Jupyter/Heart_failure_model.json", auto_unbox = T, digits = 8, pretty = T)


