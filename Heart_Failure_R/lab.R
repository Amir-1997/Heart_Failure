library(tidyverse)
library(naniar)
library(mice)

df <- read.csv("../Heart_Failure_py/CSV_datasets/Heart_failure.csv")
# Goal: to develop a logistic regression model that predicts Coronary Artery Disease


####### --- Data Cleaning --- #######
  # TO-DO's
my_colnames <- colnames(df)

df2 <- df %>% 
  mutate(across(
    .cols = everything(),
    function(x){
      case_when(x %in% c("?", "-9") ~ NA,
                TRUE ~ x)
    }
  )) %>% 
  mutate(across(
    .cols = everything(), as.numeric
  )) %>%
  mutate(
    chol = case_when(
      chol == 0 ~ NA,
      T ~ chol
    )
  )
  




######## --- Handling NA-vals --- #######
my_colnames = colnames(df2)

#1 Remove too much NA cols

na_plot <- sapply(colnames(df2), function(x){
    round((mean(is.na(df2[[x]]))), 2)
  }
  ) %>% sort()

barplot(na_plot,
        horiz = T,
        las = 1,
        xlim = c(0, 1),
        cex.names = 0.8)

df2 <- df2 %>% 
  select(-c("thal", "ca", "slope", "chol"))


naniar::vis_miss(df2, show_perc = T)

#2. Impute w/ mean or median for restecg

unique(df2$restecg)

df2$restecg <- factor(df2$restecg,
                      levels = c(0, 1, 2))
df2 %>% str()
plot(df2[["restecg"]])

    #There are two NA values just impute both of them with 0

df2 <- df2 %>% 
  mutate(
    restecg = case_when(
      is.na(restecg) ~ "0",
      TRUE ~ restecg
    )
  )

vis_miss(df2)
test_dat <- df2[1:120, ]
train_dat <- df2[121:nrow(df2), ]
write.csv2(test_dat, file = "../Heart_Failure_py/CSV_datasets/test.csv")
save(test_dat, file = "test.RData")
write.csv2(train_dat, file = "../Heart_Failure_py/CSV_datasets/train.csv")
save(train_dat, file = "train.RData")

#3 Handle the stress related variables which disappeared together:
  # thalach, exang, oldpeak, trestbps.

#prep dat for modelling

sapply(colnames(df2), function(x){
  length(unique(df2[[x]]))  
}
)

df2 <- df2 %>% 
  mutate(
    across(
      .cols = c("sex", "fbs", "restecg", "exang"),
      ~ factor(.)
    )
  )


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
rm(cat_col, cat_cols, train_dat, test_dat, na_plot)

# For numerical variables
par(mfrow = c(3, 2))
num_cols <- my_colnames[sapply(my_colnames, function(x)!(is.factor(df2[[x]])))]

for (num_col in num_cols){
  hist(
       df2[[num_col]],
       main = paste0(num_col))
  }

# All are fine as well: num is my target variable and it is a factor it slipped.

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

mtds = c("pmm", "logreg", "polyreg", "polyr")

imputed$method
methods_vector_lr <- c(
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
                visitSequence = my_sequence)

attributes(imputed_rf)
complete(imputed_rf, 1) %>% view()



imp_dat <- complete(imputed_rf, 1)
naniar::vis_miss(imp_dat)

###  Test the validity of your tested values.

# For the three numerical variables

densityplot(imputed_rf) # passed
plot(imputed_rf) # passed

# For the two categorical variables

        # Test logical plausibility
stripplot(imputed_rf, fbs + exang ~ .imp) # gets it is categorical

        # check proportion

orig_fbs <- prop.table(table(imputed_dat_rf$fbs))
imp_fbs <- prop.table(table(imp_dat_rf$fbs))


orig_exang <- prop.table(table(imputed_dat_rf$exang))
imp_exang <- prop.table(table(imp_dat_rf$exang))

  
cat("fbs\n", orig_fbs, "\n", imp_fbs)
cat("Exang\n", orig_exang, "\n", imp_exang) #passed 

      # Relationship check.

#####---- Logistic regression ----######



test_dat %>% str()

df2 %>% str()



