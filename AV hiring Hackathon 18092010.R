setwd("D:/Study/Hackathon analytics vidhya/Hackathon AV hiring  10092020")


library(tidyverse)
library(tidymodels)
library(rsample)
df <- read.csv("combine_set3_train_test.csv", header = T)

data_train1<-read.csv('train.csv', header =T)  #9527
data_test1<- read.csv("test.csv",header = T)  #5045

df %>% count(Business_Sourced_fac)

df %>% summary()
df %>% colnames()
df %>% str()
df <- df[,-c(5,8,13)]
df$Business_Sourced<- as.factor(df$Business_Sourced_fac)
df <- df[,-c(7)]
dim(df) # 40 columns
# Create train and test sets
set.seed(15)
#tidy_split <- initial_split(df, prop = .8, strata = species)
tidy_split <- initial_split(df[1:9527,],prop = .8 , strata =Business_Sourced)
train_data <- training(tidy_split)
test_data <- testing(tidy_split)
dim(train_data)
#test_data <- testing(df[9528:14572,])
#train_data <- training(tidy_split)
#test_data <- testing(tidy_split)
kfolds_data <- vfold_cv(train_data)
#train_data$Business_Sourced<- as.factor(train_data$Business_Sourced)
str(train_data)
train_data %>% count(Business_Sourced)

# Pre-processing

tidy_rec <- recipe(Business_Sourced~., data = train_data) %>% 
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes())
tidy_rec

# Models

xgboost_model <- boost_tree(trees = tune(), tree_depth = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

logistic_model <- multinom_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

# Create tune grids

xgboost_grid <- grid_regular(parameters(xgboost_model), levels = 3)
logistic_grid <- grid_regular(parameters(logistic_model), levels = 5)

model_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

# Tune models

xgboost_res <- tune_grid(
  xgboost_model,
  tidy_rec,
  grid = xgboost_grid,
  resamples = kfolds_data,
  metrics = model_metrics
)

logistic_res <- tune_grid(
  logistic_model,
  tidy_rec,
  grid = logistic_grid,
  resamples = kfolds_data,
  metrics = model_metrics
)

# Evaluate Models
xgboost_res %>% collect_metrics(summarize = FALSE) %>% mutate(model = "xgboost") %>% 
  bind_rows(logistic_res %>% collect_metrics(summarize = FALSE) %>% mutate(model = "logistic")) %>% 
  ggplot(aes(x = model, y = .estimate)) + 
  geom_boxplot() + 
  facet_wrap(~.metric, scales = "free")

# Create our final model 


xgboost_model <- finalize_model(xgboost_model, xgboost_res %>% show_best("roc_auc") %>% slice(2))

final_model <- workflow() %>% 
  add_model(xgboost_model) %>% 
  add_recipe(tidy_rec)

# Very good results
last_fit(final_model, tidy_split) %>% collect_metrics()

final_model <- final_model %>% fit(df)

saveRDS(final_model, "xgbmodel_tidyverse.rds")
final_model
library(modelr)
test_data1<- df[9528:14572,]
class(test_data1)
pred<- predict(final_model,test_data1)
table(pred)
############################################################################
#Bagging 1.1
library(rpart)
library(caret)
df <- df[,-c(5,8,13)]
df$Business_Sourced<- as.factor(df$Business_Sourced_fac)
df <- df[,-c(7)]
colnames(df)
data_train5<- df[1:9527,]
data_test5<- df[9528:14572,]
cntrl <- trainControl(method = "cv", number = 10)
control = rpart.control(minsplit = 2, cp = 0)
fit_bag<- caret::train(Business_Sourced ~ ., data = data_train5, method = "treebag",nbagg= 200, trControl = cntrl)
pred_train_rf<- fit_bag %>% predict(data_train5[,-'Business_Sourced'])
rm(pred_test_rf)
colnames(data_test5)
pred_test_rf<- fit_bag %>% predict(data_test5[,-40])
confusionMatrix(pred_train_rf, data_train5[,7]) # Accuracy % ovefitting  94 %
confusionMatrix(fit_bag)  #81.08 on tri
table(pred_train_rf)
class(pred_train_rf)
pred_test_rf <- as.data.frame(pred_test_rf)
dim(pred_test_rf)
str(pred_test_rf)
pred_test_rf$ID <- data_test1$ID
head(pred_test_rf)
pred_test_rf1<- as.data.frame(cbind(pred_test_rf$ID,pred_test_rf$pred_test_rf))
head(pred_test_rf1)
colnames(pred_test_rf1)[1] <- "ID"
colnames(pred_test_rf1)[2] <- "Business_Sourced"
str(pred_test_rf1)
pred_test_rf1$Business_Sourced<- ifelse(as.numeric(pred_test_rf1$Business_Sourced)==1,0,1)
head(pred_test_rf1)
write.csv(pred_test_rf1,'pred_test5_treebag1.csv',row.names = F)


# Bagging Random Forest 1.2






#########################################################################
## If you have used one of the tune_* functions to find the best parameters for your model and then finalized 
#  your workflow with those parameters, the next step is to train or fit that workflow one more time on the whole training set.

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

k_folds <- vfold_cv(data_train5, strata = Business_Sourced)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  size = 5
)

xgb_wf <- workflow() %>%
  add_formula(Business_Sourced ~ .) %>%
  add_model(xgb_spec)

xgb_wf

#Next, let's finalize this workflow and then fit() it to the training data.
#(The tuning process used the training data, but that was to find the best model parameters, not to train the model itself.)

doParallel::registerDoParallel()
set.seed(123)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = k_folds,
  grid = xgb_grid
)

xgb_res

trained_wf <- xgb_wf %>%
  finalize_workflow(
    select_best(xgb_res, "roc_auc")
  ) %>%
  fit(data_test5)

trained_wf


###############################################################################################################
# Columns for Office_PIN , Applicant_City_PIN , Application_Receipt_Date_Count ,Applicant_Age , Manager_Exp , Manager_Age
install.packages('devtools')
library(devtools)
install.packages("dataPreparation")
library(dataPreparation)
df1 <- read.csv("combine_set3_train_test.csv", header = T)
bins1 <- build_bins(df1, cols = "Office_PIN", n_bins = 10, type = "equal_freq")
bins2 <- build_bins( df1, cols = "Applicant_City_PIN", n_bins = 10, type = "equal_freq")
bins3 <- build_bins( df1, cols = "Application_Receipt_Date_Count", n_bins = 10, type = "equal_freq")
bins4 <- build_bins( df1, cols = "Applicant_Age", n_bins = 10, type = "equal_freq")
bins5 <- build_bins( df1, cols = "Manager_Exp", n_bins = 10, type = "equal_freq")
bins6 <- build_bins( df1, cols = "Manager_Age", n_bins = 10, type = "equal_freq")

print(bins1)  #-Inf    1    2    3    4    5    7    8  Inf
print(bins2) #-Inf    1    2    3    4    5    7    8  Inf
print(bins3) # -Inf   16   23   27   32   39   43   51   66   86  Inf
print(bins4) # -Inf   35   37   39   41   43   45   48   52   59  Inf
print(bins5)  #-Inf    0   12   13   14   15   17  Inf
print(bins6) # -Inf    0   40   42   44   45   47   49   51   55  Inf

df1<- fastDiscretization(dataSet = df1, bins = list(Office_PIN = c(0,1,2,3,4,5,7,8,9)))
df1<- fastDiscretization(dataSet = df1, bins = list(Applicant_City_PIN = c(0,1,2,3,4,5,7,8,9)))
df1<- fastDiscretization(dataSet = df1, bins = list(Application_Receipt_Date_Count = c(0,16, 23, 27, 32, 39, 43, 51, 66, 86)))
df1<- fastDiscretization(dataSet = df1, bins = list(Applicant_Age = c(0,35, 37, 39, 41, 43, 45, 48, 52, 59)))
df1<- fastDiscretization(dataSet = df1, bins = list(Manager_Exp = c(0, 12, 13, 14, 15, 17)))
df1<- fastDiscretization(dataSet = df1, bins = list(Manager_Age = c(0, 40, 42, 44, 45, 47, 49, 51, 55)))

head(df1)
summary(df1)
write.csv(df1, 'combine_data_train_test4.csv', row.names = F)
dim(df1)
colnames(df1)
encoding <- build_encoding(dataSet = df1, cols = "auto", verbose = TRUE)
df2 <- one_hot_encoder(dataSet = df1[,-c(9)], encoding = encoding, drop = TRUE, verbose = TRUE)
str(df2)
colnames(df2)
df2 <- df2[,-c(50,51)]
bijections <- whichAreBijection(dataSet = df2, verbose = TRUE)

summary(df2)
write.csv(df1, 'combine_data_train_test5.csv', row.names = F)
#################################################################################################################

data_train5<- df[1:9527,]
data_test5<- df[9528:14572,]