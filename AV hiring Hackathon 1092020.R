setwd("D:/Study/Hackathon analytics vidhya/Hackathon AV hiring  10092020")

data_train <- read.csv("train.csv", header = T, stringsAsFactors = T ,na.strings = "")  # na.strings=c(""," ","NA") can also use this
data_test <- read.csv("test.csv", header =T,stringsAsFactors = T, na.strings = "", )   #na.strings=c(""," ","NA")

dim(data_train)# 9527   23
dim(data_test)#5045   22

str(data_train)  # data is treated as factos, city pins int, manager grade as int

summary(data_train)  
# general overview
# NA in Applicant city pin, Applicant birhtdate is have blank spaces,applicant gender, 
# Manager Business is showing values as negative it means manager is loosing business. and we have to manager bussiness 2 is the same case
# with manager business 2 or are they independent of each other.

anyNA(data_train)  # true as we have already seen from the summary
anyNA(data_test)  # its also true

# Next step converting blank spae to na.
library(tidyverse)
data_train<- data_train %>% mutate_all(na_if, c(""))
data_test <- data_test %>% mutate_all(na_if, c(""))

# blanks replaced with na.

summary(data_train)

# total na's
sum(is.na(data_train))  # 10496 na's
sum(is.na(data_test))  # 11869 more na's than test that is concerning.

# na count columnwise
data_train_missing <- data_train%>% 
  summarize(number_NAs = sum(is.na(data_train)))
head(data_train)


# Percentwise NA values
na_col_train<- summarise_all(data_train, ~(sum(is.na(.)*100 / nrow(data_msme))))
class(na_col)
ifelse(na_col_train>0,colnames(na_col_train),print('No missing Value'))

x = na_col_train %>%
  select_if(na_col_train>0) %>% mutate()
x
# Obs 
# missing value of 2.12% is same as same kind of missing value across
#"Manager_DOJ" "Manager_Joining_Designation"
#Manager_Current_Designation   Manager_Grade   Manager_Status  
#[1,] "Manager_Current_Designation" "Manager_Grade" "Manager_Status"
#Manager_Gender   Manager_DoB   Manager_Num_Application  
#[1,] "Manager_Gender" "Manager_DoB" "Manager_Num_Application"
#Manager_Num_Coded   Manager_Business   Manager_Num_Products  
#[1,] "Manager_Num_Coded" "Manager_Business" "Manager_Num_Products"
#Manager_Business2   Manager_Num_Products2   Business_Sourced  
#[1,] "Manager_Business2" "Manager_Num_Products2" "No missing Value"

# NA for test data columnwise
na_col_test<- summarise_all(data_test, ~(sum(is.na(.)*100 / nrow(data_test))))
na_col_test %>%
  select_if(na_col_test>0) %>% mutate()

# Obs
# similar pattern of na in  test data for the same variable.  missing value in test for these variable is 16% while in test its 16%
# this needs to be taken while imputed by meging test and train and according impute in the table.
# Applicant occupation in train is 3.79 % while in test its 19% thats a huge difference.

# Binding Train and test for imputation

data_master<-  rbind(data_train[,-23],data_test)
sapply(data_master, function(x) sum(is.na(x)))
str(data_master)
data_master$Application_Receipt_Date[which(is.na(data_master$Application_Receipt_Date))]
which(is.na(data_master$Application_Receipt_Date))
head(data_master$Application_Receipt_Date)
head(data_train$Application_Receipt_Date)     
      
# Converting to factor to date in the relevent columns
data_master1<- data_master %>% select(Manager_DoB,Manager_DOJ,Applicant_BirthDate,Application_Receipt_Date) %>% mutate_if(is.factor,anydate)
#data_master<- data_master %>% mutate(Manager_DoB,Manager_DOJ,Applicant_BirthDate,Application_Receipt_Date) %>% mutate_if(is.factor,anydate)
sapply(data_master, function(x) sum(is.na(x)))
summary(data_master1)
str(data_master1)

# Age function
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

sapply(data_master1, function(x) age(seq(x, length.out = 6, by = "years")))
sapply(data_master1, age(data_master1, floor = FALSE))

## testing
my.dob <- as.Date('1983-10-20')
age(my.dob, floor = FALSE)
str(my.dob)
# test 2
yr = duration(num = 1, units = "years")
yr
data_master1[, Manager_DoB := new_interval(Manager_DoB, Sys.Date())/yr][]
data_master1$Manager_DoB <- floor(age_calc(data_master1$Manager_DoB, units = "years"))

# success exp
data_master1 %>% select(Manager_DoB) %>%  
  mutate(Manager_DoB,Manager_age = as.numeric(round((Sys.Date()-Manager_DoB)/365 , digit = 0)))
# converting all columns to age
data_master_age_calc <-data_master1 %>%  
  mutate(Manager_DoB,Manager_age = as.numeric(round((Sys.Date()-Manager_DoB)/365 , digit = 0)),
         Manager_DOJ,Manager_Exp = as.numeric(round((Sys.Date()-Manager_DOJ)/365 , digit = 0)),
         Applicant_BirthDate,Applicant_age = as.numeric(round((Sys.Date()-Applicant_BirthDate)/365 , digit = 0)),
         Application_Receipt_Date,Application_age = as.numeric(round((Sys.Date()-Application_Receipt_Date)/365 , digit = 0)))

# Inserting into data_master the ages
data_master <- cbind(data_master, data_master_age_calc[,5:8])
str(data_master)
colnames(data_master)

# Dropping the date columns
data_master <-  select(data_master,-c(Manager_DoB,Manager_DOJ,Applicant_BirthDate,Application_Receipt_Date))
#data_master <-  select(data_master,-c(19:30))
str(data_master)

write.csv(data_master,"data_master_ageformat.csv",row.names = F)




##############################################################
# Missing values plot
data_master %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  xlab("Observation") +
  theme(axis.text.y  = element_text(size = 4))
###############################################################

#################################################################################
# Office pin unique values
unique(data_master$Office_PIN)
x = as.data.frame(table(data_master$Office_PIN))
str(x)
x$Freq

#library(reshape2)
#dat.new <- dcast(dat_master$, cell ~ sport, fun.aggregate = length)

fn <- function(x) {
  u <- unique.default(x)
  out <- list(x = u, freq = .Internal(tabulate(match(x, u), length(u))))
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}
y = fn(data_master$Office_PIN)
y
y<- y %>% rename(Office_PIN = x)
y

data_master1 <- data_master
colnames(data_master1)
data_master1 <- full_join(data_master1,y)
head(data_master1)

sum(is.na(data_master1$freq))
data_master1<- data_master1 %>% rename(Office_PIN_freq = freq)
data_master1<- data_master1[,-2]
summary(data_master1)


write.csv(data_master1, "data_master1_pinfreq.csv", row.names = F)
########################################################################
# other tweaking in office_pin
exp <- data_master[, c(1,2)]
summary(exp)
class(exp)
sum(is.na(exp))
exp %>% 
  group_by(as.factor(Office_PIN)) %>% 
  filter(n() == 1) %>% 
  summarise(n = n())
head(exp)
exp$Office_PIN_numeric<- data_master$Office_PIN
exp$Office_PIN <- factor(exp$Office_PIN)
exp<- exp[,2:3]
head(exp)
summary(exp)
exp <- exp%>%select_if(is.factor)%>%gather()%>%group_by(key,value)%>%summarise(count=n())%>%mutate(perc=count/sum(count))
summary(exp)
exp$count
exp1<-  exp %>% 
  select_if(is.factor) %>% 
  map(~round(prop.table(table(.x)), 2))
head(exp1)

x <-prop.table(table(data_train$Business_Sourced))  # target variable ratio 65.70% 0 and  34.29 1

exp %>%
  as_tibble() %>%
  mutate(obs = map(Freq, ~rep_len(1, .x)))

library(tidyr)
library(dplyr)
library(janitor)
exp1 <- exp %>%
  count(Office_PIN) %>%
  rename(proportion = n) %>%
  adorn_percentages("col", na.rm = TRUE, proportion) #%>%
  #adorn_pct_formatting(,,,proportion) # the commas say to use the default values of the other arguments

summary(exp1)
class(exp1)
head(exp1)
nrow(exp1)


exp %>%
  count(Office_PIN) %>%
  group_by(Office_PIN) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))

##########################################################################################################################
# similary applicant id city pin imputation with freq

exp1 <- data_master1[,c('ID',"Applicant_City_PIN")]
library(tidyverse)
anyNA(exp1)
sum(is.na(exp1$Applicant_City_PIN))
str(exp1)
class(exp1)
nrow(exp1)
which(is.na(exp1$Applicant_City_PIN))
#exp1$Applicant_City_PIN[which(is.na(exp1$Applicant_City_PIN)),] <- -999

#Replacing missing values with -999
exp1 <- exp1 %>% mutate_if(is.integer, ~replace(., is.na(.), -999))

#exp1 <- exp1 %>% mutate(Applicant_City_PIN,as.factor)
exp1$Applicant_City_PIN<- factor(exp1$Applicant_City_PIN)
exp1 <- exp1 %>% 
  count(Applicant_City_PIN)  %>% arrange(desc(n)) 
head(exp1)
#exp2 <- exp1 %>%
#  count(Applicant_City_PIN) %>%
#  group_by(Applicant_City_PIN) %>%          # now required with changes to dplyr::count()
#  mutate(prop = prop.table(n)) %>%
#  arrange(desc(prop))
#str(exp2)
anyNA(exp1)
exp1 <- exp1 %>% mutate_if(is.factor,as.numeric)

# exp2

data_master1 <- read.csv("data_master1_pinfreq.csv")
fn <- function(x) {
  u <- unique.default(x)
  out <- list(x = u, freq = .Internal(tabulate(match(x, u), length(u))))
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}
y = fn(data_master$Applicant_City_PIN)
head(y)

y<- y %>% rename(Applicant_City_PIN_freq = freq)
y<- y %>% rename(Applicant_City_PIN = x)
head(y)

colnames(data_master1)
#data_master1 <- data_master1[,-c(22:24)]
head(data_master1)


#data_master1$Applicant_City_PIN <- data_master$Applicant_City_PIN
data_master1 <- full_join(data_master1,y)


sum(is.na(data_master1$freq))
#data_master1<- data_master1 %>% rename(Applicant_City_PIN_freq = n)
data_master1<- data_master1[,-2]
summary(data_master1)


write.csv(data_master1, "data_master1_pinfreq_appPinFreq.csv", row.names = F)
##########################################################################################################
# similary applicant id city pin imputation with freq
gg_miss_var(data_master1)
exp1 <- data_master1[,c('ID',"Applicant_Qualification")]
library(tidyverse)
anyNA(exp1)
sum(is.na(exp1$Applicant_Qualification))
str(exp1)
class(exp1)
nrow(exp1)
which(is.na(exp1$Applicant_City_PIN))

# exp2
fn <- function(x) {
  u <- unique.default(x)
  out <- list(x = u, freq = .Internal(tabulate(match(x, u), length(u))))
  class(out) <- "data.frame"
  attr(out, "row.names") <- seq_along(u)
  out
}
y = fn(data_master$Applicant_Qualification)
head(y)


################ lets not impute in this  ###################


#y<- y %>% rename(Applicant_City_PIN_freq = freq)
#y<- y %>% rename(Applicant_City_PIN = x)
#head(y)

#colnames(data_master1)
#data_master1 <- data_master1[,-c(22:24)]
#head(data_master1)


#data_master1$Applicant_City_PIN <- data_master$Applicant_City_PIN
#data_master1 <- full_join(data_master1,y)


#sum(is.na(data_master1$freq))
#data_master1<- data_master1 %>% rename(Applicant_City_PIN_freq = n)
#data_master1<- data_master1[,-2]
#summary(data_master1)


#write.csv(data_master1, "data_master1_pinfreq_appPinFreq.csv", row.names = F)
#########################################################################################
# Lets deep dive applicant occupation
y = fn(data_master1$Applicant_Occupation)
head(y)
#Significant na 2225 replace these with missing
colnames(data_master1)
exp1 <- data_master1[,c(1,4)]
str(exp1)
exp1 <- exp1 %>% mutate_if(is.character, ~replace(., is.na(.), "Missing"))

table(exp1$Applicant_Occupation)
anyNA(exp1)
anyNA(data_master1$Applicant_Occupation)
data_master1$Applicant_Occupation <- exp1$Applicant_Occupation


summary(data_master1)
gg_miss_var(data_master1)
summary(data_master1)
table(data_master1$Applicant_Qualification)
sum(is.na(data_master1$Applicant_Occupation))


write.csv(data_master1, "data_master1_pinfreq_appPinFreq_appOcc.csv", row.names = F)
######################################################################################

# Other calculation
data_master1 %>% select(Manager_DoB) %>% mutate(Manager_DoB, floor(age_calc(data_master1$Manager_DoB, units = "years")))
#":="   package data.table
library(data.table)
install.packages('anytime')
library(anytime)
library(lubridate)
sum(is.na(data_master))  # 22365
#sum(complete.cases(data_master))
library(ggplot2)
library(naniar)
# %wise missing value
gg_miss_var(data_master1, show_pct = T)
gg_miss_var(data_master, show_pct = T)
miss_var_summary(data_master1)
miss_var_summary(data_master)

data_master %>% 
  group_by(Manager_Joining_Designation) %>%
  summarise(Count=n(), .groups = 'drop')
summary(data_master)
str(data_master)
summary(data_master$Applicant_Occupation)  # considerable amount of na's
library(gridExtra)
p <-
  data_master %>%
  #filter(variable == 'Applicant_Occupation') %>%
  ggplot() +
  
  geom_bar(aes(x = 'Applicant_Occupation', fill = pct)) +
  geom_text(aes(label = paste(pct_label, n, sep = "\n")), 
            lineheight = 0.8,
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~ variable)

x = table(data_master$Applicant_Occupation)

barplot(x$Freq)
ggplot(as.data.frame(table(data_master)), aes(x=data_master$Applicant_Occupation, y=Freq, fill=Applicant_Occupation)) + geom_bar(stat="identity")

prop.table(table(data_master$Applicant_Occupation)) # not showing NA as factor

data_master$Applicant_Occupation <- factor(ifelse(is.na(data_master$Applicant_Occupation),
                                                  'Missing',paste(data_master$Applicant_Occupation)))
prop.table(table(data_master$Applicant_Occupation))  # 15 % of missing values are there

# Coverting manager doj, manager dob, and applicant dob to age  ( Manager_DoB,Manager_DoB,Applicant_BirthDate)

data_master1<- data_master %>% mutate(Manager_DoB, as.character)
data_master1$Manager_DoB <- format(as.Date(data_master1$Manager_DoB , format="%d/%m/%Y"),"%Y/%m/%d")
data_master1$Manager_DOJ <- format(as.Date(data_master1$Manager_DOJ , format="%d/%m/%Y"),"%Y/%m/%d")
data_master1$Applicant_BirthDate <- format(as.Date(data_master1$Applicant_BirthDate, format="%d/%m/%Y"),"%Y/%m/%d")
str(data_master)

exp1 <-data_master1 %>% mutate_if(is.numeric.Date, as.character)
str(exp1)
inx <- sapply(exp1, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
exp1[inx] <- lapply(exp1[inx], as.character)
str(exp1)
summary(exp1)
install.packages('eeptools')
library(eeptools)
exp1 <- exp1 %>% select(Manager_DoB,Manager_DOJ,Applicant_BirthDate,Application_Receipt_Date) 
yourdata$age <- floor(age_calc(yourdata$birthdate, units = "years"))

parse_date_time(x = exp$date,
                orders = c("d m y", "d B Y", "m/d/y"),
                locale = "eng")
library(lubridate)
exp1$Manager_DoB  <-  format(parse_date_time(bind$Date.of.Record, orders = "ymd"), "%Y%m%d")
head(exp1)
str(exp1)
exp1<- exp1 %>% mutate_if(is.character, as.Date)
str(MSME_Data_Modified_v1.5_clean_without_NA)
########################################################################################################

##############################################################
# Median Impute
# log transformation
vis_miss(data_master1, cluster = TRUE)

# data master 1 is not havinf the target variale
data_master1_recipe <- recipe(Balan ~ ., data = ames_train) %>%
  step_log(all_outcomes())
ames_recipe %>%
  step_medianimpute(Gr_Liv_Area)

# exp2
miss_var_summary(data_master1)
gg_miss_var(data_master1)

impute_median <- function(x){
  ind_na <- is.na(x)
  x[ind_na] <- median(x[!ind_na])
  as.numeric(x)
}


str(data_master1)
dat %>% 
  group_by(Customer_id) %>% 
  mutate_at(vars(a, b), impute_median)


###############################################################
# KNN impute
#ames_recipe %>%
#  step_knnimpute(all_predictors(), neighbors = 6)
library(DMwR)
# KNN imputation
data_train1 <- data_master1[1:9527,]
data_test1 <- data_master1[9528:14572,]
data_master2 <- kNN(data_train1,data_test1, k = 5, prob = T) # perform knn imputation.
data_master2<- kNN(data_master1)
anyNA(data_msme_impute_knn)
#Check for missing
sapply(data_msme_impute_knn, function(x) sum(is.na(x)))

##################################################################
# Tree based Impute
ames_recipe %>%
  step_bagimpute(all_predictors())

######################################################################
# Impute with MIce
str(data_master1)

data_master2 <- data_master1[,-1] %>% dplyr::mutate_if(is.character,as.factor)
str(data_master2)
#data_master2$Manager_Grade <- factor(data_master2$Manager_Grade)
#data_master2$Manager_Num_Application <- factor(data_master2$Manager_Num_Application)
#data_master2$Manager_Num_Coded <- factor(data_master2$Manager_Num_Coded)
#summary(data_master2)
#str(data_master2)
#table(data_master$Manager_Business)

data_master2_numeric <-  dplyr::select_if(data_master2, is.numeric)
str(data_master2_numeric)

data_master2_cat <- dplyr::select_if(data_master2, is.factor)
str(data_master2_cat)

# Imputing in numeric data set
library(mice)
init = mice(data_master2_numeric,m = 5, method = "pmm", maxit = 50, seed = 500)
#meth = init$method
#predM = init$predictorMatrix
data_master2_numeric_comp<- complete(init)


# Imputing in categorical variable
init_cat = mice(data_master2_cat,m = 5, method = "polyreg", maxit = 50, seed = 500)
#meth = init$method
#predM = init$predictorMatrix
data_master2_cat_comp<- complete(init_cat)

# Merging the data set
data_master3 <- cbind(data_master2_numeric_comp, data_master2_cat_comp)


#Check for missing
sapply(data_master3, function(x) sum(is.na(x)))
write.csv(data_master3,"data_master3_imputed_all.csv", row.names = F)
############################################################################################
# One hot encoding
# dummify the data
library(caret)
dmy <- dummyVars(" ~ .", data = data_master3)
data_master3_ohe <- data.frame(predict(dmy, newdata = data_master3))
dim(data_master3_ohe) ## 14572 54 cols
str(data_master3_ohe)


# Dividing the train and test
data_train1_withfactorvar <- data_master3[1:9527,]
data_test1_withfactorvar <- data_master3[9528:14572,]

data_train1_withfactorvar$Business_Sourced<- data_train$Business_Sourced
write.csv(data_train1_withfactorvar,"train_imputed_clean_withfactorvar.csv", row.names = F)
write.csv(data_test1_withfactorvar,'test_imputed_clean_withfactorvar.csv', row.names=F)


data_train1_ohe <- data_master3_ohe[1:9527,]
data_test1_ohe <- data_master3_ohe[9528:14572,]

data_train1_ohe$Business_Sourced<- data_train$Business_Sourced
write.csv(data_train1_ohe,"train_imputed_clean_ohe.csv", row.names = F)
write.csv(data_test1_ohe,'test_imputed_clean_ohe.csv', row.names=F)

#############################################################################################

# Modelling

# Model comparision with factor varaibles
library(tidymodels)
set.seed(42)
tidy_split <- initial_split(data_train1_withfactorvar, prop = .999, strata = Business_Sourced)
train_data <- training(tidy_split)
tidy_k_folds <- vfold_cv(train_data)
tidy_k_folds
data_train1_withfactorvar$Business_Sourced<- factor(data_train1_withfactorvar$Business_Sourced)

# Pre-processing data 
tidy_rec <- recipe(Business_Sourced~., data = data_train1_withfactorvar) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
tidy_rec %>% prep()


# Create models 
baseline_model <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")
randomForest_model <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")
XGBoost_model <- boost_tree() %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

# Fit resamples
logistic_res <- fit_resamples(baseline_model, tidy_rec, tidy_k_folds)
randomForest_res <- fit_resamples(randomForest_model, tidy_rec, tidy_k_folds)
XGBoost_res <- fit_resamples(XGBoost_model, tidy_rec, tidy_k_folds)


# Create a tibble of model results 
model_res <- tibble(model = list(logistic_res, randomForest_res, XGBoost_res),
                    model_name = c("logistic", "randomForest", "XGBoost"))
model_res
# Create a helper function for collecting the metrics 
map_collect_metrics <- function(model){
  
  model %>% 
    select(id, .metrics) %>% 
    unnest()
}
# Apply helper function and extract the metrics 
model_res <- model_res %>% 
  mutate(res = map(model, map_collect_metrics)) %>% 
  select(model_name, res) %>% 
  unnest(res)
model_res


# PLots
model_res %>% 
  ggplot(aes(x = model_name, y = .estimate)) + 
  geom_boxplot() + 
  facet_wrap(~.metric, scales = "free_y")

model_res %>% 
  ggplot(aes(x = model_name, y = .estimate, color = id, group = id)) + 
  geom_line() + 
  facet_wrap(~.metric, scales = "free_y")

model_res %>% 
  ggplot(aes(x = .estimate, color = model_name, fill = model_name)) + 
  geom_density(alpha = .1) + 
  facet_wrap(~.metric, scales = "free_y")

model_res %>% 
  group_by(model_name, .metric) %>% 
  summarise(mean = mean(.estimate))                                 

####################################################################################################

# Naive bayes

# Normalising the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
str(data_train1_withfactorvar)
data_train1_withfactorvar$Business_Sourced<- factor(data_train1_withfactorvar$Business_Sourced)

data_train2<- data_train1_withfactorvar %>%  mutate_if(is.numeric, normalize)
data_test2<- data_test1_withfactorvar %>% mutate_if(is.numeric, normalize)
summary(data_train2)
summary(data_test2)

# So, multi collinearity does not affect the Naive Bayes
# MOdel 1 Naive Bayes
train_control <- trainControl(
  method = "cv", 
  number = 3   )
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)  )

library(caret)
# Not working
nb.m1 <- caret::train(
  Business_Sourced ~ ., data = data_train2,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid
  #,
  #preProc = c("BoxCox", "center", "scale", "pca")
)
#warnings()
# top 5 modesl
nb.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


#Simple Naive bayes
# apply NB classifier
library(klaR)
dim(data_train2)
str(data_train2)
nb.res <- NaiveBayes(Business_Sourced ~ ., data=data_train2)
# plot search grid results
plot(nb.res)
nrow(data_train2)
# predict on holdout units
nb.pred <- predict(nb.res, data_test2)
n
nrow(data_test2)
head(nb.pred$class)
nrow(as.data.frame(nb.pred$class))

submission <- read.csv("sample.csv", header =T)
submission$Business_Sourced <- nb.pred$class

confusionMatrix(nb.pred$class,data_train2$Business_Sourced)  # train accuracy # 78.38
# Test prediction
#data_pred <- predict(nb.m1,data_test2,type="raw")
write.csv(submission,"submission_nb1.csv",row.names = F)
#######################################################
## Iteration 2 Naive bayes

data_train2<- data_train1_withfactorvar #%>%  mutate_if(is.numeric, normalize)
data_test2<- data_test1_withfactorvar #%>% mutate_if(is.numeric, normalize)
# create response and feature data
features <- setdiff(names(data_train2), "Business_Sourced")
x <- data_train2[, features]
y <- data_train2$Business_Sourced

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)  ## 65% train accuracy

# prediciton
nb.pred <- predict(nb.m1, data_test2)
head(nb.pred)
submission$Business_Sourced <- nb.pred

# Test prediction
#data_pred <- predict(nb.m1,data_test2,type="raw")
write.csv(submission,"submission_nb2.csv",row.names = F)  #accuracy 50%

###################################################################################################
# KNN

ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- caret::train(Business_Sourced ~ ., data = data_train2, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knn.pred <- predict(knnFit, data_test2)
head(nb.pred)
submission$Business_Sourced <- knn.pred

# Test prediction
#data_pred <- predict(nb.m1,data_test2,type="raw")
write.csv(submission,"submission_knn1.csv",row.names = F)  #accuracy 50%

#####################################################################################################

#Logistic Regression
#```{r}

# Logistic Regression 1.1 without smote
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
#trControl$sampling<- 'smote'
fit_glm = caret::train(
  Business_Sourced ~ .,
  data = data_train2,
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)

glm.pred <- predict(fit_glm, data_test2)
head(nb.pred)
submission$Business_Sourced <- glm.pred
write.csv(submission,"submission_glm1.csv",row.names = F)  #accuracy 50%
############################################################################
data_train3<- data_train1_ohe# %>%  mutate_if(is.numeric, normalize)
data_test3<- data_test1_ohe #%>% mutate_if(is.numeric, normalize)
dim(data_train3)
dim(data_test3)
data_train3$Business_Sourced<- data_train
summary(data_train)
table(data_train$Business_Sourced)

data_train3 %>% select(Business_Sourced) %>% prop.table()
table(data_train3$Business_Sourced)
summary(data_train1_ohe)

# Bagging Random Forest 1.2
# Tune using caret
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
mtry <- sqrt(ncol(data_train3))
data_train3$Business_Sourced<- as.factor(data_train3$Business_Sourced)
rf_random <- caret::train(Business_Sourced~., data=data_train3, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
rf.pred<- rf_random %>% predict(data_test3)

head(rf.pred)
submission$Business_Sourced <- rf.pred
write.csv(submission,"submission_rf1.csv",row.names = F)
######################################################################################

# Specify the model and the parameters to tune (parnsip)
model <-
  boost_tree(tree_depth = tune(), mtry = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

summary(data_train2)
summary(data_test2)
# Specify the resampling method (rsample)
splits <- vfold_cv(data_train2, v = 2)

# Specify the metrics to optimize (yardstick)
metrics <- metric_set(accuracy)

# Specify the parameters grid (or you can use dials to automate your grid search)
grid <- expand_grid(tree_depth = c(4, 6, 8, 10),
                    mtry = c(2, 10, 50)) # You can add others

# Run each model (tune)
tuned <- tune_grid(formula = Business_Sourced ~ .,
                   model = model,
                   resamples = splits,
                   grid = grid,
                   metrics = metrics,
                   control = control_grid(verbose = TRUE))

summary(tuned)
# Check results
show_best(tuned)
autoplot(tuned)
select_best(tuned)

# Update model
tuned_model <- 
  model %>% 
  finalize_model(select_best(tuned)) %>% 
  fit(Business_Sourced ~ ., data = data_train2)

# Make prediction 
class(tuned)
class(data_train2)
predict(tuned, data_train2)
pred <- predict(tuned_model, data_test2)
head(pred$.pred_class)
#confusionMatrix(pred, cartest[,9])
submission$Business_Sourced <- pred$.pred_class
write.csv(submission,"submission_xgb1.csv",row.names = F)

###########################################################################################

data_train4<- read.csv('train.csv', header =T, na.strings = "")
data_test4<- read.csv('test.csv', header = T, na.strings = "")

sapply(data_train4, function(x) sum(is.na(x)))
#tapply(data_train4, function(x) sum(is.na(x)))
lapply(data_train4, function(x)sum(is.na(x)))
#apply(data_train4, function(x) sum(is.na(x)))

colnames(data_train4)
data_train4_na <- data_train4[which(is.na(data_train4$Manager_DOJ)),c(2:9,23)]

head(data_train4_na)

# Age calculation
data_train4_na %>% select(Applicant_BirthDate) %>%  mutate_if(is.character,as.Date) %>%
  mutate(Applicant_BirthDate,Applicant_age = as.numeric(round((Sys.Date()-Applicant_BirthDate)/365 , digit = 0)))

colnames(data_train4_na)
str(data_train4_na)
dim(data_train4_na)

#data_train4_na$Applicant_BirthDate <- as.Date(data_train4_na$Applicant_BirthDate, format="%d/%m/%Y")
#data_train4_na$Application_Receipt_Date <- as.Date(data_train4_na$Application_Receipt_Date, format="%d/%m/%Y")
library(lubridate)
library(tidyverse)
class(date_df)
date_df<- as.data.frame(date_df)
date_df <-  date_df %>% select(Applicant_BirthDate,Application_Receipt_Date) %>%
  mutate(Applicant_BirthDate, Applicant_Dob = dmy(Applicant_BirthDate),
         Application_Receipt_Date, Application_Receipt_Duration = dmy(Application_Receipt_Date))

#install.packages('chron')
#library(chron)
#ata_df <- data_train4_na %>% mutate_at(vars(Applicant_BirthDate,Application_Receipt_Date), function(x) chron(times. = x))
date_df <- data_train4_na[,c(2,5)]

date_df<- lapply(date_df, function(x)(as.factor(x)))
str(date_df)


date_df<- date_df %>%
  mutate(Applicant_BirthDate ,Applicant_age = as.numeric(round((Sys.Date()-Applicant_BirthDate)/365 , digit = 0)))

colnames(data_train1)
data_train1_withfactorvar$ID <- data_train1$ID
data_train4 <- data_train1_withfactorvar[,c(1:5,19:23)]

data_train4_na <- data_train4[which(is.na(data_train4$Manager_DOJ)),c(1:9,23)]
data_train4_na <- merge(data_train4,data_train4_na,by = 'ID')
str(data_train4_na)
dim(data_train4_na)
colnames(data_train4_na)

data_train4_na <- data_train4_na[,-c(10:23)]
data_train4_na <- data_train4_na[,-c(10:17)]

colnames(data_train2)
data_train2$ID <- data_train1$ID
str(data_train4)
str(data_train1_withfactorvar)
colnames(data_train1_withfactorvar)
data_train4 <- data_train1_withfactorvar[,c(1:5,19:23)]
rm(data_train4_na1)
data_train4_na1 <- data_train2[which(data_train4$ID==data_train2$ID),]
colnames(data_train4_na1)
##############################################################################

# data without managers data and inserting applicant age

rm(data_train4)
colnames(data_train4)
colnames(data_train)
data_train4 <- data_train[which(is.na(data_train$Manager_DOJ)),c(1:9,23)]
head(data_train4)
data_train4 <- left_join(data_train4,data_train2, by = 'ID')
dim(data_train4)
data_train4<- data_train4[,-c(11:19,24:32)]

head(data_train4)
sapply(data_train4, function(x) sum(is.na(x)))
data_train4$Office_PIN <- as.character(data_train4$Office_PIN)
data_train4$Office_PIN <- substr(data_train4$Office_PIN, 1, 2)
head(data_train4)
y<- function(x) apply(x, 2, function(x){x[is.na(x)]<-median(x, na.rm=T); x})
dim(y)
y
data_train4$Applicant_City_PIN <- ifelse(is.na(data_train4$Applicant_City_PIN),median(data_train4$Applicant_City_PIN,na.rm=T),data_train4$Applicant_City_PIN)
data_train4$Applicant_City_PIN <- substr(data_train4$Applicant_City_PIN, 1, 2)
head(data_train4)
data_train4 <- data_train4 %>% mutate_at(vars(Office_PIN,Applicant_City_PIN), as.factor)
str(data_train4)
data_train4$Business_Sourced.x<- factor(data_train4$Business_Sourced.x)
data_train4 <- data_train4 %>% mutate_at(vars(Applicant_Occupation.x), as.character)
data_train4$Applicant_Occupation.x[which(is.na(data_train4$Applicant_Occupation.x))] <- 'Missing'
data_train4 <- data_train4 %>% mutate_at(vars(Applicant_Occupation.x), as.factor)
sapply(data_train4, function(x) sum(is.na(x)))

library(magrittr) # for piping
data_train4 %<>% mutate(Applicant_Gender.x = fct_explicit_na(Applicant_Gender.x , na_level = "None"),
                        Applicant_Marital_Status.x = fct_explicit_na(Applicant_Marital_Status.x , na_level = "None"),
                        Applicant_Qualification.x  = fct_explicit_na(Applicant_Qualification.x  , na_level = "None"))

data_train4 <- data_train4[,-3]
colnames(data_train4)
str(data_train4)
write.csv(data_train4,'train_data_maagermissing.csv', row.names = F)

library(caret)
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
#trControl$sampling<- 'smote'
fit_glm = caret::train(
  Business_Sourced.x ~ .,
  data = data_train4[,-1],
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)

glm.pred <- predict(fit_glm, data_train4[,-c(1,8)])
glm.pred
table(glm.pred,data_train4$Business_Sourced.x)
confusionMatrix(glm.pred)

###############################################################

data_train5 <- data_train[which(is.na(data_train$Manager_Business)),]
head(data_train5)
colnames(data_train5)
data_train5 <- data_train5[,-c(10:22)]
data_train5[sapply(data_train5, is.numeric)] <- lapply(data_train5[sapply(data_train5, is.numeric)], function(x) ifelse(is.na(x), -999, x))
str(data_train5)
summary(data_train5)
data_train5[sapply(data_train5, is.integer)] <- lapply(data_train5[sapply(data_train5, is.integer)], function(x) ifelse(is.na(x), -999, x))
summary(data_train5)
str(data_train2)
#data_train5$App_Age <- data_train2$Applican[data_train2$ID==data_train5$ID]
#data_train5<- data_train5 %>% select(-App_Age)

#data_train5_date<- data_train5 %>% select(Applicant_BirthDate) %>%
#  mutate_if(is.factor,anydate)
str(data_train5)
data_train5 <- data_train5 %>% mutate(Applicant_BirthDate,Applicant_age = as.numeric(round((Sys.Date()-anydate(Applicant_BirthDate))/365 , digit = 0)),
                                      Application_Receipt_Date,Application_age = as.numeric(round((Sys.Date()-anydate(Application_Receipt_Date))/365 , digit = 0)))

summary(data_train5)
data_train5<- data_train5 %>% select(-Applicant_age,-Application_age)

data_train5 %<>% mutate(Applicant_Gender= fct_explicit_na(Applicant_Gender , na_level = "None"),
                        Applicant_Marital_Status= fct_explicit_na(Applicant_Marital_Status , na_level = "None"),
                        Applicant_Qualification  = fct_explicit_na(Applicant_Qualification , na_level = "None"),
                        Applicant_Occupation  = fct_explicit_na(Applicant_Occupation , na_level = "None"))


#Check for missing
sapply(data_train5, function(x) sum(is.na(x)))
library(anytime)

impute_median <- function(x){
  ind_na <- is.na(x)
  x[ind_na] <- median(x[!ind_na])
  as.numeric(x)
}

data_train5<- data_train5 %>% 
  mutate_at(vars(Applicant_age), impute_median)
#Check for missing
sapply(data_train5, function(x) sum(is.na(x)))
write.csv(data_train5, "train_data_missing_manager.csv", row.names = F)
data_train5 <- data_train5[,-c(1,6)]

table(data_train5$Business_Sourced)
data_train5$Office_PIN <- substr(data_train5$Office_PIN, 1, 1)
data_train5$Applicant_City_PIN <- substr(data_train5$Applicant_City_PIN, 1, 1)

str(data_train5)
data_train5 <- data_train5[,-c(2)]

data_train5 %<>% mutate_if(is.character,as.factor)
data_train5 %<>% mutate_if(is.integer,as.factor)
str(data_train5)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
data_train5[,8:9] <-as.data.frame(lapply(data_train5[8:9], normalize))
summary(data_train5)
#
# Creating Test data
data_test5 <- data_test[which(is.na(data_test$Manager_Business)),]
data_test4 <- data_test[which(is.na(data_test$Manager_Business)),]
str(data_test5)
dim(data_test5)  #824 22
colnames(data_test5)
data_test5 <- data_test5[,-c(10:22)]
data_test5[sapply(data_test5, is.numeric)] <- lapply(data_test5[sapply(data_test5, is.numeric)], function(x) ifelse(is.na(x), -999, x))
str(data_test5)
summary(data_train5)
data_test5[sapply(data_test5, is.integer)] <- lapply(data_test5[sapply(data_test5, is.integer)], function(x) ifelse(is.na(x), -999, x))
summary(data_test5)
data_test5 <- data_test5 %>% mutate(Applicant_BirthDate,Applicant_age = as.numeric(round((Sys.Date()-anydate(Applicant_BirthDate))/365 , digit = 0)),
                                      Application_Receipt_Date,Application_age = as.numeric(round((Sys.Date()-anydate(Application_Receipt_Date))/365 , digit = 0)))


data_test5 %<>% mutate(Applicant_Gender= fct_explicit_na(Applicant_Gender , na_level = "None"),
                        Applicant_Marital_Status= fct_explicit_na(Applicant_Marital_Status , na_level = "None"),
                        Applicant_Qualification  = fct_explicit_na(Applicant_Qualification , na_level = "None"),
                        Applicant_Occupation  = fct_explicit_na(Applicant_Occupation , na_level = "None"))

data_test5<- data_test5 %>% 
  mutate_at(vars(Applicant_age), impute_median)
data_test5$Office_PIN <- substr(data_test5$Office_PIN, 1, 1)
data_test5$Applicant_City_PIN <- substr(data_test5$Applicant_City_PIN, 1, 1)
data_test5 %<>% mutate_if(is.character,as.factor)
data_test5 %<>% mutate_if(is.integer,as.factor)
str(data_test5)  # application age is not changing
colnames(data_test5)
colnames(data_train5)
data_test5<- data_test5[,-c(1,3,6)]
data_test5[,7] <-as.data.frame(lapply(data_test5[7], normalize))
summary(data_test5)

sapply(data_test5, function(x) sum(is.na(x)))
#

write.csv(data_train5,"train_data_missingManager3.csv", row.names = F)
write.csv(data_test5,"test_data_missingManager3.csv", row.names = F)


#  KNN with normalized data
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=FALSE)
knnFit <- caret::train(Business_Sourced ~ ., data = data_train5[,-9], method = "knn", 
                       trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
data_pred <- predict(knnFit,data_test5[,-8])
data_pred_train<- predict(knnFit,data_train5[,-c(7,9)], type = 'raw')
data_pred
data_pred_train

confusionMatrix(data_pred_train,data_train5[,7])  # 64.68
  
#
# acc
(427+27)/nrow(data_train5)
sum(as.numeric(data_pred))
data_pred
barplot(prop.table(table(as.data.frame(data_pred))))
str(data_train5)
str(data_test5)
# KNN 2
library(class)
library(e1071)
set.seed(1)
data_train5_label<- data_train5[,7]
summary(data_train5_label)
tuneknn <- tune.knn(data_train5[,-c(7,9)],
                     data_train5_label,
                     k=1:50,
                     tunecontrol=tune.control(sampling = "cross"), cross = 3)
data_test_pred <- knn(train = data_train5[,-c(7,9)], test = data_test5[,-8],cl = data_train5[,7], k=15)

library(caret)
confusionMatrix(data_test_pred,
                data_test_labels, 
                positive = "M",
                mode="everything")
  
# Naive Bayes
library(naivebayes)
nb <- naive_bayes(Business_Sourced ~ ., data_train5[,-9])

data_pred <- predict(nb,data_test5[,-8],type="class")

confusionMatrix(data_pred,data$Quality[146:177], positive = "a", mode="everything")

# XGBOOST
# Fit the model on the training set
set.seed(123)
modelxgb <- train(
  Business_Sourced ~., data = data_train5[,-9], method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)


# Best tuning parameter
modelxgb$bestTune


# Make predictions on the test data
predicted.classes <- modelxgb %>% predict(data_test5[,-8])
head(predicted.classes)
plot(varImp(modelxgb))

pred_train <- modelxgb %>% predict(data_train5[,-c(7,9)])
confusionMatrix(pred_train,data_train5$Business_Sourced)  # 66 % on train data


# XGB itr 2
# Specify the model and the parameters to tune (parnsip)
library(tidymodels)
library(tidyverse)
model <-
  boost_tree(tree_depth = tune(), mtry = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

# Specify the resampling method (rsample)
splits <- vfold_cv(data_train[,-9], v = 2)

# Specify the metrics to optimize (yardstick)
metrics <- metric_set(roc_auc)

# Specify the parameters grid (or you can use dials to automate your grid search)
grid <- expand_grid(tree_depth = c(4, 6, 8, 10),
                    mtry = c(2, 10, 50)) # You can add others

# Run each model (tune)
tuned <- tune_grid(formula = factor(Business_Sourced) ~ .,
                   model = model,
                   resamples = splits,
                   grid = grid,
                   metrics = metrics,
                   control = control_grid(verbose = TRUE))

str(data_train5)
summary(tuned)
# Check results
show_best(tuned)
autoplot(tuned)
select_best(tuned)

# Update model
tuned_model <- 
  model %>% 
  finalize_model(select_best(tuned)) %>% 
  fit(factor(Business_Sourced) ~ ., data = data_train5[,-9])

# Make prediction 
#predict(tuned, factor(data_train5[,-9])
pred <- predict(tuned_model, data_train5[,-c(7,9)])
head(data.frame(pred))
confusionMatrix(pred$.pred_class, factor(data_train5[,7]))

pred_test_xgb <- predict(tuned_model, data_test5[,-9])   #  72 %
table(pred_test_xgb)
pred_test_xgb$ID <- data_test4$ID
write.csv(pred_test_xgb,'pred_test5_missingManager_xbg.csv',row.names = F)

# Logistic Regression 1.2 without smote removing Work.exp
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = twoClassSummary)
trControl$sampling<- 'smote'
fit_glm = caret::train(
  Business_Sourced ~ .,
  data = data_train5[,-9],
  method = "glm",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
data_pred <- predict(fit_glm,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 82.88 accuracy decreasd


# randomforest
#Bagging 1.1
library(rpart)
cntrl <- trainControl(method = "cv", number = 10)
control = rpart.control(minsplit = 2, cp = 0)
fit_bag<- caret::train(Business_Sourced ~ ., data = data_train5[,-9], method = "treebag",nbagg= 200, trControl = cntrl)
pred_train_rf<- fit_bag %>% predict(data_train5[,-c(7,9)])
rm(pred_test_rf)
pred_test_rf<- fit_bag %>% predict(data_test5[,-c(8)])
confusionMatrix(pred_train_rf, data_train5[,7]) # Accuracy % ovefitting  94 %
confusionMatrix(fit_bag)  #81.08 on tri
table(pred_train_rf)
class(pred_train_rf)
pred_test_rf <- as.data.frame(pred_test_rf)
dim(pred_test_rf)
str(pred_test_rf)
pred_test_rf$ID <- data_test4$ID
write.csv(pred_test_rf,'pred_test5_missingManager_rf_bagging.csv',row.names = F)


# Bagging Random Forest 1.2
# Tune using caret
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
mtry <- sqrt(ncol(data_train5[,-9]))
rf_random <- caret::train(Business_Sourced~., data=data_train5[,-9], method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
pred_test_rf2<- rf_random %>% predict(data_test5[,-8])
pred_train_rf2<- rf_random %>% predict(data_train5[,-c(7,9)])
confusionMatrix(pred_train_rf2, data_train5[,7])# Accuracy 81.26% ovefitting

pred_test_rf2 <- as.data.frame(pred_test_rf2)
pred_test_rf2$ID <- data_test4$ID
write.csv(pred_test_rf2,'pred_test5_missingManager_rf_bagging2.csv',row.names = F)


# Bagging Random Forest 1.3
# Tune using caret
# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- caret::train(Business_Sourced~., data=data_train5[,-9], method="rf", metric="Accuracy",
                              tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
pred_test_rf3<- rf_gridsearch %>% predict(data_test5[,-8])
pred_train_rf2<- rf_gridsearch %>% predict(data_train5[,-c(7,9)])
confusionMatrix(pred_train_rf2, data_train5[,7])# Accuracy 65.08% ovefitting

pred_test_rf3 <- as.data.frame(pred_test_rf3)
pred_test_rf3$ID <- data_test4$ID
write.csv(pred_test_rf3,'pred_test5_missingManager_rf_bagging3.csv',row.names = F)


######################################################################
# Boosting  1.1
Control <- trainControl(  method = "repeatedcv",  number = 10, repeats = 3)
set.seed(825)
gbmFit1 <- caret::train(Business_Sourced ~ ., data = data_train5[,-9], 
                        method = "gbm", 
                        trControl = Control,
                        verbose = FALSE)
pred_test_gbm1<- gbmFit1 %>% predict(data_test5[,-8])
pred_train_gbm1<- gbmFit1 %>% predict(data_train5[,-c(7,9)])
confusionMatrix(pred_train_gbm1, data_train5[,7])# Accuracy 66.08% ovefitting

pred_test_gbm1 <- as.data.frame(pred_test_rf3)
pred_test_gbm1$ID <- data_test4$ID
write.csv(pred_test_rf3,'pred_test5_missingManager_gbm1.csv',row.names = F)
summary(gbmFit1)
gbmFit1$bestTune
#########################################################################

# Going to main data and removing the manager missing rows and do modelling on it.

data_train6 <- data_train1_ohe[which(!is.na(data_train$Manager_Business)),]
colnames(data_train6)
data_test2_ohe <-read.csv("test_imputed_clean_ohe.csv", header = T)
data_test7 <- read.csv('test.csv',header = T)
data_test6<- data_test2_ohe[which(!is.na(data_test7$Manager_Business)),]
dim(data_test6)
colnames(data_test1)
data_test6_1<-data_test1[which(!is.na(data_test1$Manager_Business)),]
sapply(data_test6,function(x) sum(is.na(x)))


library(rpart)
cntrl <- trainControl(method = "cv", number = 10)
control = rpart.control(minsplit = 2, cp = 0)
fit_bag1<- caret::train(factor(Business_Sourced) ~ ., data = data_train6[,-56], method = "treebag",nbagg= 200, trControl = cntrl)
pred_train_bag1<- fit_bag1 %>% predict(data_train6[,-c(56)])
rm(pred_test_bag1)
dim(data_test6)
summary(data_test6)
pred_test_bag1<- fit_bag1 %>% predict(data_test6)
pred_train_bag1
table(pred_test_bag1)
data_train6$Business_Sourced
confusionMatrix(pred_train_bag1, factor(data_train6$Business_Sourced)) # Accuracy % ovefitting  99 %
#confusionMatrix(fit_bag1)  #81.08 on tri
table(pred_train_bag1)
class(pred_train_bag1)
pred_test_bag1 <- as.data.frame(pred_test_bag1)
dim(pred_test_bag1)
str(pred_test_bag1)
head(data_test6_1$ID)
pred_test_bag1$ID <- data_test6_1$ID
head(pred_test_bag1)
write.csv(pred_test_bag1,'pred_test5_withManager_bagging1.csv',row.names = F)

colnames(pred_test_bag1)
colnames(pred_test_bag1)[1] <- "Business_Sourced"
colnames(pred_test_rf)
colnames(pred_test_rf)[1]<- "Business_Sourced"
pred_test_bag2<- rbind(pred_test_bag1,pred_test_rf)
rm(pred_test_bag3)
pred_test_bag2<- pred_test_bag2[,c("ID",'Business_Sourced')]
head(pred_test_bag2)
write.csv(pred_test_bag1,'pred_test5_withManager_bagging2.csv',row.names = F)

pred_test_bag2 <- pred_test_bag2[order(pred_test_bag2$ID),]  # ordering hte column
head(pred_test_bag2)
write.csv(pred_test_bag2,'pred_test5_withManager_bagging3.csv',row.names = F)

pred_test_bag3$Business_Sourced<- pred_test_bag2$Business_Sourced
pred_test_bag3
