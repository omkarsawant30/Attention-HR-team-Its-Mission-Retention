##First we need to set the directory of the file's location in your computer and then read it so that R can download the data
setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 4 HUMAN RESOURCES")

library(dplyr)
library(car)
hr_train=read.csv("hr_train.csv")
head(hr_train)
hr_test=read.csv("hr_test.csv")

##You will need same set of vars on both train and test,its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NA
hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

all= rbind(hr_train,hr_test)

apply(all,2,function(x) length(unique(x)))
glimpse(all)

#Next we'll create dummy variables for remaining categorical variables
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)

#Remove the extra varaiables added before
hr_train = all %>% filter(data == 'train') %>% select(-data) 
hr_test= all %>% filter(data == 'test') %>% select(-left, -data) 

#perform VIF and remove variables with high multicollinearity 
for_vif = lm(left ~., data = hr_train) 
vif(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif = lm(left ~.-sales, data = hr_train) 
sort(vif(for_vif),decreasing = T)[1:3]

#Use Random Forest model to predict the values
library(randomForest)

fit_hr= randomForest(as.factor(left)~.,data=hr_train)
fit_hr
importance(fit_hr)
varImpPlot(fit_hr)

score=predict(fit_hr,newdata= hr_test, type="prob")[,2]
write.csv(score,'Omkar_Sawant_P4_part2.csv',row.names = F)

#---------------------------------------QUIZ-----------------------------------------------------------------------------------
# 1.	Which modelling technique can not be used for the given problem?
# Ans: b. linear regression

# 2.	Find out total promotions happened in last 5 years
sum(hr_train$promotion_last_5years)
# Ans: 228

# 3.	Find out the variance in statisfaction_level for category 0 of variable 'left' 
# (round off to 4 decimal places).
install.packages("magrittr")
library(magrittr)
library(dplyr)
hr_train %>%
  select(satisfaction_level, left) %>%
  filter(left == 0) %>%
  group_by(left) %>%
  summarise(var(satisfaction_level))
# Ans: 0.0487

# 4.	Does average_monthly_hours follow normal distribution?
boxplot(hr_train$average_montly_hours)
hist(hr_train$average_montly_hours, 100)
# Ans: No

# 5.	Find out which category of salary has maximum employee resignation. 
hr_train %>%
  select(salary, left) %>%
  filter(left == 1) %>%
  arrange(salary) %>%
  group_by(salary) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Ans: sales

# 6.	Find out correlation coefficient between last_evaluation and 
#     average_monthly_hours (round it off to 2 decimal places).
cor(x = hr_train$last_evaluation, hr_train$average_montly_hours, method = 'pearson')
# Ans: 0.33

# 7.	According to given data what is the probability that someone will 
# leave the organisation if they were involved in a work accident? 
# (round off 2 decimal places)
hr_train %>%
  select(Work_accident, left) %>%
  filter(Work_accident == 1) %>%
  arrange(left) %>%
  group_by(left) %>%
  summarise(n = n())

270/(1245 + 270)

# Ans : 0.18

# 8.	What is the median time spent with the company among people leaving 
# the company?
median(hr_train$time_spend_company)
# Ans: 3

# 9.	Which sales category has maximum median average_monthly_hours?
#   Note: you need to write name of just that category . 
# Any further details will result in your answer being marked as wrong. 
# Answers are not case sensitive . 
hr_train %>%
  select(sales, average_montly_hours) %>%
  arrange(sales) %>%
  group_by(sales) %>%
  summarise(median_hrs = median(average_montly_hours),
            n = n()) %>%
  arrange(-median_hrs)

# Ans :It's a tie between accounting and sales. Both are 200.

# 10.	Does number of projects significantly differ between two categories of 
#  the target variable "left"?
#   Note : Just write 'Yes' or 'No' . If you write sentences , automated 
# grading will consider it incorrect . Answers are not case sensitive . 

hr_train %>%
  select(left, number_project) %>%
  arrange(left) %>%
  group_by(left) %>%
  summarise(avg_project = mean(number_project),
            med_project = median(number_project)) %>%
  arrange(desc(avg_project))

# Ans: No








