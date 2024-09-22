# Customer churn -- 
# Sales department at AdviseInvest is facing a problem where many of customers who scheduled a sales  call does not answer the call.
# We will clean data and then use a classification tree model to estimate the probability of customers answering sales call.
# The result shows that the most important factors are customer income, and whether customer provided non‐mobile phone for follow‐up call. From the result, we could find customer segment likely to answer the call.
# - customers with income > 39000, less than 2 accounts and 1 new car 
# - customers with income > 39000, more than 2 accounts.
# - customers with income > 80000, providing no mobile for follow-up call and more than 1 saving account.
# These are the customer segment that we need to focus on for future call.

#load the library to use the function
library(tidyverse)
library(rpart)  
library(rpart.plot) 
library(readr)

#import the dataset
advise_invest <- read_csv('/Users/linhdo/Downloads/adviseinvest.csv')

#data cleaning
advise_invest <- advise_invest  |>            
  select(-product) |>                                        # Remove the product column
  filter(income > 0,                                          # Filter out mistaken data
         num_accts < 5) |> 
  mutate(answered = ifelse(answered==0, "no","yes"),          # Turn answered into yes/no 
         answered = factor(answered,                          # Turn answered into factor
                           levels  = c("no", "yes")),         # Specify factor levels
         female = factor(female),                             # Make other binary and categorical                                                                                                        # variables into factors
         job = factor(job),
         rent = factor(rent),
         own_res = factor(own_res),
         new_car = factor(new_car),
         mobile = factor(mobile),
         chk_acct = factor(chk_acct),
         sav_acct = factor(sav_acct)) 


#Calculate distribution of the target variables aka accuracy of the majority class classifier in this case
summary(advise_invest$answered)

round(mean(advise_invest$answered== 'yes'),3)
round(mean(advise_invest$answered== 'no'),3)

#fit a tree model to the outcome using income variable - income model
income_model <- rpart(formula = answered ~ income, data = advise_invest)

#plot the income model
rpart.plot(x = income_model)

#calculate accuracy
## get prediction
(predict(income_model, type = "class")==advise_invest$answered) |> 
  mean ()

(leafy_tree <- rpart(formula = answered ~., 
                     data = advise_invest,
                     maxdepth = 5,
                     minbucket = 10))

rpart.plot(x = leafy_tree, 
           tweak = 1, 
           roundint = T) 

