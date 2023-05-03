# HW2 - Urvi Vaidya

library(ggplot2)
setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 2/')

# Part a.
# reading in our data
dat <- read.csv("hw2_credit_card_balance_new.csv")

# looking at the columns & datatypes
str(dat)

# we can see that gender, student & married are chr type and need to be 
# converted to numeric/binary ehnicity has categories therefore we need 
# to create dummy variables 

# converting gender, student, married to binary
dat$Gender <- as.numeric(as.factor(dat$Gender)) - 1
# renaming the gender column
names(dat)[8] <- "Female" 

dat$Student <- as.numeric(as.factor(dat$Student)) - 1
dat$Married <- as.numeric(as.factor(dat$Married)) - 1

# converting ethnicity variable to factors
Eth <- as.factor(dat$Ethnicity)
tempEth <- data.frame(model.matrix(~Eth - 1))
dat1 <- cbind(dat, tempEth)

# removing the extra ethnicity & obs_number columns and completing data setup
# balance is now our first column
newdat <- dat1[,c(12, 2:10, 13:16)]

rm(tempEth,Eth,dat1)
str(newdat) # checking our final data



# Part b.
cor(newdat) 
# correlation between between all independent variables & balance
cor(newdat)[1,]

# rating, limit & income, in that order, are the top 3 most strongly correlated variables with 
# credit card Balance. This seems fairly normal, considering people with higher income generally
# have a higher credit rating which gives them a higher credit limit which would explain the 
# higher end of month credit card balance. However, we also know that correlation does not necessarily
# imply causation. We simply know that some variables that are have a high positive correlation value
# (closer to 1) to balance have a positive impact on balance whereas some variables that have a high negative
# value closer to -1 have a negative impact on balance and the variables that are closer to 0 have no
# impact on balance.


# Part c.
# regression with top 3 most correlated variables
model1 <- lm(Balance ~ Income + Limit + Rating, data=newdat)
summary(model1)


# we can see that the R^2 value is 0.8657, thus, this model is able to explain 86.57% of the variance in 
# credit card balance. Therefore, about 23.43% of the variance is not explained by our current model, which
# is not too bad. Additionally, as per this model, for each unit increase in income the balance goes
# decreases by 7.5505 dollars, for each unit increase in limit the balance increases by 0.0827 cents and
# for each unit increase in rating the balance increased by 2.6747 dollars. Income and Rating are a significant 
# variables at the 0.001 level of significance & Limit is a signififcant variable at the 0.05 level of significance.


# Part d. 
# regression with all the variables
model_all <- lm(Balance ~ ., data=newdat)
summary(model_all)

# the R^2 value is 0.9443, thus, this model is able to explain 94.43% of the variance in credit card balance. 
# Therefore, about 5.57% of the variance is not explained by model_all, which is not bad at all.
# Income, Limit, Num_Cards, Student, Eth1, and Eth3 are a significant variables at the 0.001 level of 
# significance Rating & Eth2 are significant variables at the 0.01 level of significance. Female(Gender)
# and Eth4 are significant variables at the 0.05 level of significance. 


# nested f-test
# we will not do it by hand and will use the anova model
anova(model1, model_all)

#  Ho:  Models are same
#  Ha:  Model with more variables is better
# Here P = 2.2e-16 < alpha = 0.05, we reject Ho
# The result shows a Df of 10 (indicating that the more complex model has 10 additional parameters), 
# and a very small p-value (< 2.2e-16). This means that adding the additional variables to the model_all 
# did lead to a significantly improved fit over model1 at the 0.001 level of significance. This means that
# our model1 can be improved upon by adding additional variables in order to better predict Balance.



# Part e.
model_best <- lm(Balance ~ Income + Limit + Rating + Student + Num_Cards + Female + Eth1 + Eth2 + Eth3 + Eth4, data = newdat)
summary(model_best)

# we create our model_best using all the significant variables at the 0.05 level of confidence. This will 
# ensure that our model contains only the significant variables and thus give us the best results without
# burdening the model with the variables that are not significant. 


# f-test
anova(model_best, model_all)
#  Ho:  Models are same
#  Ha:  Model with more variables is better
#  P.value = 0.4519 is NOT less than alpha = 0.05, we Fail to Reject Ho.
# The result shows a Df of 3 (indicating that the more complex model has 3 additional parameters), 
# and a p-value = 0.4519. This means that having the additional variables in model_all 
# did not lead to a significantly improved fit over model_best at the 0.05 level of significance. 
# We do not have evidence that the Big model is better than the Small model. Thus we say, the Small model 
# (model_best) is just as good as the Big model (model_all).

# 95% prediction interval for observation 20 using model_best
predict(model_best, newdat[250,], interval = "prediction", level = 0.95)
# output:
# fit      lwr      upr
# 639.7683 422.6494 856.8872

# According to our model_best, for observation 250, the balancet is predicted to be 639.7683 with a lower 
# and upper bound of 422.6494 and 856.8872 respectively at a 0.05 level of significance.


# Part f.
# we will use our model_best as the base for creating both our models

# plotting Balance with some of the other variables in model best
plot(newdat)
ggplot(dat, aes(Income, Balance)) + geom_point() + geom_smooth()
ggplot(dat, aes(Limit, Balance)) + geom_point() + geom_smooth()
ggplot(dat, aes(Rating, Balance)) + geom_point() + geom_smooth()

# we can see from the graphs that limit has the best polynomial relationship with Balance. 
model_poly <- lm(Balance ~ Income + I(Limit^2) + Rating + Student + Num_Cards + Female + Eth1 + Eth2 + Eth3 + Eth4, data = newdat)
summary(model_poly)

# Our regression shows that Limit^2 is a significant to the model with a p-value  2e-16 at the 0.001 level of significance.


model_interaction <- lm(Balance ~ Income + Rating*Limit + Student + Num_Cards + Female + Eth1 + Eth2 + Eth3 + Eth4, data = newdat)
summary(model_interaction)
# for the interaction model I used the model_best and used Rating*Limit as I think that people who have
# higher rating have more limit vs people who have lower rating and thus lower limit would affect their balance.
# a higher rating*limit would mean a higher balance as opposed to people who have a lower rating and thus lower limit.
# Our regression shows that the interaction variable is significant to the model with p-value of < 2e-16 at the 0.001 level
# of significance. 



# Part g.
#  Setting up a random training data set
set.seed(123456)
rows.train <- sample(1:450,300)
newdat.train <- newdat[rows.train,]

# Setting up a test set
newdat.test <- newdat[-rows.train,]

# running our model_best regression on the training set
model_train <- lm(Balance ~ Income + Limit + Rating + Student + Num_Cards + Female + Eth1 + Eth2 + Eth3 + Eth4, data = newdat.train)
summary(model_train)

# RSS
sum((model_train$residuals)^2)
# MSE
mean(model_train$residuals^2) 
# RMSE
sqrt(mean(model_train$residuals^2)) 


# comparing to model_best
# RSS
sum((model_best$residuals)^2)
# MSE
mean(model_best$residuals^2) 
# RMSE
sqrt(mean(model_best$residuals^2)) 

summary(model_best)

# When we compare model_train with model_best we see the following:
# The variable Female(Gender) becomes not significant and variable Eth4 reduces 
# significance in model_train.
# The R^2 has increased a little bit when using only the training set. We can observe that the
# RMSE of the model_train is a little higher than model_best which means that the model fits
# better when using the whole data set.


# Part h.
yhat.test <- predict(model_train, newdat.test)

# our testing residuals
test_resid <- newdat.test$Balance - yhat.test

# RSS
sum(test_resid^2)
# MSE
mean(test_resid^2) 
# RMSE
sqrt(mean(test_resid^2)) 

# when we run our model_best on the testing dataset we see that the RMSE(112.56) is 
# higher that the RMSE(106.61) of the training dataset. Our model_train has fewer parameters
# and fewer datapoints which has a higher bias. However, our model should perform better on the
# test data resulting in lower variance. The model fits better on the training dataset
# as compared to the test dataset.


# Part i. 
# running our model_all regression on the training set
model_train_all <- lm(Balance ~ ., data = newdat.train)
summary(model_train_all)

# RSS
sum((model_train_all$residuals)^2)
# MSE
mean(model_train_all$residuals^2) 
# RMSE
sqrt(mean(model_train_all$residuals^2)) 


# predicting test set yhat using model_train_all
yhat.test_all <- predict(model_train_all, newdat.test)

# our testing residuals
test_resid_all <- newdat.test$Balance - yhat.test

# RSS
sum(test_resid_all^2)
# MSE
mean(test_resid_all^2) 
# RMSE
sqrt(mean(test_resid_all^2)) 

# our results using model_all are almost the same as the results we got using model_best.
# therefore our bias-variance tradeoff for model_all would not differ from that of model_best. 
# this is not surprising since as we discovered in out f-test in part e that having the additional 
# variables in model_all compared to model_best did not lead to a significantly improved fit over 
# model_best at the 0.05 level of significance. We do not have evidence that the Big model is better 
# than the Small model. Thus we say, the Small model (model_best) is just as good as the Big model 
# (model_all). Therefore the slight difference in the performance of our training and testing
# results for both model_best and model_all is not statistically significant at the 0.05 level of 
# significance. 



# Part j.

# Thus from our regression analysis using model_best we see that the variables Income and 
# Female appear to have a negative effect on Balance while the other variables have a positive
# effect on Balance. We can aslo see that all the variables in our model. except Female(gender)
# and Eth4 are statistically significant at a 0.05 level of significance. 
# I would suggest the bank manager target students as their balance tends to be higher
# by $428 on average. 
# My model_best is performing relatively well with a R^2 of 0.9473, thus model_best
# is able to explain 94.73% of the variance in balance, which is a pretty good number,
# additionally my model performed relatively well on the testing set as well.
# Therefore I am sufficiently confident in using my model for predicting balance. 
# I think additional variables such as debt(other than credit card debt), job type, 
# family_size or other such variables might help us better understand customers
# lifestyle and their ability to pay credit card balance. 


