# HW2 - Urvi Vaidya

library(boot)
library(leaps)
library(glmnet)
setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 3/')

# Part 1.
# reading in our data
dat <- read.csv("hw3_hour.csv")

# looking at the columns & datatypes
str(dat)


# setting up the data
mnth <- as.factor(dat$mnth)
season <- as.factor(dat$season)
hr <- as.factor(dat$hr)
wkday <- as.factor(dat$wkday)
weathersit <- as.factor(dat$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
newdat <- cbind(dat[,c(15,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], dat[,c(9, 7)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], dat[,11:14])
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday, tmp_weathersit)

str(newdat)

# running our all-in regression
reg_all_in <- lm(cnt ~. , data=newdat)
summary(reg_all_in)

# wkday5 has NA in the regression results. This could be due to the fact that 
# wkday5 is linearly related or dependant to one of the other variables. 
# This co-liearity/dependency manifests itself in the form of NAs. 

# When original variable has k 
# attributes, we create Dummy variables that (k – 1) dummy variables. 
# In our case we there are 0-6 wkday variables and so any one of them can be 
# excluded. The analysis treats the missing dummy variable as a baseline with 
# which to compare all others. Thus if a day is not wkday0, wkday1, wkday2, wkday3
# or wkday4 then it is assumed to be wkday5. Thus wkday 5 is defined as the 
# absence of the other wkday variables. If we include all 6 wkday variables then 
# they will be multi-collinear.  

# dropping the wkday5 variable
newdat <- newdat[,c(1:23, 25:54)]




# Part 2. 
#  Setting up a random training data set
set.seed(92925329)
rows.train <- sample(1:nrow(newdat),round(nrow(newdat)/2))
newdat.train <- newdat[rows.train,]

# Setting up a test set
newdat.test <- newdat[-rows.train,]

# running our model_best regression on the training set
model_train_all <- lm(cnt ~ . , data = newdat.train)
sum.reg.all <- summary(model_train_all)
sum.reg.all


# MSE & RMSE
MSE.train.all <- sum(sum.reg.all$residuals^2)/sum.reg.all$df[2]
MSE.train.all
RMSE.train.all <- sqrt(sum(model_train_all$residuals^2)/8637) 
RMSE.train.all
sum.reg.all$sigma


# test data predictions based on model_train_all
test_pred_all <- predict(model_train_all, newdat.test)

# our testing residuals
test_resid_all <- newdat.test$cnt - test_pred_all

# MSE & RMSE
MSE.test.all <- sum(test_resid_all^2)
MSE.test.all
RMSE.test.all <- sqrt(sum(test_resid_all^2)/8636)
RMSE.test.all

# The R^2 for the training set is 0.687, which means the model could only explain 
# 68.7% of the variance in cnt. Therefore, the model could not explain about
# 31.3% of the variance in cnt. This is not the best model. RMSE on train is 101.24 
# and RMSE on test is 102.86. These values are very close therefore the test set did not 
# perform very badly. 



# Part 3.
model_best <- lm(cnt ~ yr + season1 + season2 + season3 + holiday + mnth3 + mnth5 + mnth8 + mnth9 + mnth10 + 
                   hr0 + hr1 + hr2 + hr3 + hr4 + hr5 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + 
                   hr15 + wkday0 + wkday1 + hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
                   weathersit3 + temp + atemp + hum + windspeed, data=newdat.train)

sum.reg.best <- summary(model_best)
sum.reg.best


# MSE & RMSE
MSE.train.best <- sum(sum.reg.best$residuals^2)/sum.reg.best$df[2]
MSE.train.best
RMSE.train.best <- sqrt(sum(sum.reg.best$residuals^2)/8649) 
RMSE.train.best
sum.reg.best$sigma

# test data predictions based on model_train_all
test_pred_best <- predict(model_best, newdat.test)

# our testing residuals
test_resid_best <- newdat.test$cnt - test_pred_best

# MSE & RMSE
MSE.test.best <- sum(test_resid_best^2)/8648 
MSE.test.best
RMSE.test.best <- sqrt(sum(test_resid_best^2)/8648)
RMSE.test.best

anova(model_best, model_train_all)

# Logically the season, time of day, weather situation, temperature and humidity 
# would affect bike rentals. I believe this is the best model because it incorporates 
# the variables with the highest significance while maintaining if not improving the
# R^2 of the model_all. Additionally we can conduct a nested f-test:
#  Ho:  Models are same
#  Ha:  Model with more variables is better
# Here P = 0.5967 > alpha = 0.05, we do not reject Ho. Thus the model with the additional variables
# did not lead to a significantly improved fit over model_best at the 0.05 level of significance
# The R^2 for the training set of model_best is 0.6867, which means the model could only explain 
# 68.67% of the variance in cnt. Therefore, the model could not explain about
# 31.33% of the variance in cnt. This is not the best model but is as good as or slightly better
# than our model_all. RMSE on train is 101.24 and RMSE on test is 102.80. These values are very close 
# therefore the test set did not perform very badly using model_best. 



# Part 4.
#  First fit the linear regression with variables from model_best using the "glm" (generalized linear model command)
glm.train.best <- glm(cnt ~ yr + season1 + season2 + season3 + holiday + mnth3 + mnth5 + mnth8 + mnth9 + mnth10 + 
                        hr0 + hr1 + hr2 + hr3 + hr4 + hr5 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + 
                        hr15 + wkday0 + wkday1 + hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
                        weathersit3 + temp + atemp + hum + windspeed, data=newdat.train)

summary(glm.train.best)

#  Check residuals/RMSE
MSE.glm.train.best <- sum(glm.train.best$residuals^2)/glm.train.best$df.residual
MSE.glm.train.best
RMSE.glm.train.best <- ((sum(glm.train.best$residuals^2)/glm.train.best$df.residual))^0.5
RMSE.glm.train.best



#  Now use "cv.glm" to perform LOOCV 
cv.err <- cv.glm(newdat.train, glm.train.best)


#  The MSEs are given in the delta variable
MSE.train.LOO <- cv.err$delta[2]
MSE.train.LOO
RMSE.train.LOO <- sqrt(MSE.train.LOO)
RMSE.train.LOO


# K-Fold Cross Validation
# K = 5
cv.err.5 <- cv.glm(newdat.train, glm.train.best, K = 5)
MSE.cv5 <- cv.err.5$delta[2]
MSE.cv5
RMSE.cv5 <- MSE.cv5^0.5
RMSE.cv5

# This CV5 RMSE is very close to LOOCV

# K = 10
cv.err.10 <- cv.glm(newdat.train, glm.train.best, K = 10)
MSE.cv10 <- cv.err.10$delta[2]
MSE.cv10
RMSE.cv10 <- MSE.cv10^0.5
RMSE.cv10

# We can see that the all our RMSE values are very close.



# Part 5. 

#  Run "best" subsets regression
#  Since there are 52 predictor variables, we will look at the highest R-squared 
# models up to 52 variables
regfit.full <- regsubsets(cnt ~ ., newdat.train, nvmax = 52, really.big = T)
sum.regfit.full <- summary(regfit.full)

# plot to see which is best
plot(sum.regfit.full$adjr2)
which.max(sum.regfit.full$adjr2)


# Although model 43 has the highest R^2 we will use the 29th model. The R^2 appears 
# to taper off around the 29th model, therefore adding any more variables may not 
# really increase the performance. 
# we choose a simple model with less variables because in this particular case more
# variables is only marginally improving the R^2. Therefore we can perhaps achieve 
# the same fit with the smaller model. 

newdat.test.mat <- model.matrix(cnt~.,data = newdat.test)
coef29 <- coef(regfit.full,29)
yhat29 <- newdat.test.mat[,names(coef29)] %*% coef29
MSE.bs29 <- mean((newdat.test$cnt - yhat29)^2)
RMSE.bs29 <- MSE.bs29^0.5
RMSE.bs29

# comparing with 43 
coef43 <- coef(regfit.full,43)
yhat43 <- newdat.test.mat[,names(coef43)] %*% coef43
MSE.bs43 <- mean((newdat.test$cnt - yhat43)^2)
RMSE.bs43 <- MSE.bs43^0.5
RMSE.bs43
# we can see that the RMSE of model 43 is in fact lower however, I will
# prefer the smaller model for now.



# Part 6.
# stepwise regression - forward
regfit.fwd <- regsubsets(cnt~.,data=newdat.train, nvmax = 52,
                         method = "forward")
sum.regfit.fwd <- summary(regfit.fwd)
which.max(sum.regfit.fwd$adjr2)


# stepwise regression - backward
regfit.bkwd <- regsubsets(cnt~.,data=newdat.train, nvmax = 52,
                         method = "backward")
sum.regfit.bkwd <- summary(regfit.bkwd)
which.max(sum.regfit.bkwd$adjr2)

# we can see that best model using forward selection is model 46 whereas the best
# model using backward selection is 43. Model 43 was 'best' according to
# regsubsets as well. Models using forward and backward stepwise regression are 
# different because they use different methods of feature selection. Forward adds 
# variables at every step to increase r value whereas backwards removes a variable 
# at each step to cause least amount of loss to the r2 value. Therefore, the best model
# may not be the same according to both methods. 


# Test RMSE for the above 2 models
# using which.max we get Model 46 as the best for forward selection
coef46 <- coef(regfit.fwd,46)
yhat46 <- newdat.test.mat[,names(coef46)] %*% coef46
MSE.fwd46 <- mean((newdat.test$cnt - yhat46)^2)
RMSE.fwd46 <- MSE.fwd46^0.5
RMSE.fwd46

# using which.max we get Model 43 as the best for backward selection
coef43b <- coef(regfit.bkwd,43)
yhat43b <- newdat.test.mat[,names(coef43)] %*% coef43
MSE.bkwd43 <- mean((newdat.test$cnt - yhat43)^2)
RMSE.bkwd43 <- MSE.bkwd43^0.5
RMSE.bkwd43




# Part 7.
# Set up our data into y and X for train
y <- newdat.train$cnt
X <- model.matrix(cnt ~ ., newdat.train)[,-1]

# Set up our data into y and X for test
y.test <- newdat.test$cnt
X.test <- model.matrix(cnt ~ ., newdat.test)[,-1]


# Cross-validation to determine the best lambda 
cv.out <- cv.glmnet(X, y, alpha = 0)
plot(cv.out)

#  getting the best performing lambda.
bestlam = cv.out$lambda.min
bestlam
log(bestlam)

# the best lambda is 7.438359 and the log(bestlam) is 2.00665

ridge.train <- glmnet(X, y, alpha = 0, lambda = bestlam, thresh = 1e-12)

#  Using the best value of lambda to estimate the test MSE & RMSE
ridge.pred <- predict(ridge.train, s=bestlam, newx = X.test)
MSE.ridge.test <- mean((ridge.pred-y.test)^2)
RMSE.ridge.test <- MSE.ridge.test^0.5
RMSE.ridge.test



# Part 8. 
# finding best lambda using lasso
cv.out1 <- cv.glmnet(X, y, alpha = 1)
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min
bestlam1
log(bestlam1)

# the best lambda is 0.05759251 and the log(bestlam) is -2.854363

lasso.train <- glmnet(X, y, alpha=1, lambda=bestlam1, thresh = 1e-12)

#  Using the best value of lambda to estimate the test MSE & RMSE
lasso.pred <- predict(lasso.train, s=bestlam1, 
                      newx = X.test)
MSE.lasso.test <- mean((lasso.pred-y.test)^2)
RMSE.lasso.test <- MSE.lasso.test^0.5
RMSE.lasso.test



# Part 9.
# table showing all RMSE values found for training
RMSE.table <- data.frame(matrix(0,1,13))
names(RMSE.table) <- c("RMSE.train.all", "RMSE.test.all", "RMSE.train.best",
                       "RMSE.test.best", "RMSE.train.LOOCV", "RMSE.5-Fold", 
                       "RMSE.10-Fold", "tsetRMSE.regsubsets.29", "testRMSE.regsubsets.43",
                       "testRMSE.fwd46", "testRMSE.bkwd43", "RMSE.ridge.test",
                       "RMSE.lasso.test")

RMSE.table[1,] <- c(RMSE.train.all, RMSE.test.all, RMSE.train.best, 
                    RMSE.test.best, RMSE.train.LOO, RMSE.cv5, RMSE.cv10,
                    RMSE.bs29, RMSE.bs43, RMSE.fwd46,RMSE.bkwd43,
                    RMSE.ridge.test, RMSE.lasso.test)


# lets look at all our RMSE values
RMSE.table


##   RMSE.train.all  RMSE.test.all  RMSE.train.best  RMSE.test.best  RMSE.train.LOOCV
##        101.2431       102.861        101.2326       102.8012         101.4955
##   RMSE.5-Fold    RMSE.10-Fold   tsetRMSE.regsubsets.29   testRMSE.regsubsets.43
##    101.4132        101.5067               103.2841               102.5314
##   testRMSE.fwd46   testRMSE.bkwd43   RMSE.ridge.test   RMSE.lasso.test
##     102.5116           102.5314        103.2512        102.5622



# all the RMSE values are similar and not that far off from each other. Some 
# automated model selection techniques worked better than others. While ridge
# and lasso were close, they did not perform as well as the other methods.
# LOOCV, and k-fold cross validation & stepwise selection performed slightly better.
# When we look at our model best test results, we can say that although
# automated model selection techniques are useful, in this particular case our
# model best was relatively good when compared to the automated model selection
# methods.



# Part 10.
# Lesson 1: The RMSE is similar for most of our models including our base model 
# and the R^2 does not fluctuate very much. A low RMSE and high R^2 are needed 
# for our model to be considered good.

# Lesson 2: we need additional variables to better forecast bike rentals. The 
# highest R62 is about 0.68 which does not satisfactorily explain the variance 
# in bike rentals. Even after using all the variables we were not able to achieve 
# a decent R^2.

# Lesson 3: Using automated model selection has its pros and cons. We are able
# to build a model however, in certain situations it may not be the best course 
# of action. Sometimes it may be necessary to include variables in the regression
# just for better logic/presentation, even if they are not significant. 



