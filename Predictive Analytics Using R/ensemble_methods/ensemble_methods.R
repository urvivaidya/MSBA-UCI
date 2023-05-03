#
#  Homework 8 Answers - Classification Models Revisited
#  BANA288 Predictive Analytics
#  Murphy, 3/16/23
#
#
#
#  Q1.  Read in and transform the Bike Share data 
#     to ready it for analysis.  Then pick a linear
#     regression model
#
#
dat <- read.csv("hw8_bike_share_day.csv")
names(dat)
str(dat)
#
#  Convert variables to indicators
#
season <- as.factor(dat$season)
mnth <- as.factor(dat$mnth)
weekday <- as.factor(dat$weekday)
weathersit <- as.factor(dat$weathersit)
tmp_seas <- data.frame(model.matrix(~season - 1))
tmp_mnth <- data.frame(model.matrix(~mnth - 1))
tmp_weekday <- data.frame(model.matrix(~weekday - 1))
tmp_weathersit <- data.frame(model.matrix(~weathersit - 1))
dat1 <- cbind(dat[,c(14,4,6,10:13)],tmp_seas[,1:3],
              tmp_mnth[,1:11],tmp_weekday[,1:6],
              tmp_weathersit[,2:3])
rm(season, mnth, weekday, weathersit)
rm(tmp_seas, tmp_mnth, tmp_weekday, tmp_weathersit)
str(dat1)
#
#  Note that the "workday" variable is just the sum of 
#    the weekday variables minus the holiday variable
#    So that one was dropped during the run of the script 
#    above
#
#  Run the "all in" linear regression to check for dependencies
#    (note there are other ways to do this....)
#
reg.all <- lm(cnt ~ ., data = dat1)
summary(reg.all)
#
#  Just to see (not part of homework) try dropping
#    atemp (highly correlated with temp) 
#    weekdays even though some were significant. 
#
names(dat1)
dat2 <- dat1[,c(1:4,6:21,28:29)]
reg.all2 <- lm(cnt ~ ., data = dat2)
summary(reg.all2)
#
#  This one is close in terms of R-squared.
#    All but some months significant and 
#    R-sq = 0.8426 compared to 0.8484
#    with the "all-in" model
#
#  Check ANOVA
#
anova(reg.all2, reg.all)
#
#  Turns out the second model is not as good 
#    as the first, but still going to stick
#    with it as the overall fit, r-sq, almost
#    exactly the same.  This is likely b/c of 
#    the significant weekdays that were dropped
#
#  Compute the RMSE for the chosen model
#
sum.reg2 <- summary(reg.all2)
sum.reg2$sigma
sd(dat2$cnt)
#
#  The RMSE of the second model is 780.
#  There error with the y = y-bar model 
#    is 1937
#  So we reduced the error by 
#
sum.reg2$sigma/sd(dat2$cnt)
#
#  about 40% by using the variables in this
#    second model
#
RMSE.reg.all2 <- sum.reg2$sigma
#
#
#  Q2.  Build a regression tree using 
#    all data
#
#install.packages("tree")
library(tree)
tree.all <- tree(cnt ~ ., data = dat1)
summary(tree.all)
tree.all
#
#  There are 9 terminal nodes or leaves on the tree
#    which means there are 8 splits.  
#
#  The "tree.all" report is the three itself.  For 
#    example, the first split is on temperature 
#    at 0.4324 (this is normalized value)
#  Using the high (39) and low temps (8) 
#    (in degrees C) this temperature is
#
t_cut <- 0.4324*(39 - -8) + - 8
t_cut
#
#  12.32 degrees C or
#
9/5 * 12.32 + 32
#
#  54.2 degrees F
#  In this location that is "hot" versus "cold"
#
#  Plot the tree
#
plot(tree.all)
text(tree.all, cex = 0.7)
#
#  The MSE is given the by mean deviance
#
summary(tree.all)
#
#  The MSE is 774600
#
774600^0.5
#
#  The RMSE is 880 for this tree
#
#  The MSE calculations for the tree are based on
#    722 degrees of freedom = 731 - 9
#    Note we substract 9 for the nine terminal nodes
#    The are analagous to the parameters for the tree.
#
yhat.tree.all <- predict(tree.all, dat1)
MSE.tree.all <- (sum((dat2$cnt - yhat.tree.all)^2))/(731 - 9)
RMSE.tree.all <- MSE.tree.all^0.5
RMSE.tree.all
sum.reg2$sigma
#
#  The RMSE is larger for the tree as compared to 
#    second regression.  It worse because the tree 
#    does not account for all the variables.
#  It is really only accounting for 6 of them. 
#
#
#  Now prune the tree.  Pick an effective number of 
#     terminal nodes (leaves).
#
prune.tree <- prune.tree(tree.all)
plot(prune.tree)
#
#  The lowest MSE occurs with 9 leaves, but 
#    one could consider 6, 7 or 8 leaves as the 
#    MSE levels flatten out there.
#    --> Choose 7.
#
prune.tree.1 <- prune.tree(tree.all, best = 7)
plot(prune.tree.1)
text(prune.tree.1, cex = 0.7)
summary(prune.tree.1)
#  
#  MSE for the pruned tree
#
yhat.prune.1 <- predict(prune.tree.1, dat1)
MSE.prune <- sum((dat2$cnt - yhat.prune.1)^2)/
  summary(prune.tree.1)$df
RMSE.prune <- MSE.prune^0.5
RMSE.prune
#
#  Compute it another way directly from
#    the pruned tree
#
sum.tree.p1 <- summary(prune.tree.1)
(sum.tree.p1$dev/sum.tree.p1$df)^0.5
#
#  The RMSE is now up to 930 with
#    the simpler, 7 node tree, versus 880
#    with the original 9 node tree, versus
#    768 with the "all-in" logistic regression.
#
#  One would likely choose the tree with 
#    best = 9 (unpruned) because the MSE is lower 
#    than the one just computed.  It is always
#    a tradeoff, expediency versus accuracy,
#    however.
#
#
#  Q3.  Set up training and test data
#    Build a new pruned tree on this data
#
set.seed(610626)
train <- sample(1:nrow(dat1), nrow(dat1)/2)
dat1.train <- dat1[train, ]
dat1.test <- dat1[-train, ]
#
#  Let's build a new tree on the training data
#
tree.train <- tree(cnt ~ ., data = dat1.train)
plot(tree.train)
text(tree.train, cex = 0.7)
summary(tree.train)
737100^0.5
#
#  Performance is not too bad on the training 
#    data set, RMSE = 859
#  Check out options for pruning
#
prune.tree.tr <- prune.tree(tree.train)
plot(prune.tree.tr)
#
#  From the graph, deviance (MSE) starts to 
#    level off around 6 terminal nodes.
#  Thus 6 or 7 terminal nodes might
#    be a good choice.  Go for 6.
#
prune.tree.2 <- prune.tree(tree.train, best = 6)
plot(prune.tree.2)
text(prune.tree.2, cex = 0.7)
summary(prune.tree.2)
853100^0.5
#
#  Error (RMSE) is back up in the 900s at 924.
#  Now check out the error on the test data
#
yhat.prune.2 <- predict(prune.tree.2, dat1.test)
MSE.prune2 <- sum((dat1.test$cnt - yhat.prune.2)^2)/
  nrow(dat1.test)
RMSE.prune2 <- MSE.prune2^0.5
RMSE.prune2
#
#  One sees that the error is beginning to grow
#    fairly large, MSE-test, here is at 1004
#
#
#  Aside on Cross-Validation of Trees
#  
#  Cost-complexity pruning performs cross-validation
#    on an existing tree with k = 10 fold cross 
#    validation to see where the sweet spot is between
#    over fitting (high variance) and under fitting
#    (high bias) trees.
#  Here cross-validation is run on both the original
#    tree fit in Q2 on all the data and on the
#    tree fit in Q4 on the training data
#
?cv.tree
cv.tree.all <- cv.tree(tree.all)
plot(cv.tree.all)
cv.tree.train <- cv.tree(tree.train)
plot(cv.tree.train)
#
#  In both cases the plots show almost no overfitting
#  That is, there is no saddle point or valley 
#    between the final tree and trees with less 
#    terminal nodes
#
#  All of this points to potentially not-pruning
#    the regression tree in this setting.  The
#    original trees fit were not that complex
#    and pruning reduced the quality of fit
#    as measured by the MSE.  Also, there is
#    no indication of overfitting here.
#
#  Compute the error on the original (unpruned) 
#    tree built from the training data
#
yhat.tree.tst <- predict(tree.train, dat1.test)
MSE.tree.tst <- sum((dat1.test$cnt - yhat.tree.tst)^2)/
  nrow(dat1.test)
RMSE.tree.tst <- MSE.tree.tst^0.5
RMSE.tree.tst
#
#  Stick with the pruned regression tree from above,
#    "prune.tree.2"  --> Lower test MSE 
#    1004 for prune.tree.2 versus 1016 for original, 
#    not pruned tree, just computed here.
#
#
#  Q4.  Set up the data for the second part of 
#     the homework.  Run "all-in" logistic 
#     regression
#
#  Read in the term deposit data
#
dat4 <- read.csv("hw8_bank_term_deposit_big.csv")
str(dat4)
#
#  Create factor for some variables
#
jb <- as.factor(dat4$job)
mari <- as.factor(dat4$marital)
ed <- as.factor(dat4$education)
cntct <- as.factor(dat4$contact)
m <- as.factor(dat4$month)
pout <- as.factor(dat4$poutcome)
#
#  Convert variables to indicators
#    Note some variables have 2 levels and some have more
#
tmp_job <- data.frame(model.matrix(~jb - 1))
tmp_marit <- data.frame(model.matrix(~mari - 1))
tmp_educ <- data.frame(model.matrix(~ed - 1))
tmp_contact <- data.frame(model.matrix(~cntct - 1))
tmp_month <- data.frame(model.matrix(~m - 1))
tmp_poutcome <- data.frame(model.matrix(~pout - 1))
dat4$loan <- as.numeric(as.factor(dat4$loan)) - 1
dat4$default <- as.numeric(as.factor(dat4$default)) - 1
dat4$housing <- as.numeric(as.factor(dat4$housing)) - 1
dat4$deposit <- as.numeric(as.factor(dat4$deposit)) - 1
#
#  Take care of “pdays”
#
pdaysnew <- ifelse(dat4$pdays != -1, dat4$pdays, 0)
#
#  Bind stuff together in a new data frame
#
names(dat4)
dat4 <- cbind(dat4[,c(17,1,5:8,10,12:13,15)],
              tmp_job[,1:11], tmp_marit[,1:2], tmp_educ[,1:3],
              tmp_contact[,1:2], tmp_month[,1:11], 
              tmp_poutcome[,1:3],
              data.frame(pdaysnew))
names(dat4)
#
#  Get rid of junk for simplicity
#
rm(tmp_job, tmp_marit, tmp_contact,
   tmp_month, tmp_poutcome, tmp_educ)
rm(jb, mari, ed, cntct, m, pout, pdaysnew)

#
#  Set up 3000 row training and test data sets
#    with balanced rows.  Start with the training
#    set.
#   
set.seed(123456)
dat4$deposit[1:20]
dep.1 <- dat4[dat4$deposit == 1,]
dep.0 <- dat4[dat4$deposit == 0,]
train.1 <- sample(1:nrow(dep.1),1500)
train.0 <- sample(1:nrow(dep.0),1500)
dat4.train <- rbind(dep.1[train.1,],dep.0[train.0,])
#
#  Now the test set
#
dep.1.lo <- dep.1[-train.1,]
dep.0.lo <- dep.0[-train.0,]
train.1.lo <- sample(1:nrow(dep.1.lo),1500)
train.0.lo <- sample(1:nrow(dep.0.lo),1500)
dat4.test <- rbind(dep.1.lo[train.1.lo,],dep.0.lo[train.0.lo,])
#
#  Remove not needed objects
#
rm(dep.0, dep.0.lo, dep.1.lo, dep.1)
rm(train.0, train.0.lo, train.1, train.1.lo)
#
#
#  Q5.  Select a preferred regression model
#
#  Run an "all-in" logistic regression on the
#    training set
#
lreg.all <- glm(deposit ~ ., data = dat4.train, 
             family = "binomial")
summary(lreg.all)
sum.lr.all <- summary(lreg.all)
#
#  Not many significant variables, but big drop
#    in deviance.  This likely means that some
#    variables are working.  
#     --> Likely multicollinearity here
#
#Check the fit.
#
yhat.lr.all <- predict(lreg.all, dat4.train, 
                       type = "response")
yhat.lr.all.cl <- ifelse(yhat.lr.all > 0.5, 1, 0)
tab.lr.all <- table(dat4.train$deposit, yhat.lr.all.cl, 
                    dnn = c("Actual", "Predicted"))
tab.lr.all
err.lr.all <- mean(dat4.train$deposit != yhat.lr.all.cl)
err.lr.all
#
#  The error 16.7% from this "all-in" regression
#    model.  The confusion matrix shows balanced 
#    performance.
#
#  Now select a "preferred" logistic regression 
#    model
#
summary(lreg.all)
sum.lr.all$coefficients[,4] < 0.1
str(dat4.train)
dat5.train <- dat4.train[,c(1,5:6,8:9,11,14:16,18:20,23,28:39,41:43)]
dat5.test <- dat4.test[,c(1,5:6,8:9,11,14:16,18:20,23,28:39,41:43)]
lreg.5 <- glm(deposit ~ ., data = dat5.train, 
                family = "binomial")
summary(lreg.5)
sum.lr.5 <- summary(lreg.5)
#
#  Compute the fit of the second model,
#    LREG 5
#
yhat.lr.5 <- predict(lreg.5, dat5.train, 
                       type = "response")
yhat.lr.5.cl <- ifelse(yhat.lr.5 > 0.5, 1, 0)
tab.lr.5 <- table(dat5.train$deposit, yhat.lr.5.cl, 
                    dnn = c("Actual", "Predicted"))
tab.lr.5
err.lr.5 <- mean(dat5.train$deposit != yhat.lr.5.cl)
err.lr.5
#
#  The error from this more expedient logistic
#    regression is 17.8% which is up from 
#    16.7% on the "all-in" model.  Basic story
#    higher bias, but more expedient model.
#    Decision maker has to choose between these.
#
#
#  Q6.  Build a pruned classification tree 
#    predicting high ridership "High"
#
#  Classification trees require the Y variable
#    to be of data type "factor"
#
dat4.train$deposit <- as.factor(dat4.train$deposit)
dat4.test$deposit <- as.factor(dat4.test$deposit)
tree.cl.all <- tree(deposit ~ ., data = dat4.train)
summary(tree.cl.all)
#
#  This shows that 691 out of 3000 cases are mis-classified
#    which results in an error of 23.0%.  We're 
#    under performing the logistic regression already
#
plot(tree.cl.all)
text(tree.cl.all, pretty = 0)
#
#  It's a pretty simple little tree
#
#  Prune the tree
#
tree.cl.pr <- prune.misclass(tree.cl.all)
plot(tree.cl.pr)
#
#  The results of the prune by mis-classification 
#    plot show that a tree with 3 terminal nodes
#    is just as good.  
#
tree.pr3 <- prune.misclass(tree.cl.all, best = 3)
summary(tree.pr3)
plot(tree.pr3)
text(tree.pr3, cex = 0.7)
#
#  This is a really simple tree with the same error 
#    rate on the training data of 23.0%.  
#    Let's check it on the test data
#
str(dat4.test)
yhat.pr3.cl <- predict(tree.pr3, dat4.test, type = "class")
tab.pr3 <- table(dat4.test$deposit, yhat.pr3.cl, 
                    dnn = c("Actual", "Predicted"))
tab.pr3
err.pr3 <- mean(dat4.train$deposit != yhat.pr3.cl)
err.pr3
#
#  The error is 22% in the test set, better than in
#    training set, but worse the logistic regression
#    in Q5.
#  
#
#  Q7.  Apply Bootstrap Aggregation (Bagging) 
#     
#
#install.packages("randomForest")
library(randomForest)
#
#  First run bagging
#  Build 1000 trees and computed the "bagged" 
#    estimate, i.e., the average over these 1000
#    bootstrapped trees
#
bag.1000 <- randomForest(deposit ~ ., data = dat4.train, 
                            ntree = 1000, mtry = 42, 
                            importance=TRUE)
bag.1000
#
#  Compute errors on test data for bagging
#
yhat.bag <- predict(bag.1000, dat4.test)
tab.bag <- table(dat4.test$deposit, yhat.bag, 
                       dnn = c("Actual", "Predicted"))
tab.bag
err.bag <- mean(dat4.test$deposit != yhat.bag)
err.bag
#
#  The error is the lowest so far on the test data
#    at 14.2%.
#
#
#  Q8.  Apply Random Forrest Procedure version of 
#      Bootstrap Aggregation
#
#  Now try building a random forest estimator
#  Here, 1000 trees are built, but instead of considering
#    all variables a subset is chosen at random each 
#    time a tree is built.  This way the trees built
#    are uncorrelated from one another, reducing the 
#    variance of the aggregated (average) of them.
#  The James text puts the number of variables at 
#    square root of the total.  Here the total is 42,
#    so set mtry = 7
#
rf.1000 <- randomForest(deposit ~ ., data = dat4.train, 
                           ntree = 1000, mtry = 7,
                           importance=TRUE)
rf.1000
#
#  Compute the error on the test data set
#
yhat.rf <- predict(rf.1000, dat4.test)
tab.rf <- table(dat4.test$deposit, yhat.rf, 
                        dnn = c("Actual", "Predicted"))
tab.rf
err.rf <- mean(dat4.test$deposit != yhat.rf)
err.rf
#
#  The random forest error on the test set was better
#    than just bagging at 13.7%
#
#  
#  Q9.  Apply Gradient Boosting procedure
#
#install.packages("gbm")
library(gbm)
#
#  Gradient boosting requires the Y variable to 
#    a number
#
str(dat4.train)
dat4.train$deposit <- as.numeric(dat4.train$deposit) - 1
dat4.test$deposit <- as.numeric(dat4.test$deposit) - 1
str(dat4.train$deposit)
#
#  Run the boosting algorithm for 1000 steps
#  The learning rate is 0.02
#  Trees aren't grown deep in boosting.
#
boost.1000 <- gbm(deposit ~ ., data = dat4.train, 
                       distribution = "bernoulli", 
                       n.trees = 1000, 
                       interaction.depth = , 
                       shrinkage = 0.02)
summary(boost.1000)
#
#  The most important variables are listed first
#
#  Compute the classification error for boosting
#    on the test set
#
#  Once the predictions are made, they have to be 
#    converted to 0s and 1s as in the case of 
#    logistic regression
#
yhat.bst <- predict(boost.1000, dat4.test,
                      n.trees = 1000,  type = "response")
yhat.bst.cl <- ifelse(yhat.bst > 0.5, 1, 0)
#
#  Now compute confusion matrics
#
tab.bst <- table(dat4.train$deposit, yhat.bst.cl, 
                   dnn = c("Actual", "Predicted"))
tab.bst
err.bst <- mean(dat4.test$deposit != yhat.bst.cl)
err.bst
#
#  The boosted error is 15.3%.  Of the three
#    methods tried here random forests worked the 
#    best
#
#
#  Q10.  Summary and thoughts
#
#  For forecasting the "qualitative" variable "deposit", 
#    the following errors were observed
#
err.lr.all
err.lr.5
err.pr3
err.bag
err.rf
err.bst
#
#  The lowest error is about 13.7% in predicting 
#    "deposit" was based on random forests
#
#  Important variables can be identified by
#    the variable importance plot:
#
varImpPlot(rf.1000)
#
#  Duration (of call) is by far the most important variable
#  Contact by cellular, previous success, and certain other
#    variables are important, but a distant second
#
#
#
#
#  End Homework 8 Answers
#