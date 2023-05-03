# HW7 - Urvi Vaidya

setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 7')
library(class)
library(MASS)
library(e1071)
library(tree)
library(pROC)

# Part 1.
dat <- read.csv("hw7_bank_term_deposit_big.csv")
str(dat)

#  Create factor for some variables
jb <- as.factor(dat$job)
mari <- as.factor(dat$marital)
ed <- as.factor(dat$education)
cntct <- as.factor(dat$contact)
m <- as.factor(dat$month)
pout <- as.factor(dat$poutcome)

# Convert variables to indicators
# Note some variables have 2 levels and some have more
tmp_job <- data.frame(model.matrix(~jb - 1))
tmp_marit <- data.frame(model.matrix(~mari - 1))
tmp_educ <- data.frame(model.matrix(~ed - 1))
tmp_contact <- data.frame(model.matrix(~cntct - 1))
tmp_month <- data.frame(model.matrix(~m - 1))
tmp_poutcome <- data.frame(model.matrix(~pout - 1))
dat$loan <- as.numeric(as.factor(dat$loan)) - 1
dat$default <- as.numeric(as.factor(dat$default)) - 1
dat$housing <- as.numeric(as.factor(dat$housing)) - 1
dat$deposit <- as.numeric(as.factor(dat$deposit)) - 1

# Take care of “pdays”
pdaysnew <- ifelse(dat$pdays != -1, dat$pdays, 0)

# Bind stuff together in a new data frame
names(dat)
dat1 <- cbind(dat[,c(17,1,5:8,10,12:13,15)],
              tmp_job[,1:11], tmp_marit[,1:2], tmp_educ[,1:3],
              tmp_contact[,1:2], tmp_month[,1:11], 
              tmp_poutcome[,1:3],
              data.frame(pdaysnew))
names(dat1)

# Get rid of junk for simplicity
rm(tmp_job, tmp_marit, tmp_contact,
   tmp_month, tmp_poutcome, tmp_educ)
rm(jb, mari, ed, cntct, m, pout, pdaysnew)

# lets look at our final data
str(dat1)

# Our (Y) variable is the response variable ,“deposit” - yes/no which is now 1/0.
# It is the first column in dat1. 

unique(dat$pdays)
# pdays is number of days that passed by after the client was last contacted from 
# a previous campaign (numeric; 999 means client was not previously contacted)
# Thus a value of -1 probably means that the client was contacted on the day itself.
# The transformation first creates factor variables for our categorical variables and
# then we have transformed those to numerical indicator variables.



# Part 2.
# variable correlations with deposit
cor(dat1)[1,]

# top 3 correlated variables
# duration : 0.3945210159
# poutsuccess : 0.3067882107
# cntctcellular : 0.1358729355

# duration :last contact duration, in seconds (numeric). We know from the data 
# dictionary that the duration is not known before a call is performed, however,
# after the end of the call y is obviously known. This explains why duration is 
# the most highly correlated with deposit.

# poutcome : outcome of the previous marketing campaign (categorical: 'failure',
# 'nonexistent','success') - this makes sense as a customer who was previously 
# receptive to the banks marketing activities is likelier to be receptive
# this time as well. 

# cntctcellular : if the communication type is cellular. This may not necessarily 
# be a causal relationship, however, if the contact type is cellular then perhaps
# the customer receives the marketing information more directly, therefore, he/she
# may be more likelier to get a deposit since, he/she perhaps gets more freqeunt
# marketing information.


# setting up training and testing set
# separating the dependent variable
succ <- subset(dat1, dat1$deposit == 1)
fail <- subset(dat1, dat1$deposit == 0)

# setting seed
set.seed(112233)


# Randomly selecting 2000 rows from each group of 
# of numbers (1-->5289 and 1-->39922) 
# since we have 5289 datapoints for success we will take 2000 for each train and test
train.succ <- sample(1:nrow(succ),2000)
train.fail <- sample(1:nrow(fail),2000)

# creating the training set
dat.train <- rbind(succ[train.succ,],fail[train.fail,])
# checking our train set
table(dat.train$deposit)

# Now we take a sample of 2000 of the "failure" 
# observations from the 39922 - 2000 = 37922 
# that are left over and 2000 'success' observations
# from the 5289 - 2000 = 3289 that are left over after building the 
# training data set
newfail <- fail[-train.fail,]
test.fail <- newfail[sample(1:nrow(newfail),2000),]

newsucc <- succ[-train.succ,] 
test.succ <- newsucc[sample(1:nrow(newsucc),2000),] 

# binding our test set
dat.test <- rbind(test.succ,test.fail)

#  Check results
table(dat.test$deposit)

# removing unnecessary varaibles
rm(succ, fail, test.fail, newfail, newsucc,
   loan.count.train, loan.count.test,
   train.fail, train.succ)



# Part 3.
# glm model with all independat variables
logreg_all <- glm(deposit ~ ., data = dat.train, 
              family = "binomial")

summary(logreg_all)

# we can see that the following variables are significant at the 0.05 level of significance
# duration, housing, loan, campaign, maridivorced,
# marimarried, edprimary, cntctcellular, cntcttelephone, mapr, maug
# mfeb, mjan, mjul, mjun, mmay, mnov, poutother, poutsuccess

# prediction on test set
yhat.test <- predict(logreg_all, dat.test, 
                     type = "response") 

# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)

# confusion matrix of our test set
tab.logreg_all.test <- table(dat.test$deposit, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.logreg_all.test

# overall test error 
lr_all <- mean(yhat.test.class != dat.test$deposit)
lr_all
# class 1 test error
lr_class1 <- tab.logreg_all.test[2,1]/2000
lr_class1
# class 0 test error
lr_class0 <- tab.logreg_all.test[1,2]/2000
lr_class0



# Part 4.
# glm model with significant variables at the 0.05 level of significance
# except duration because of what we know about duration
logreg_sig <- glm(deposit ~ housing + loan + campaign + maridivorced +
                    marimarried + edprimary + cntctcellular + cntcttelephone +
                    mapr + maug + mfeb + mjan + mjul + mjun + mmay + mnov +
                    poutother + poutsuccess, data = dat.train, family = "binomial")

summary(logreg_sig)


# prediction on test set
yhat.test.sig <- predict(logreg_sig, dat.test, 
                     type = "response") 

# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
yhat.test.class.sig <- ifelse(yhat.test.sig > 0.5, 1, 0)

# confusion matrix of our test set
tab.logreg_sig.test <- table(dat.test$deposit, 
                             yhat.test.class.sig, 
                             dnn = c("Actual","Predicted"))
tab.logreg_sig.test

# overall test error 
lr_sub <- mean(yhat.test.class.sig != dat.test$deposit)
lr_sub
# class 1 test error
lr2_class1 <- tab.logreg_sig.test[2,1]/2000
lr2_class1
# class 0 test error
lr2_class0 <- tab.logreg_sig.test[1,2]/2000
lr2_class0

# we can see that removing the duration variable has greatly affected our model
# accuracy. Our error rate has almost doubled. When we try this model with the
# addition of the duration variable the error rate is similar to our model from
# part 3, which is the model all.

names(dat.train)

# creating our new train and test data
new.train <- dat.train[,c(1,5,6,9,22:24,27:30,32:35,37,38,41,42)]
names(new.train)
dim(new.train)

new.test <- dat.test[,c(1,5,6,9,22:24,27:30,32:35,37,38,41,42)]
names(new.test)
dim(new.test)


# Part 5.
# LDA
lda.fit <- lda(deposit ~ ., data = new.train)
lda.fit

# plotting out lda
plot(lda.fit)

# using our lda model to make predictions on our test data
lda.pred <- predict(lda.fit, new.test)
# lda.pred$posterior
names(lda.pred)

# confusion matrix
lda.test.class <- lda.pred$class
tab.lda <- table(new.test$deposit, lda.test.class,
                 dnn = c("Actual", "Predicted"))
tab.lda

# overall test error
lda_err <- mean(new.test$deposit != lda.test.class)
lda_err
# class 1 test error
lda_class1 <- tab.lda[2,1]/2000
lda_class1
# class 0 test error
lda_class0 <- tab.lda[1,2]/2000
lda_class0


# Part 6.
# Naive Bayes
nb.fit <- naiveBayes(deposit ~ ., data = new.train)
nb.fit

# using our nb model to make predictions on our test data
nb.pred <- predict(nb.fit, new.test)

# confusion matrix
tab.nb <- table(new.test$deposit, nb.pred,
                 dnn = c("Actual", "Predicted"))
tab.nb

# overall test error
nb_err <- mean(new.test$deposit != nb.pred)
nb_err
# class 1 test error
nb_class1 <- tab.nb[2,1]/2000
nb_class1
# class 0 test error
nb_class0 <- tab.nb[1,2]/2000
nb_class0

str(new.train)

# Part 7.
# we will not scale our data in this part since almost all of our variables are categorical
# except campaign
# lets check our error for different values of K 
kval <- list(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
k_err <- 1:15
v <- 1
for(k in kval){
  knn1 <- knn(new.train[,2:18], new.test[,2:18], new.train[,1], k=k)
  knn_err <- mean(new.test[,1] != knn1)
  k_err[v] <- knn_err
  v <- v + 1
}
k_err

plot(kval, k_err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN")

# we can see that the error is lowest for k=9
# lets compute our wrror values for the best k
knnbest <- knn(new.train[,2:18], new.test[,2:18], new.train[,1], k=9)

# confusion matrix with the results on the test data.  
tab.knnbest <- table(new.test[,1], knnbest,
                  dnn = c("Actual", "Predicted"))
tab.knnbest

knnbest_err <- mean(new.test[,1] != knnbest)
knnbest_err
# class 1 test error
knnbest_class1 <- tab.knnbest[2,1]/2000
knnbest_class1
# class 0 test error
knnbest_class0 <- tab.knnbest[1,2]/2000
knnbest_class0



# Part 8.
# converting the Y-variable to a "factor" since classification tree models assume
# that the Y variable is qualitative
new.train[,1] <- as.factor(new.train[,1])
new.test[,1] <- as.factor(new.test[,1])

# building our first tree
tree1 <- tree(deposit ~., data = new.train)
summary(tree1)

# plotting our tree
plot(tree1)
text(tree1, pretty = 0)

# metadata
tree1

# making predictions on our test data using tree1
tree.pred.test <- predict(tree1, new.test, type = "class")

# confusion matrix
tab_tree1 <- table(new.test$deposit, tree.pred.test,
      dnn = c("Actual", "Predicted"))
tab_tree1

# overall error
tree1_err <- mean(new.test$deposit != tree.pred.test)
# class 1 test error
tree1_class1 <- tab_tree1[2,1]/2000
tree1_class1
# class 0 test error
tree1_class0 <- tab_tree1[1,2]/2000
tree1_class0



# Part 9. 
# pruning
prune <- prune.misclass(tree1)
names(prune)


#  Plotting the results of the prune to identify the right tree size
plot(prune)
plot(prune$size, prune$dev, xlab = "Size of Tree",
     ylab = "Deviation/Deviance")

# pruning our tree1
prune.tree1 <- prune.misclass(tree1, best = 3)
summary(prune.tree1)

# plotting our pruned tree
plot(prune.tree1)
text(prune.tree1, pretty = 0)

# making predictions on our test data using prune.tree1
pt1.pred <- predict(prune.tree1, new.test, type = "class")

# confusion matrix
tab_pt1 <-table(new.test$deposit, pt1.pred,
      dnn = c("Actual", "Predicted"))

# overall error
pt1_err <- mean(new.test$deposit != pt1.pred)
# class 1 test error
pt1_class1 <- tab_pt1[2,1]/2000
pt1_class1
# class 0 test error
pt1_class0 <- tab_pt1[1,2]/2000
pt1_class0

# The error for both our tree1 and pruned tree1 is the same. This males sense as we are
# pruning our tree and not changing it. We use tree of size 3 as that is the size at which
# the misclassification rate remains the same, there is no point adding more nodes where the 
# rate is not improved. Simpler and smaller is preferable better.


# Part 10.
# lets look at all our estimates
test.error <- data.frame(matrix(0,3,8))
names(test.error) <- c("Error" , "Logistic Reg", 'Logistic Reg Sub', "Linear Discriminant Analysis",
                            "Naive Bayes", 'KNN k=9', "Tree1", 'Pruned Tree')

test.error[1,] <- c('Overall Error', lr_all, lr_sub, lda_err, nb_err, knnbest_err,
                    tree1_err, pt1_err)
test.error[2,] <- c('Class 1 Err', lr_class1,lr2_class1, lda_class1, nb_class1, 
                    knnbest_class1, tree1_class1, pt1_class1)
test.error[3,] <- c('Class 0 Err', lr_class0, lr2_class0, lda_class0, nb_class0, 
                    knnbest_class0, tree1_class0, pt1_class0)


# lets look at our table
test.error


# Part 10.	

# looking at the errors of all our models from the above table i believe linear 
# discriminant analysis(lda) has worked best. 
# We have had better results with our logistic regression all model, however, that may 
# be attributed to the fact that we have used the duration variable in ur all model, 
# which as per the data dictionary gives the decision of the customer at the 
# end of the call, therefore it would not be fair to use it within our model. 
# One of the reasons that LDA has worked better is perhaps due to the fact that our data
# is normally distributed and our sample measurements are independent from each other. 


# Firstly, the bank should pay a little more attention to customers that have previously
# been receptive to their products/offers. We can see that poutsuccess appears to have the
# largets impact on the probability of deposit being 1. 
# Secondly, for some reason the customers contacted in jan and august appear to be least 
# likepy to open a deposit with the bank, therefore, the bank should perhaps investigate as 
# to why this is, it could be perhaps that in january people dont have as much money since 
# people tend to spend a lot during the holidays just before, and in august there are 
# higher expenses since its the start of the school year. 
# Thirdly, the bank should try to give targeted offers to customers who provide their
# cellphone number, they appear to be more receptive to the banks offers, this may be because
# the customer is reached more dicrectly, sometimes emails are easy to ignore. 


