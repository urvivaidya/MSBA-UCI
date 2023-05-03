setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/Final Project')

library(class)
library(MASS)
library(e1071)
library(tree)
library(pROC)
library(smotefamily)
library(boot)
library(reshape2)
library(ggplot2)
library(glmnet)

options(scipen=999)

# Part 1
# EXPLORATORY EDA AND SETTING UP TRAIN AND TEST SET
dat <- read.csv("bankrupcy_data.csv")
table(dat$bankrupt)

# we drop the repeated and redundant columns to prevent multi-colinearity
# based on domain knowledge, since several measures correspond to the same thing
# but have been recoded in different ways for accounting purposes.
dat <- dat[,c(1,7:16,19:22, 25:38,40,41,45:77,83, 86, 87:92, 94)]
# str(dat)

# setting up training and testing set
# separating the dependent variable
yes <- subset(dat, dat$bankrupt == 1)
no <- subset(dat, dat$bankrupt == 0)

# setting seed
set.seed(112233)

# Randomly selecting 1550 rows from both groups of 
# of numbers (1-->50 and 1-->1500) 
test.yes <- sample(1:nrow(yes),50)
test.no <- sample(1:nrow(no),1500)

# separating our testing set
dat.test <- rbind(yes[test.yes,],no[test.no,])
# checking our train set
table(dat.test$bankrupt)


# creating the training set
newyes <- yes[-test.yes,]
newno <- no[-test.no,] 

# our base train set
train_og <- rbind(newyes,newno)

#  Check results
table(train_og$bankrupt)


# bootstrapped train set (with replacement)
train.newyes <- sample(1:nrow(newyes),500, replace=TRUE)
train.newno <- sample(1:nrow(newno),3000)

# our base train set
train_boot <- rbind(newyes[train.newyes,],newno[train.newno,])

#  Check results
table(train_boot$bankrupt)


# ADAS bootstrapping - Generate synthetic positive instances using ADASYN algorithm
adas_train <- ADAS(train_og,train_og$bankrupt,K=5)
train_adas <- adas_train$data

# dropping the last class column since its the same as bankrupt
train_adas <- train_adas[,c(1:73)]

# lets look at our synthetic values
# adas_train$syn_data

# checking our distribution
table(train_adas$bankrupt)

# removing unnecessary variables
rm(yes, no, test.yes, test.no, newyes, newno, train.newyes,
   train.newno)


# we run an initial log_reg to identify significant variables
glm_var <- glm(bankrupt ~ ., data=train_og, family='binomial')
summary(glm_var)

# we can see that all our variables are highly significant, this is not that
# surprising since most of our data is financial/account related. Lets look at 
# some correlations

# variable correlations with deposit for our original data
cor(train_og)[1,]
# looking at the the variables correlated at 10%
cor(train_og)[1,abs(cor(train_og)[1,])>0.1]

# heatmap
cormat <- round(cor(train_og),2)[,abs(cor(train_og)[1,])>0.1]
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# variable correlations with deposit for our bootstrap data
cor(train_boot)[1,]
# looking at the the variables correlated at 10%
cor(train_boot)[1,abs(cor(train_boot)[1,])>0.1]

# heatmap
cormat1 <- round(cor(train_boot),2)[,abs(cor(train_boot)[1,])>0.1]
melted_cormat1 <- melt(cormat1)
head(melted_cormat1)

ggplot(data = melted_cormat1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


# variable correlations with deposit for our adas data
cor(train_adas)[1,]
# looking at the the variables correlated at 10%
cor(train_adas)[1,abs(cor(train_adas)[1,])>0.1]

# heatmap
cormat2 <- round(cor(train_adas),2)[,abs(cor(train_adas)[1,])>0.1]
melted_cormat2 <- melt(cormat2)
head(melted_cormat2)

ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


# we are mainly concerned with the variables which are correlated to bankrupt
# at 0.1 and more, thus we only plot heatmaps for those variables, to make sure
# our model doesnt not have any multi-colinearity. We can see that there is some
# correlation, bit its not significant enough for us to worry about for now. 
# thus based on the above correlations we will use the common variables that are 
# correlated to bankrupt for all the 3 datasets, so we can have comparable
# results. We have 18 variables that are common in the 3 training datasets. 
# 'X15', 'X18','X19','X37','X40','X52', 'X54','X60','X65','X66','X68','X70','X82','X85','X86','X89','X90','X91'

train_og <- train_og[,c(1,11,12,13,29,31,40,42,48,53,54,56,58,65,66,67,70,71,72)]
train_boot <- train_boot[,c(1,11,12,13,29,31,40,42,48,53,54,56,58,65,66,67,70,71,72)]
train_adas <- train_adas[,c(1,11,12,13,29,31,40,42,48,53,54,56,58,65,66,67,70,71,72)]

# since we have removed these columns for our training data we will match out test set as well.
dat.test <- dat.test[,c(1,11,12,13,29,31,40,42,48,53,54,56,58,65,66,67,70,71,72)]



# Part 2 
# LOGISTIC REGRESSION

# part 2a
# on our original train data
glm_base <- glm(bankrupt ~ ., data=train_og, family='binomial')
summary(glm_base)

# prediction using our train set for error
yhat.train.lr <- predict(glm_base, train_og, 
                            type = "response") 

# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
lr_yhat.train.class <- ifelse(yhat.train.lr > 0.5, 1, 0)

# confusion matrix of our train set
tab.lr_train <- table(train_og$bankrupt, 
                      lr_yhat.train.class, 
                      dnn = c("Actual","Predicted"))
tab.lr_train

# overall train error 
lr_train <- mean(lr_yhat.train.class != train_og$bankrupt)
lr_train
# class 1 test error
lr_train_class1 <- tab.lr_train[2,1]/170
lr_train_class1
# class 0 test error
lr_train_class0 <- tab.lr_train[1,2]/5099
lr_train_class0



# prediction using our test set for error
yhat.test.lr <- predict(glm_base, dat.test, 
                        type = "response") 
lr_yhat.test.class <- ifelse(yhat.test.lr > 0.5, 1, 0)

tab.lr_test <- table(dat.test$bankrupt, 
                     lr_yhat.test.class, 
                     dnn = c("Actual","Predicted"))
tab.lr_test

# overall test error 
lr_test <- mean(lr_yhat.test.class != dat.test$bankrupt)
lr_test
# class 1 test error
lr_test_class1 <- tab.lr_test[2,1]/50
lr_test_class1
# class 0 test error
lr_test_class0 <- tab.lr_test[1,2]/1500
lr_test_class0





# part 2c
# model on our train_boot data
glm_boot <- glm(bankrupt ~ ., data=train_boot, family='binomial')
summary(glm_boot)

# prediction using our train set for error
yhat.train.lr_boot <- predict(glm_boot, train_boot, 
                         type = "response") 


# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
lr_yhat.train.boot.class <- ifelse(yhat.train.lr_boot > 0.5, 1, 0)

# confusion matrix of our train set
tab.lr_train_boot <- table(train_boot$bankrupt, 
                      lr_yhat.train.boot.class, 
                      dnn = c("Actual","Predicted"))
tab.lr_train_boot

# overall train error 
lr_train_boot <- mean(lr_yhat.train.boot.class != train_boot$bankrupt)
lr_train_boot
# class 1 test error
lr_train_boot_class1 <- tab.lr_train_boot[2,1]/500
lr_train_boot_class1
# class 0 test error
lr_train_boot_class0 <- tab.lr_train_boot[1,2]/3000
lr_train_boot_class0


# prediction using our test set for error
yhat.test.lr_boot <- predict(glm_boot, dat.test, 
                             type = "response") 
lr_yhat.test.boot.class <- ifelse(yhat.test.lr_boot > 0.5, 1, 0)

tab.lr_test.boot <- table(dat.test$bankrupt, 
                          lr_yhat.test.boot.class, 
                          dnn = c("Actual","Predicted"))
tab.lr_test.boot

# overall test error 
lr_test_boot <- mean(lr_yhat.test.boot.class != dat.test$bankrupt)
lr_test_boot
# class 1 test error
lr_test_boot_class1 <- tab.lr_test.boot[2,1]/50
lr_test_boot_class1
# class 0 test error
lr_test_boot_class0 <- tab.lr_test.boot[1,2]/1500
lr_test_boot_class0




# part 2d
# on our train_adas data
glm_adas <- glm(bankrupt ~ ., data=train_adas, family='binomial')
summary(glm_adas)

# prediction using our train set for error
yhat.train.lr_adas <- predict(glm_adas, train_adas, 
                              type = "response") 


# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
lr_yhat.train.adas.class <- ifelse(yhat.train.lr_adas > 0.5, 1, 0)

# confusion matrix of our train set
tab.lr_train_adas <- table(train_adas$bankrupt, 
                           lr_yhat.train.adas.class, 
                           dnn = c("Actual","Predicted"))
tab.lr_train_adas

# overall train error 
lr_train_adas <- mean(lr_yhat.train.adas.class != train_adas$bankrupt)
lr_train_adas
# class 1 test error
lr_train_adas_class1 <- tab.lr_train_adas[2,1]/5152
lr_train_adas_class1
# class 0 test error
lr_train_adas_class0 <- tab.lr_train_adas[1,2]/5099
lr_train_adas_class0


# prediction using our test set for error
yhat.test.lr_adas <- predict(glm_adas, dat.test, 
                             type = "response") 
lr_yhat.test.adas.class <- ifelse(yhat.test.lr_adas > 0.5, 1, 0)

tab.lr_test.adas <- table(dat.test$bankrupt, 
                          lr_yhat.test.adas.class, 
                          dnn = c("Actual","Predicted"))
tab.lr_test.adas

# overall test error 
lr_test_adas <- mean(lr_yhat.test.adas.class != dat.test$bankrupt)
lr_test_adas
# class 1 test error
lr_test_adas_class1 <- tab.lr_test.adas[2,1]/50
lr_test_adas_class1
# class 0 test error
lr_test_adas_class0 <- tab.lr_test.adas[1,2]/1500
lr_test_adas_class0

# After performing the predictions on the test dataset using the models built using the 3 train sets,
# the logistic regression performs the best[considering only performance]




# Part 3
# DECISION TREES

# part 3a
# on our original train data
# converting the Y-variable to a "factor" since classification tree models assume
# that the Y variable is qualitative
new.train_og <- train_og
new.train_og[,1] <- as.factor(new.train_og[,1])

# preparing our test data
new.dat.test <- dat.test
new.dat.test[,1] <- as.factor(new.dat.test[,1])

# building our first tree
tree_og <- tree(bankrupt ~., data = new.train_og)
summary(tree_og)

# plotting our tree
plot(tree_og)
text(tree_og, pretty = 0)

# metadata
tree_og

# making predictions on our train data using tree1
tree.pred.train_og <- predict(tree_og, new.train_og, type = "class")

# confusion matrix
tab_tree_og <- table(new.train_og$bankrupt, tree.pred.train_og,
                   dnn = c("Actual", "Predicted"))
tab_tree_og

# overall error
tree_og_err <- mean(new.train_og$bankrupt != tree.pred.train_og)
tree_og_err
# class 1 train error
tree_og_class1 <- tab_tree_og[2,1]/170
tree_og_class1
# class 0 train error
tree_og_class0 <- tab_tree_og[1,2]/5099
tree_og_class0 


# making predictions using our test data
tree.pred.test <- predict(tree_og, new.dat.test, type = "class")

# confusion matrix
tab_tree_og.test <- table(new.dat.test$bankrupt, tree.pred.test,
                          dnn = c("Actual", "Predicted"))
tab_tree_og.test

# overall error
tree_og_err.test <- mean(new.dat.test$bankrupt != tree.pred.test)
tree_og_err.test
# class 1 test error
tree_og_class1.test <- tab_tree_og.test[2,1]/50
tree_og_class1.test
# class 0 test error
tree_og_class0.test <- tab_tree_og.test[1,2]/1500
tree_og_class0.test



# pruning our decision tree
prune_og <- prune.misclass(tree_og)
names(prune_og)

#  Plotting the results of the prune to identify the right tree size
plot(prune_og)
plot(prune_og$size, prune_og$dev, xlab = "Size of Tree",
     ylab = "Deviation/Deviance")

# pruning our tree1
prune.tree_og <- prune.misclass(tree_og, best = 4)
summary(prune.tree_og)

# plotting our pruned tree
plot(prune.tree_og)
text(prune.tree_og, pretty = 0)

# making predictions on our test data using prune.tree1
pt1.pred <- predict(prune.tree_og, new.train_og, type = "class")

# confusion matrix
tab_pt1 <-table(new.train_og$bankrupt, pt1.pred,
                dnn = c("Actual", "Predicted"))

tab_pt1

# overall error
pt1_err <- mean(new.train_og$bankrupt != pt1.pred)
pt1_err
# class 1 test error
pt1_class1 <- tab_pt1[2,1]/170
pt1_class1
# class 0 test error
pt1_class0 <- tab_pt1[1,2]/5099
pt1_class0


# prediction using our test set for error
pt1.test.pred <- predict(prune.tree_og, new.dat.test, type = "class")

# confusion matrix
tab_pt1.test <-table(new.dat.test$bankrupt, pt1.test.pred,
                     dnn = c("Actual", "Predicted"))

tab_pt1.test

# overall error
pt1_err.test <- mean(new.dat.test$bankrupt != pt1.test.pred)
pt1_err.test
# class 1 test error
pt1_class1.test <- tab_pt1.test[2,1]/50
pt1_class1.test
# class 0 test error
pt1_class0.test <- tab_pt1.test[1,2]/1500
pt1_class0.test





# part 3b
# model on our train_boot data
# converting the Y-variable to a "factor" since classification tree models assume
# that the Y variable is qualitative
new.train_boot <- train_boot
new.train_boot[,1] <- as.factor(new.train_boot[,1])

# building our first tree
tree_boot <- tree(bankrupt ~., data = new.train_boot)
summary(tree_boot)

# plotting our tree
plot(tree_boot)
text(tree_boot, pretty = 0)

# metadata
tree_boot

# making predictions on our test data using tree1
tree.pred.train_boot <- predict(tree_boot, new.train_boot, type = "class")

# confusion matrix
tab_tree_boot <- table(new.train_boot$bankrupt, tree.pred.train_boot,
                     dnn = c("Actual", "Predicted"))
tab_tree_boot

# overall error
tree_boot_err <- mean(new.train_boot$bankrupt != tree.pred.train_boot)
tree_boot_err
# class 1 test error
tree_boot_class1 <- tab_tree_boot[2,1]/500
tree_boot_class1
# class 0 test error
tree_boot_class0 <- tab_tree_boot[1,2]/3000
tree_boot_class0 

# predictions on our test data
tree.pred.test_boot <- predict(tree_boot, new.dat.test, type = "class")

# confusion matrix
tab_tree_boot.test <- table(new.dat.test$bankrupt, tree.pred.test_boot,
                            dnn = c("Actual", "Predicted"))
tab_tree_boot.test

# overall error
tree_boot_err.test <- mean(new.dat.test$bankrupt != tree.pred.test_boot)
tree_boot_err.test
# class 1 test error
tree_boot_class1.test <- tab_tree_boot.test[2,1]/50
tree_boot_class1.test
# class 0 test error
tree_boot_class0.test <- tab_tree_boot.test[1,2]/1500
tree_boot_class0.test 


# pruning our decision tree
prune_boot <- prune.misclass(tree_boot)
names(prune_boot)


#  Plotting the results of the prune to identify the right tree size
plot(prune_boot)
plot(prune_boot$size, prune_boot$dev, xlab = "Size of Tree",
     ylab = "Deviation/Deviance")

# pruning our tree1
prune.tree_boot <- prune.misclass(tree_boot, best = 11)
summary(prune.tree_boot)

# plotting our pruned tree
plot(prune.tree_boot)
text(prune.tree_boot, pretty = 0)

# making predictions on our test data using prune.tree1
pt2.pred <- predict(prune.tree_boot, new.train_boot, type = "class")

# confusion matrix
tab_pt2 <-table(new.train_boot$bankrupt, pt2.pred,
                dnn = c("Actual", "Predicted"))

tab_pt2

# overall error
pt2_err <- mean(new.train_boot$bankrupt != pt2.pred)
pt2_err
# class 1 test error
pt2_class1 <- tab_pt2[2,1]/500
pt2_class1
# class 0 test error
pt2_class0 <- tab_pt2[1,2]/3000
pt2_class0

# predictions using our test data
pt2.test.pred <- predict(prune.tree_boot, new.dat.test, type = "class")

# confusion matrix
tab_pt2.test <-table(new.dat.test$bankrupt, pt2.test.pred,
                     dnn = c("Actual", "Predicted"))

tab_pt2.test

# overall error
pt2_err.test <- mean(new.dat.test$bankrupt != pt2.test.pred)
pt2_err.test
# class 1 test error
pt2_class1.test <- tab_pt2.test[2,1]/50
pt2_class1.test
# class 0 test error
pt2_class0.test <- tab_pt2.test[1,2]/1550
pt2_class0.test



# part 3c
# model on our train_adas data
# converting the Y-variable to a "factor" since classification tree models assume
# that the Y variable is qualitative
new.train_adas <- train_adas
new.train_adas[,1] <- as.factor(new.train_adas[,1])

# building our first tree
tree_adas <- tree(bankrupt ~., data = new.train_adas)
summary(tree_adas)

# plotting our tree
plot(tree_adas)
text(tree_adas, pretty = 0)

# metadata
tree_adas

# making predictions on our test data using tree1
tree.pred.train_adas <- predict(tree_adas, new.train_adas, type = "class")

# confusion matrix
tab_tree_adas <- table(new.train_adas$bankrupt, tree.pred.train_adas,
                       dnn = c("Actual", "Predicted"))
tab_tree_adas

# overall error
tree_adas_err <- mean(new.train_adas$bankrupt != tree.pred.train_adas)
tree_adas_err
# class 1 test error
tree_adas_class1 <- tab_tree_adas[2,1]/5152
tree_adas_class1
# class 0 test error
tree_adas_class0 <- tab_tree_adas[1,2]/5099
tree_adas_class0 

# predictions using our test data
tree.pred.test_adas <- predict(tree_adas, new.dat.test, type = "class")

# confusion matrix
tab_tree_adas.test <- table(new.dat.test$bankrupt, tree.pred.test_adas,
                            dnn = c("Actual", "Predicted"))
tab_tree_adas.test

# overall error
tree_adas_err.test <- mean(new.dat.test$bankrupt != tree.pred.test_adas)
tree_adas_err.test
# class 1 test error
tree_adas_class1.test <- tab_tree_adas.test[2,1]/50
tree_adas_class1.test
# class 0 test error
tree_adas_class0.test <- tab_tree_adas.test[1,2]/1500
tree_adas_class0.test



# pruning our decision tree
prune_adas <- prune.misclass(tree_adas)
names(prune_boot)


#  Plotting the results of the prune to identify the right tree size
plot(prune_adas)
plot(prune_adas$size, prune_adas$dev, xlab = "Size of Tree",
     ylab = "Deviation/Deviance")

# pruning our tree1
prune.tree_adas <- prune.misclass(tree_adas, best = 5)
summary(prune.tree_adas)

# plotting our pruned tree
plot(prune.tree_adas)
text(prune.tree_adas, pretty = 0)

# making predictions on our test data using prune.tree1
pt3.pred <- predict(prune.tree_adas, new.train_adas, type = "class")

# confusion matrix
tab_pt3 <-table(new.train_adas$bankrupt, pt3.pred,
                dnn = c("Actual", "Predicted"))

tab_pt3

# overall error
pt3_err <- mean(new.train_adas$bankrupt != pt3.pred)
pt3_err
# class 1 test error
pt3_class1 <- tab_pt3[2,1]/5152
pt3_class1
# class 0 test error
pt3_class0 <- tab_pt3[1,2]/5099
pt3_class0

# prediction using our test set for error
pt3.pred.test <- predict(prune.tree_adas, new.dat.test, type = "class")

# confusion matrix
tab_pt3.test <-table(new.dat.test$bankrupt, pt3.pred.test,
                     dnn = c("Actual", "Predicted"))

tab_pt3.test

# overall error
pt3_err.test <- mean(new.dat.test$bankrupt != pt3.pred.test)
pt3_err.test
# class 1 test error
pt3_class1.test <- tab_pt3.test[2,1]/50
pt3_class1.test
# class 0 test error
pt3_class0.test <- tab_pt3.test[1,2]/1500
pt3_class0.test




# Part 4
# table to compare our train values
train.error <- data.frame(matrix(0,3,10))
names(train.error) <- c("Error" , "Logistic Reg Train","Logistic Reg Boot", "Logistic Reg ADAS",
                        "Decision Tree", "Pruned Tree", "Decision Tree Boot","Pruned Tree Boot",
                        "Decision Tree Adas", "Pruned Tree ADAS")

train.error[1,] <- c('Overall Error', lr_train, lr_train_boot, lr_train_adas, tree_og_err, pt1_err,
                     tree_boot_err, pt2_err, tree_adas_err, pt3_err)
train.error[2,] <- c('Class 1 Err', lr_train_class1,lr_train_boot_class1, lr_train_adas_class1, 
                     tree_og_class1, pt1_class1, tree_boot_class1, pt2_class1, tree_adas_class1,
                     pt3_class1)
train.error[3,] <- c('Class 0 Err', lr_train_class0, lr_train_boot_class0, lr_train_adas_class0, 
                     tree_og_class0, pt1_class0, tree_boot_class0, pt2_class0, tree_adas_class0,
                     pt3_class0)

# lets look at our table
train.error



# table to compare our test values
test.error <- data.frame(matrix(0,3,10))
names(test.error) <- c("Error" , "Logistic Reg","Logistic Reg Boot", "Logistic Reg ADAS",
                       "Decision Tree", "Pruned Tree", "Decision Tree Boot","Pruned Tree Boot",
                       "Decision Tree Adas", "Pruned Tree ADAS")

test.error[1,] <- c('Overall Error', lr_test, lr_test_boot, lr_test_adas, tree_og_err.test, pt1_err.test,
                    tree_boot_err.test, pt2_err.test, tree_adas_err.test, pt3_err.test)
test.error[2,] <- c('Class 1 Err', lr_test_class1, lr_test_boot_class1, lr_test_adas_class1, 
                    tree_og_class1.test, pt1_class1.test, tree_boot_class1.test, pt2_class1.test
                    , tree_adas_class1.test,
                    pt3_class1.test)
test.error[3,] <- c('Class 0 Err', lr_test_class0, lr_test_boot_class0, lr_test_adas_class0, 
                    tree_og_class0.test, pt1_class0.test, tree_boot_class0.test, pt2_class0.test
                    , tree_adas_class0.test,
                    pt3_class0.test)
test.error



# Part 5
# Prediction of Bankrupty for MediaTek INC. using our best model

max(dat.test$X40)
# setting up our data
mediatek <- data.frame(X15 = 0.1262, X18 = 2.73, X19 = 0.7626, X37 = 0.2718,
                       X40 = 0.4427, X52 = 8.211, X54 = 0.2565, X60 = 0.2327,
                       X65 = 0.3523, X65 = 0.3523, X66 = 0.3195, X68 = 0.4985,
                       X70 = 0.7078, X82 = 0.2369, X85 = 0, X86 = 0.1942, X89 = 0.4666,
                       X90 = 0.2666, X91 = 0.3732)

# lets check
mediatek

# prediction using our train set for error
yhat.mediatek <- predict(glm_base, mediatek, 
                         type = "response") 

# classifying the deposit variable as 0/1 or no/yes with a threshold of 0.5
lr_yhat.mediatek.class <- ifelse(yhat.mediatek > 0.5, 1, 0)
lr_yhat.mediatek.class


# we try to predict the bankruptcy of Mediatek Inc. since their share prices 
# have been dropping for the past year and eps is very low, However, the model did 
# not do very well since it predicted that mediatek will go bankrupt, which is not 
# likely since the company is doing financially well as of today. It must be mentioned 
# that the model is not 100% accurate.












