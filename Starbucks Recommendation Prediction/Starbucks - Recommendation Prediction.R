# setting our working directory
setwd("~/OneDrive/MAC/UCI MSBA Coursework/BANA 200 - Foundations of Business Analytics/Take home Final")

# importing the libraries
options(scipen=999)
library(miscTools)
library(cluster)
library(NbClust)


# read in a the complete starbucks data
starbucks_og = as.data.frame(read.table("Starbucks_Data.txt", header=T, sep="\t"))

# cheking if we loaded the dataset correctly
starbucks_og

# since we are mostly using the X1 to X22 and recommend columns only 
# we will subset our data to contain just those variables
# we know that X1 to X22 are the first 22 columnns and recommend is the 24th column
starbucks <- starbucks_og[,c(1:22,24)]
head(starbucks) # checking of we subset correctly



# QUESTION 1 
# Q1 PART a
# we split the data subset to create training and testing samples
K <- 5000 # this is the datapoint at which we want to split the dataset
N <- nrow(starbucks) # total no of rows in the dataset

train_set <- as.data.frame(starbucks[1:K,]) # taking rows 1 to 5000
test_set <- as.data.frame(starbucks[(K+1):N,]) # taking rows 5001 to end

# further separating the test set as dependent variable (target variable) and all other columns
test_set.Y <- test_set[,23] # column 23 is now the recommend variable
test_set.X <- test_set[,1:22] # column 1:22 is X1:X22 the independent variables


# Q1 PART b
# run regression on training data
train_regression_one <- lm(recommend ~ ., data=train_set) 
summary(train_regression_one)     

# checking the variables that are significant at the 5% level (have a p-value less than 0.05)
significant_variables <- coef(summary(train_regression_one))[,4] < 0.05
significant_variables

# count of significant variables
table(significant_variables)


# Q1 Part c
# now we calculate the predictions on the test data so that we can see the out of sample r2
test_set_preds <- as.vector(predict(object=train_regression_one, newdata=test_set.X)) 

# compute test sample R-squared value so that we can compare the two
test_set_r2  <- rSquared(y=test_set.Y, resid= (test_set.Y - test_set_preds))

# we already have the r2 value for the training set so we wont calculate that again
# let us view the out of sample r2 value
round(test_set_r2,4) # r square is 0.2944



# QUESTION 2
# forward selection
# we first fit a model with no variables (only the intercept)
null_model <- lm(recommend ~ 1, data=train_set) # intercept only model

# we now fit the full model with all X variables
full_model <- lm(recommend ~ ., data = train_set)

# finally we perform a forward variable selection regression model 
forward_model_results <- step(object=null_model, direction="forward", scope=formula(full_model))
summary(forward_model_results) # 19 out of the 22 variables are retained



# QUESTION 3 
# Q3 Part a
X <- starbucks[,1:22]
X


# Q3 Part b
# diagnostic using multiple approaches
nb <- NbClust(X, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
# using the majority rule we get the optimum no of clusters as 2

# we look at our nb results
nb

# we create a bar plot to visualise our cluster analysis and 
# determine the best no of clusters
barplot(table(nb$Best.n[1,]),
        xlab="Numer of Clusters")


# Q3 Part c
# Let's run a K-means cluster analysis on 2 clusters
cluster_results = kmeans(x = X, centers = 2, iter.max=1000, nstart=100)

# we create a frequency table summarizing the number of people in each cluster
segment_count = table(cluster_results$cluster)
segment_count # cluster 1 has 2891 persons and Cluster 2 has 3230 persons


# Q3 Part d
# examining cluster centers which are used to interpret the results of the segments
t(round(cluster_results$centers,2)) # segment 2 is more satisfied


# Q3 Part e1
# we add a cluster_no column to our starbucks dataframe using the above clusters
starbucks <- cbind(starbucks,cluster_results$cluster)
starbucks

# we use the new starbucks dataframe to divide our data into 
# most satisfied and all others
most_satisfied <- starbucks[cluster_results$cluster == 2, 1:23]
all_others <- starbucks[cluster_results$cluster == 1, 1:23]

# checking if we have done our split correctly
nrow(most_satisfied) # 3230
nrow(all_others) # 2891


# Q3 Part e2
# we run a regression on our most satisfied segment
most_satisfied_regression <- lm(recommend ~ ., data=most_satisfied) 

# we run a regression on our all others segment
all_others_regression <- lm(recommend ~ ., data=all_others)


# Q3 Part e3
# lets look at the average mean of the fitted values for the above 2 regressions
round(mean(fitted.values(most_satisfied_regression)),2) # average for most_satisfied customers 
round(mean(fitted.values(all_others_regression)),2) # average for all_other customers



# QUESTION 4
# Q4 Part a
# increasing the values in columns X1, X2, X7, X8, and X10 of all_others by 1
all_others[,1] <- ifelse(all_others[,1] < 5, all_others[,1]+1, all_others[,1])
all_others[,2] <- ifelse(all_others[,2] < 5, all_others[,2]+1, all_others[,2])
all_others[,7] <- ifelse(all_others[,7] < 5, all_others[,7]+1, all_others[,7])
all_others[,8] <- ifelse(all_others[,8] < 5, all_others[,8]+1, all_others[,8])
all_others[,10] <- ifelse(all_others[,10] < 5, all_others[,10]+1, all_others[,10])

# checking if we updated correctly
head(all_others)


# Q4 Part b
# calculating our predictions based on updated all others 
new_all_others_preds <- as.vector(predict(object=all_others_regression, newdata=all_others[,1:22])) 


# Q4 Part c
# getting the average of the newly recalculated recommend based on the updated all_others
round(mean(new_all_others_preds),2) # we get the new average as 7.05

