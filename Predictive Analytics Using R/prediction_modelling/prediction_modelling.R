# URVI VAIDYA MSBA Section B

#  Answers to Homework Assignment 1
#  BA 288 Winter 2022
#  Murphy 1/7/22 Draft
#
#
#  Read in Universal Bank Data 
#
# list.files(path = ".")
dat <- read.csv("~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 1/hw1_universal_bank.csv")
str(dat)



#  Part a.  Graphical descriptive statistics

# PIE CHART
tab1 <- table(dat$Sec_Account)
names(tab1) <- c("Yes", "No")
pie(tab1, names(tab1), main = "Proportion of Customers who have a Securities Account", 
    col = c("darkblue", "red"))

# BAR PLOT
tab2 <- table(dat$Education)
names(tab2) <- c("Less than college", "College Degree", "Graduate/Advanced Degree")
barplot(tab2, ylab = "Frequency", col = c("violet","magenta", "purple"),
        main = "Education Level of Customer")

# HSTOGRAM
hist(dat$Crdt_Crd_Avg)
hist(dat$Crdt_Crd_Avg, main = "Average Spending on Credit Card issued by Bank", ylab = "Dollars",
     xlab = "Credit Card Average", col = 'darkblue')

# BOXPLOT
boxplot(Age~Online_Bank, data = dat, 
        horizontal = T, col = c("Red","darkBlue"))




#  Part b.  Conditional boxplot of Credit Card Debt on Acept Offer

boxplot(Crdt_Crd_Avg~Acpt_Offer, data = dat, 
        horizontal = T, col = c("red","darkgreen"), ylab = "Accepts Offer",
        xlab = "Credit Card Average", names= c('NO', 'YES'))

#  second “conditioned” chart of your choice
boxplot(Income~CD_Account, data = dat, 
        horizontal = T, col = c("red","darkgreen"), ylab = "Cash Deposit Account",
        xlab = "Income", names= c('NO', 'YES'))




#  Part c.  Scatter plots of Credit Card Debt on Income and 
#    Accept Offer on Credit Card Debt

plot(dat$Income, dat$Crdt_Crd_Avg,
     xlab = "Income", ylab="Average credit Card Debt",
     main= "Scatter plot of Income vs Avg. Credit card Debt")
abline(lm(dat$Crdt_Crd_Avg~dat$Income), lwd = 3, col='red')

# From the above plot we can see that there is no strong linear relationship between income
# and average credit card debt. 


plot(dat$Income, dat$Acpt_Offer,
     xlab = "Income", ylab="Average credit Card Debt",
     main= "Scatter plot of Income vs Avg. Credit card Debt")
abline(lm(dat$Acpt_Offer~dat$Income), lwd = 3, col='red')

# this chart does not really give us any information about the data. Additionally a scatter
# plot is not the best choice for categorical data. Given the present plot, we also cannot 
# determine with confidence if there is a direct relationship between income and accept offer without
# conducting further analysis. 


# Part d.

# Insight 1
# From the box plot in part a we can see that age of the banks customer's does not seem to impact
# their use of online banking. There sometimes is an assumption that older people are not as
# well versed with technology or that they prefer traditional banking methods however, that is no a 
# pattern we observe in our data. The usage of online banking is not impacted by age of the customer. 

# Insight 2
# It would seem logical that people with higher credit card debt are likelier to accept a personal
# loan, however we do not observe that from our data. Looking at the first boxplot in part b we can see that 
# people who have the lowest credit card debt have not accepted the personal. We can also see that there are many 
# outliers where people with high credit card debt have not accepted a personal loan offer. People with the credit 
# card debt in the middle two quartiles have accepted a personal loan offer. 

# Insight 3
# From the second box plot in part b we can see that income does not impact the customers decision to 
# open a cash deposit. Most of the customers who have a cash deposit are in the $55000 to $160000 income bracket
# whereas customers in the 40000 - 90000 income bracket do not have a cash deposit account. We also see a lot of 
# outliers in the non cash deposit category, many customers with income above 170000 do not have cash deposits. 
# This could perhaps be because customers in the lower income bracket perhaps do not have enough savings to open a 
# cash deposit account and people with very high income may be more interested in opening securities account since 
# they may be interested in investing. 



# Part e.
options(scipen = 9999)
apply(dat, 2, mean)
apply(dat, 2, median)
apply(dat, 2, sd)
apply(dat, 2, var)

# we can see from the above statistics that the sd for both work experience, income and mortgage is quite high. Even though
# the average income is about 74k the sd is about 46k and its 100k for mortgage. We can also observe that the mean and median 
# values for most columns is similar therefore the data appears to be normally distributed. 


# Part f. 
cor(dat)
# Income, Credit card average, and cash deposit account have the highest correlation to accept offer which is our dependent 
# variable. This is not surprising as these would be the factors which would impact a customer's decision to accept
# an offer or not. A customers income and their average credit and debt would directly impact  both their need for the loan
# and their ability to repay the loan. 

# Part g. 
reg1 <- lm(Crdt_Crd_Avg~Income, data=dat)
summary(reg1)

# the regression model equation is as follows: Y-hat(predicted Crdt_Crd_Avg) = 0.1103 + 0.0247 * Income
# The model is able to explain 41.32% of the variance in Average Credit Card debt, there is a lot of variance
# that is not explained by our model, therefore this is not a very accurate model for prediction.
# For every dollar increase in income the Average Credit card debt goes up by $0.0247.
# We can see that the p-value of income is very low therefore, we can say that the income variable is significant 
# at 0.001 level of significance.
# A variable is considered to be significant when changes in the variable correlate with changes in the dependent variable.

# Part h. 
reg2 <- lm(Acpt_Offer~Income, data=dat)
summary(reg2)

# The model is able to explain 26.75% of the variance in Average Credit Card debt, which is even lower than our
# earlier model. There is almost 74% variance that is not explained by this model, therefore this is not a 
# very accurate model for prediction and the fit is not very good.
# The p-value of income is very low therefore, we can say that the income variable is significant 
# at 0.001 level of significance.
# However, since accept offer is a binary variable the linear regression model is not the right model for prediction since 
# it gives us a continuous outcome while for accept offer we require a discrete outcome. 



# Part i.
newdat <- data.frame(Income = 75000)
names(newdat)
predict(reg1, newdat, interval = "prediction", level = 0.99)
# output:
# fit      lwr      upr
# 1854.697 1740.762 1968.632

# According to our first model, for a customer with income 75000, the average credit card debt is predicted
# to be 1854.697 with a lower and upper bound of 1740.762 and 1968.632 respectively at a 0.001 level of significance.
# Although income is a significant predictor of average credit card debt, this model estimate cannot be a useful
# predictor for decision makers as it only explains 41.32% of the variance in Average Credit Card debt, which is not
# enough information to make an informed decision.

predict(reg2, newdat, interval = "prediction", level = 0.99)
# output:
# fit      lwr      upr
# 251.8936 230.3954 273.3919
# According to our first model, for a customer with income 75000, the accept offer is predicted
# to be 251.8936 with a lower and upper bound of 230.3954 and 273.3919 respectively at a 0.001 level of significance.
# This is not a good model at all since accept offer can accept only two values, 0 and 1. The predicted outcomes of a linear 
# regression model are continuous and accept offer requires a discrete output or classification model. 

# Part j.
# As explained above Accept Offer can take only two values, 0 and 1. For such a prediction we require a classification model that 
# gives us a discrete outcome and not a regression model which gives a continuous outcome. 
# For predicting Accept Offer we can use Logistic Regression, Decision Trees, SVM and other such models that give
# us a discrete output and are more suited to classification problems. 






