# HW5 - Urvi Vaidya

library(fpp3)
setwd("~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 5")



# Part 1.
# Reading the bike data
bike <- read.csv('hw5_bike_share_day.csv')
str(bike)

# setting up the bike data as a tsibble
bike$dteday <- as.Date(bike$dteday, format = "%m/%d/%Y")
cnts <- ts(bike[,14], frequency = 7)
cntts <- as_tsibble(cnts)
cntts <- mutate(cntts, index = bike[,2])
names(cntts)

# plotting the data
autoplot(cntts)

# There are signals in the data as can be observed from the frequency and amplitude
# of the plot.
# we can see that the count of bikes appears to drop around october with
# the lowest months being january/december and begins to rise around april
# with a peak in june/july. Therefore, there appears to be a consistent seasonal
# pattern, which makes sense as the number of bikes in the summer months would
# be higher since the weather is better as compared to the cold temperatures of winter.


# Fitting a exponential smoothing (ses) model to the bike data with a = 0.25
fit_bike_SES_a.25 <- model(cntts, ETS(value ~ error("A") + 
                                   trend("N", alpha = 0.25) + 
                                   season("N")))
report(fit_bike_SES_a.25)
acc_ses_a.25 <- accuracy(fit_bike_SES_a.25)


# RMSE
RMSE_ses_a.25 <- acc_ses_a.25$RMSE
RMSE_ses_a.25

# plotting our results
aug_SES_a.25 <- augment(fit_bike_SES_a.25)
autoplot(aug_SES_a.25, value) +
  autolayer(aug_SES_a.25,.fitted, colour = "Red") +
  autolayer(aug_SES_a.25,.resid, colour = "Green") +
  labs(y = "Count of Bikes", title = "Bike Sharing SES Model with a = 0.25",
       x = "Year/Week") 


# Fitting a exponential smoothing (ses) model to the bike data with a = 0.75
fit_bike_SES_a.75 <- model(cntts, ETS(value ~ error("A") + 
                                        trend("N", alpha = 0.75) + 
                                        season("N")))
report(fit_bike_SES_a.75)
acc_ses_a.75 <- accuracy(fit_bike_SES_a.75)

# RMSE
RMSE_ses_a.75 <- acc_ses_a.75$RMSE
RMSE_ses_a.75

# plotting our results
aug_SES_a.75 <- augment(fit_bike_SES_a.75)
autoplot(aug_SES_a.75, value) +
  autolayer(aug_SES_a.75,.fitted, colour = "Red") +
  autolayer(aug_SES_a.75,.resid, colour = "Green") +
  labs(y = "Count of Bikes", title = "Bike Sharing SES Model with a = 0.75",
       x = "Year/Week") 


# Creating our naive model using only the mean ridership as the estimate 
fit_bike_naive <- model(cntts, Mean = MEAN(value))

report(fit_bike_naive)
acc_naive <- accuracy(fit_bike_naive)

# RMSE
RMSE_naive <- acc_naive$RMSE
RMSE_naive

# plotting our results
aug_SES_naive <- augment(fit_bike_naive)
autoplot(aug_SES_naive, value) +
  autolayer(aug_SES_naive,.fitted, colour = "Red") +
  autolayer(aug_SES_naive,.resid, colour = "Green") +
  labs(y = "Count of Bikes", title = "Bike Sharing Naive Model",
       x = "Year/Week") 


# Lets get our base RMSE of the data as well, the sd of the data
# without any models in this case.
RMSE_base <- sd(cntts$value)
RMSE_base

# Which is a better fit?  Why?
# Our Naive model  has an RMSE of 1935.886 which is the highest of our three models
# from this part. Obviously our SES models outperformed the Naive model as the
# naive model simply takes an average of the entire data and thus fits a straight
# line. Our SES a=0.25 model gives us a RMSE of 964.95 and SES a=0.75 model gives
# an RMSE of 1005.825. The AIC of SES-0.25 model is 14871.5 and the AIC of
# SES-0.75 is 14932.15.
# We also know that a higher alpha gives more weight to recent values, thus our 
# RMSE for SES a=0.75 is higher. 
# Thus, I will choose SES a=0.25 as my preferred model in this part since it gives
# us the lowest RMSE.



# Part 2.
#  Fit Holt's model - Holts model uses additive trend instead of no trend
fit_bike_Holt <- model(cntts, 
                          ETS(value ~ error("A") 
                              + trend("A") 
                              + season("N"))
)

report(fit_bike_Holt)
acc_Holt <- accuracy(fit_bike_Holt)

# RMSE
RMSE_Holt <- acc_Holt$RMSE
RMSE_Holt

aug_Holt <- augment(fit_bike_Holt)
autoplot(aug_Holt, value) +
  autolayer(aug_Holt,.fitted, colour = "Red") +
  autolayer(aug_Holt,.resid, colour = "Green") +
  labs(y = "Count of Bikes", title = "Bike Sharing with Holts Model",
       x = "Year/Week") 


#  Fit Holt's model - damped additive trend
fit_bike_HoltD <- model(cntts, 
                       ETS(value ~ error("A") 
                           + trend("Ad") 
                           + season("N"))
)

report(fit_bike_HoltD)
acc_HoltD <- accuracy(fit_bike_HoltD)

# RMSE
RMSE_HoltD <- acc_HoltD$RMSE
RMSE_HoltD

aug_HoltD <- augment(fit_bike_HoltD)
autoplot(aug_HoltD, value) +
  autolayer(aug_HoltD,.fitted, colour = "Red") +
  autolayer(aug_HoltD,.resid, colour = "Green") +
  labs(y = "Count of Bikes", title = "Bike Sharing with Holts Model",
       x = "Year/Week") 


# Our preferred simple(ses) model uses additive errors; no trend, a = 0.25;
# no seasonality. Holts model is similar to the ses model but uses additive 
# trend instead of no trend. Between the two models from this part we can see
# that the damped Holts model has a marginally lower RMSE of 964.58 compared to
# that of the simple Holts model with RMSE of 964.78. Both these models are have
# performed between than our preferred SES model from part 1. I would say the main
# reason for the better performance between our SES models and Holts' is that in
# the Holts model we also tweak the parameter trend which allows for a better fit
# of our data and a smoother forecast line since we are using additive trend for
# the slope. In this section my preferred model is Holts Damped Additive Model.




# Part 3. 
# Holts Winter Model 1 with additive trend and additive seasonality
fit_bike_HWa <- model(cntts, 
                         ETS(value ~ error("A") 
                             + trend("A") 
                             + season("A"))
)


report(fit_bike_HWa)
aug_HWa <- augment(fit_bike_HWa)
acc_HWa <- accuracy(fit_bike_HWa)

# RMSE
RMSE_HWa <- acc_HWa$RMSE
RMSE_HWa


# Holts Winter Model 2 with additive trend and multiplicative seasonality
fit_bike_HWm <- model(cntts, 
                      ETS(value ~ error("A") 
                          + trend("A") 
                          + season("M"))
)

report(fit_bike_HWm)
aug_HWm <- augment(fit_bike_HWm)
acc_HWm <- accuracy(fit_bike_HWm)

# RMSE
RMSE_HWm <- acc_HWm$RMSE
RMSE_HWm



# Holts Winter Model 3 with additive damped trend and additive seasonality
fit_bike_HWad <- model(cntts, 
                      ETS(value ~ error("A") 
                          + trend("Ad") 
                          + season("A"))
)

report(fit_bike_HWad)
aug_HWad <- augment(fit_bike_HWad)
acc_HWad <- accuracy(fit_bike_HWad)

# RMSE
RMSE_HWad <- acc_HWad$RMSE
RMSE_HWad


# Holts Winter Model 4 with additive damped trend and multiplicative seasonality
fit_bike_HWadm <- model(cntts, 
                      ETS(value ~ error("A") 
                          + trend("Ad") 
                          + season("M"))
)

report(fit_bike_HWadm)
aug_HWadm <- augment(fit_bike_HWadm)
acc_HWadm <- accuracy(fit_bike_HWadm)

# RMSE
RMSE_HWadm <- acc_HWadm$RMSE
RMSE_HWadm


# plotting our models 
#  Plot both Holt Additive & Damped additive  models together
autoplot(cntts, value, colour = "black") +
  autolayer(aug_HWa,.fitted, colour = "red") +
  autolayer(aug_HWa,.resid, colour = "red") + 
  autolayer(aug_HWad,.fitted, colour = "#d1b2ff") +
  autolayer(aug_HWad,.resid, colour = "#d1b2ff") + 
  labs(y = "Count of Bikes", title = "Bike Sharing with Holts Winter Models",
       x = "Year/Week")


#  Plot both Holt multiplicative & Damped additive multiplicative  models together
autoplot(cntts, value, colour = "black") +
  autolayer(aug_HWm,.fitted, colour = "green") +
  autolayer(aug_HWm,.resid, colour = "green") +
  autolayer(aug_HWadm,.fitted, colour = "blue") +
  autolayer(aug_HWadm,.resid, colour = "blue") + 
  labs(y = "Count of Bikes", title = "Bike Sharing with Holts Winter Models",
       x = "Year/Week")


# we can see from out first plot that both our models 1 and 2 fit almost identically. 
# Our fitted values overlap each other. This is not surprising as our RMSE 
# values for both these models is almost the same. 
# we see the second plot that has both our multiplicative models, Models 3 & 4. 
# We can observe a difference in the values here, our Model 4 appears to have a better
# fit. These views are supported by our RMSE values
# Of the four Holts Winter models I find Model 4, the Additive Damped Multiplicative Model,
# to be the best one as it has the lowest RMSE of 944.54.
# We know that the additive method is preferred when the seasonal variations are roughly 
# constant through the series, while the multiplicative method is preferred when 
# the seasonal variations are changing proportional to the level of the series. 
# Therefore, the results are not surprising, as the plot shows that the seasonal 
# variation in the data increases as the level of the series increases. 
# our data is set up weekly however since there appears to be a pattern based on
# months i would perhaps like to set up monthly data. However, this would reduce 
# our ability to provide forecasts for a smaller time period like days or weeks.



# Part 4.
# forecasts from SES a=0.25 Model in Part 1 for next 4 weeks
forc_bike_SES_a.25 <- forecast(fit_bike_SES_a.25, h = 28)
# Displaying the values of the forecasts to the console 
forc_bike_SES_a.25

# forecasts from Holts Additive Damped Model from Part 2 for next 4 weeks
forc_bike_HoltD <- forecast(fit_bike_HoltD, h = 28)
# Displaying the values of the forecasts to the console 
forc_bike_HoltD 

# forecasts from Additive Damped Multiplicative Model, Model 4,
# in Part 3 for next 4 weeks
forc_bike_HWadm <- forecast(fit_bike_HWadm, h = 28)
# Displaying the values of the forecasts to the console 
forc_bike_HWadm


#  Plot our 3 forecasts in the same plot
autoplot(cntts, value, colour = "black")+
  autolayer(forc_bike_SES_a.25, cntts, 
         level = NULL, colour = "Blue") +
  autolayer(forc_bike_HoltD, cntts, 
            level = NULL, colour = "Red") +
  autolayer(forc_bike_HWadm, cntts, 
            level = NULL, colour = "Green")
  labs(y = "Count of Bikes", title = "Bike Sharing Forecasts",
       x = "Year/Week")

  
# table showing all RMSE values found
RMSE_table_bike <- data.frame(matrix(0,1,10))
names(RMSE_table_bike) <- c("Base RMSE" , "RMSE SES Model a=0.25", "RMSE SES Model a=0.75",
                       "RMSE Naive", "RMSE Holts Model", "RMSE Holts Damped Model", 
                       "RMSE HoltsW - A:A", "RMSE HoltsW - A:M", "RMSE HoltsW - Ad:A",
                       "RMSE HoltsW - Ad:M")

RMSE_table_bike[1,] <- c(RMSE_base, RMSE_ses_a.25, RMSE_ses_a.75, RMSE_naive, RMSE_Holt, 
                    RMSE_HoltD, RMSE_HWa, RMSE_HWa, RMSE_HWad, RMSE_HWadm)


# lets look at all our RMSE values
RMSE_table_bike



 # Part 5.
# getting the JohnsonJohnson data and loading it into a tsibble
# data()
JJ <- JohnsonJohnson
str(JohnsonJohnson)
JJts <- as_tsibble(JJ, index = yearquarter())
names(JJts)[2] <- "QE"
str(JJts)


# The JJ dataset has the Quarterly Earnings per Johnson & Johnson Share, which is 
# stored in a time series format ranging from the year 1960 to 1981, the data has 21
# years of observations and is arranged quarterly. Thus, each year has 4 data points,
# one for each quarter (21*4), making a total of 84 observations. We can also see
# this from the size of the data which is 84. 

# fitting the JJ data with Holts Winter AAA Model 1
fit_JJts_HWa <- model(JJts, 
                      ETS(QE ~ error("A") 
                          + trend("A") 
                          + season("A"))
)

# reporting the co-efficients
report(fit_JJts_HWa)

acc_HWa <- accuracy(fit_JJts_HWa)

# RMSE
RMSE_AAA <- acc_HWa$RMSE
AIC_AAA <- 250.88


# plotting the model components
comp_AAA <- components(fit_JJts_HWa)
autoplot(comp_AAA)



# Part 6.
# Holts Winter Model 2 with additive trend and multiplocative seasonality
fit_JJts_HWm <- model(JJts, 
                      ETS(QE ~ error("A") 
                          + trend("A") 
                          + season("M"))
)


report(fit_JJts_HWm)
aug_JJts_HWm <- augment(fit_JJts_HWm)
acc_JJts_HWm <- accuracy(fit_JJts_HWm)

# RMSE
RMSE_JJts_HWm <- acc_JJts_HWm$RMSE
AIC_JJts_HWm <- 252.72


# Holts Winter Model 3 with additive damped trend and additive seasonality
fit_JJts_HWad <- model(JJts, 
                       ETS(QE ~ error("A") 
                           + trend("Ad") 
                           + season("A"))
)


report(fit_JJts_HWad)
aug_JJts_HWad <- augment(fit_JJts_HWad)
acc_JJts_HWad <- accuracy(fit_JJts_HWad)

# RMSE
RMSE_JJts_HWad <- acc_JJts_HWad$RMSE
AIC_JJts_HWad <- 256.14

# Holts Winter Model 4 with additive damped trend and multiplicative seasonality
fit_JJts_HWadm <- model(JJts, 
                        ETS(QE ~ error("A") 
                            + trend("Ad") 
                            + season("M"))
)


report(fit_JJts_HWadm)
aug_JJts_HWadm <- augment(fit_JJts_HWadm)
acc_JJts_HWadm <- accuracy(fit_JJts_HWadm)

# RMSE
RMSE_JJts_HWadm <- acc_JJts_HWadm$RMSE
AIC_JJts_HWadm <- 254.13

# table showing all RMSE $ AIC values found
table_JJts <- data.frame(matrix(0,2,4))
names(table_JJts) <- c( "HoltsW - A:A", "HoltsW - A:M", "HoltsW - Ad:A",
                            "HoltsW - Ad:M")

table_JJts[1,] <- c(RMSE_AAA, RMSE_JJts_HWm, RMSE_JJts_HWad, RMSE_JJts_HWadm)
table_JJts[2,] <- c(AIC_AAA, AIC_JJts_HWad, AIC_JJts_HWad, AIC_JJts_HWadm)

# lets look at all our RMSE values
table_JJts

# of all the models including the AAA model we find that the AAA model has the lowest
# RMSE of 0.4363. From part 2 we find the Holts Winter Additive Damped Multiplicative 
# model to be the best with an RMSE value of 0.4396. This is nearly the same as our AAA
# model, However we we will choose the Holts Winter Additive Damped Multiplicative 
# model as our best model for this data as the seasonality component is more even compared to
# the AAA model where the seasonality component is increasing. 

# Plotting the components of the Holts Winter Additive Damped Multiplicative
comp_ETS_best <- components(fit_JJts_HWadm)
autoplot(comp_ETS_best)


# forecasting using our best model for next 3 years
forc_JJts_HWadm <- forecast(fit_JJts_HWadm, h = 12)
# Displaying the values of the forecasts to the console 
forc_JJts_HWadm

# plotting our forecasts
autoplot(forc_JJts_HWadm, JJts, 
          level = NULL, colour = "Blue")




# Part 7.
# Automated ETS selection
# The ETS command in forecasting automatically selects the preferred model based on the 
# Akaike Information Criterion (AIC), which balances the goodness of fit of the model 
# and its complexity. The AIC balances the trade-off between model accuracy and model 
# complexity. The ETS command selects the preferred model with the lowest AIC value, 
# indicating the best balance between goodness of fit and model complexity for the given 
# data. This is done by fitting different ETS models to the data and comparing their 
# goodness of fit using AIC.

# Lets try to fit an automated ETS model on our JJts data
fit_JJts_autoETS <- model(JJts, ETS(QE))

report(fit_JJts_autoETS)

acc_autoETS <- accuracy(fit_JJts_autoETS)
acc_autoETS

RMSE_autoETS <- acc_autoETS$RMSE
RMSE_autoETS

# our automated model chose Model: ETS(M,A,A) with an RMSE of 0.4713 and AIC 163.63.
# Since the automated model uses AIC to choose the best model therefore even though the RMSE
# of this model is higher than our earlier models the AIC is lower.




# Part 8.
# setting up ur data
usemp <- us_employment
usemp <- filter(usemp, Title == "Total Private")
usemp <- usemp[,c(1,4)]
autoplot(usemp, Employed) 

# we will use automated model selection to compute the best model
fit_usemp_autoETS <- model(usemp, ETS(Employed))

report(fit_usemp_autoETS)

acc_usemp_autoETS <- accuracy(fit_usemp_autoETS)
acc_usemp_autoETS

# using auto ETS we get the best model as Model: ETS(A,Ad,A) with  Smoothing parameters:
# alpha = 0.8124939; beta = 0.04933209; gamma = 0.1875061 and phi = 0.9751562 

# plotting the model components
comp_ETS_usemp <- components(fit_usemp_autoETS)
autoplot(comp_ETS_usemp)


# forecasting using our best model for next 5 years
forc_usemp <- forecast(fit_usemp_autoETS, h = 60)
# Displaying the values of the forecasts to the console 
forc_usemp

# plotting our forecasts with 80% confidence bands
autoplot(forc_usemp, usemp, 
         level = 80 , colour = "Blue")


# Part 9.
# loading the google data
goog2015 <- filter(gafa_stock, Symbol == "GOOG", year(Date) == 2015)
goog2015 <- mutate(goog2015, day = row_number())
goog2015 <- update_tsibble(goog2015, index = day, regular = TRUE)

autoplot(goog2015,Close)


# fitting our best model using automatic model selection
fit_goog_auto <- model(goog2015, ETS(Close))

report(fit_goog_auto)
accuracy(fit_goog_auto)

# using auto ETS we get the best model as Model: ETS(A,Ad,A) with  Smoothing parameters:
# alpha = 0.8124939; beta = 0.04933209; gamma = 0.1875061 and phi = 0.9751562
# this model has an AIC of 2611.64 and an RMSE of 11.2

# plotting the model components
comp_goog_auto <- components(fit_goog_auto)
autoplot(comp_goog_auto)


# forecasting using our best model for next 5 years
forc_goog <- forecast(fit_goog_auto, h = 30)
# Displaying the values of the forecasts to the console 
forc_goog

# plotting our forecasts with 80% confidence bands
autoplot(forc_goog, goog2015, colour = "Blue")



# Part 10.
# fitting the best ARIMA Model on goog2015
fit_goog_A <- model(goog2015, ARIMA(Close))

report(fit_goog_A)
accuracy(fit_goog_A)

# using auto ARIMA we get the best model as Model: ARIMA(0,1,1) with 
# an RMSE of 11.1 and an AIC of 1925.45

# Even though the RMSE of both models is pretty similar the AIC values are very different.
# AIC accounts for the tradeoff between fit and complexity and thus in this case
# since we cannot differentiate in the RMSE values i will use the AIC value to
# pick a preferred model and choose ARIMA.
# I believe the ARIMA technique has worked better for this particular dataset since the
# AIC is much lower at 1925.45 than that of the ETS model at 2611.64. The RMSE of
# ETS is 11.2 and for ARIMA is 11.1, therefore, the ARIMA model has been able to maintain the
# RMSE while reducing the AIC. 






