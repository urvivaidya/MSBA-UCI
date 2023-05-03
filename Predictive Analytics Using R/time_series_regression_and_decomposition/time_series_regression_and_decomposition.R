# HW3 - Urvi Vaidya

library(fpp3)

setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 4/')

# Part 1.
# reading in our data
dat <- read.csv("hw4_home_starts.csv")

# looking at the columns & datatypes
str(dat)

# setting up the data
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat$Month <- as.factor(dat$Month)
dat$Quarter <- as.factor(dat$Quarter)
str(dat)
head(dat)

# adding the time trend columns
dat$timetr <- 1:756 
dat$timetrsq <- dat$timetr^2
str(dat)

# renaming the columns
names(dat)[5:6] <- c("timeTrend", "timeTrend2")

# plotting the data
plot(dat$Date, dat$Starts, xlab = "Month/Year", 
     ylab = "Starts in 1000s")
lines(dat$Date, dat$Starts, type = "l")


# Looking at the plot we can see that month and quarter are good 
# candidates for seasonal variables.
# Based on the data's monthly frequency and the long time period of the data 
# series (1958-2021), it is likely that the number of new privately-owned housing 
# units started will exhibit seasonal patterns. These patterns may repeat 
# annually and show peaks or valleys at certain times of the year. For example, 
# the number of housing starts may increase during the spring and summer months 
# when the weather is favorable for construction and people are more likely to 
# make a housing purchase, and decrease during the winter months when the weather 
# is less favorable for construction and people are less likely to make a housing 
# purchase.




# Part 2.

# regression using time trend
reg_time <- lm(Starts ~ timeTrend, data=dat)
sum_reg_time <- summary(reg_time)
sum_reg_time

# regression using time trend^2
reg_timesq <- lm(Starts ~ timeTrend + timeTrend2, data=dat)
sum_reg_timesq <- summary(reg_timesq)
sum_reg_timesq

# regression using time trend^3
reg_timecu <- lm(Starts ~ timeTrend + timeTrend2 + I(timeTrend^3), data=dat)
sum_reg_timecu <- summary(reg_timecu)
sum_reg_timecu

# regression using time trend^4
reg_timequa <- lm(Starts ~ timeTrend + timeTrend2 + I(timeTrend^3) +
                   I(timeTrend^4), data=dat)
sum_reg_timequa <- summary(reg_timequa)
sum_reg_timequa

# we get the following R^2 values from the above regressions
# timeTrend reg - 0.06702 and timeTrend is significant at 0.001 level of significance
# timeTrend^2 reg - 0.09831 but now timeTrend is significant at 0.01 level of significance
# and timeTrend^2 is significant at 0.001 level of significance
# timeTrend^3 reg - 0.1126 and all timeTrend, timeTrend^2 and timeTrend^3 are significant 
# at 0.001 level of significance
# timeTrend^4 reg - 0.1184 and interestingly only timeTrend^4 is significant at the 0.05
# level of significance
# As we add the higher order variables the complexity of the model increases and its fits
# the data better since the data is not linear.
# adding the higher order terms improves the R^2 of the model, however that improvement
# appears to taper off at the cubed time trend regression.

# using the quadratic time trend to predict starts
yhat_timesq <- predict(reg_timesq, dat)

# Residuals
pred_reg_timesq_resid <- dat$Starts - yhat_timesq

# RMSE of Model
RMSE.model.timesq <- sum_reg_timesq$sigma 
# MSE & RMSE of predictions
MSE.preds.timesq <- sum(pred_reg_timesq_resid^2)
MSE.preds.timesq
RMSE.preds.timesq <- sqrt(sum(pred_reg_timesq_resid^2)/753)
RMSE.preds.timesq


# plotting the actual housing starts and the fitted values on the same graph
pred_reg_timesq <- data.frame(dat$Date, dat$Starts, yhat_timesq)
names(pred_reg_timesq) <- c("Date", "Starts", "StartsHat")
str(pred_reg_timesq)

ggplot(pred_reg_timesq) +
  geom_line(aes(x = Date, y = Starts, color = "Starts")) +
  geom_line(aes(x = Date, y = StartsHat, color = "Forecast")) +
  ggtitle("Monthly Home Starts 1959 - 2021") +
  ylab("Starts in 1000s") +
  xlab("Year") +
  scale_color_manual(values = c("purple","red"))

# looking at the plot we can see that there appears to be a downward trend 
# in the housing starts over the years. We can see that there is a sharp dip
# around the 2008 recession which makes sense. 



# Part 3.
# using quarter as the seasonal variable. 
reg_seasonal_quart <- lm(Starts ~ Quarter + timeTrend + timeTrend2, data = dat) 
sum_reg_seasonal_quart <- summary(reg_seasonal_quart)
sum_reg_seasonal_quart

# using the quadratic time trend to predict starts
yhat_seasonal_quart <- predict(reg_seasonal_quart, dat)

# Residuals
pred_reg_seasonal_quart_resid <- dat$Starts - yhat_seasonal_quart

# RMSE of Model
RMSE.model.quart <- sum_reg_seasonal_quart$sigma 

# MSE & RMSE of predictions
MSE.preds.quart <- sum(pred_reg_seasonal_quart_resid^2)
MSE.preds.quart
RMSE.preds.quart <- sqrt(sum(pred_reg_seasonal_quart_resid^2)/753)
RMSE.preds.quart


# we can see that the R^2 of this model is 0.264, which is the highest we have
# seen so far. Even our RMSE is lower at 32.67 compared to the RMSE of our 
# earlier model at 35.83

# plotting the actual housing starts and the fitted values on the same graph
pred_reg_seasonal_quart <- data.frame(dat$Date, dat$Starts, yhat_seasonal_quart)
names(pred_reg_seasonal_quart) <- c("Date", "Starts", "StartsHat")
str(pred_reg_seasonal_quart)

ggplot(pred_reg_seasonal_quart) +
  geom_line(aes(x = Date, y = Starts, color = "Starts")) +
  geom_line(aes(x = Date, y = StartsHat, color = "Forecast")) +
  ggtitle("Monthly Home Starts 1959 - 2021") +
  ylab("Starts in 1000s") +
  xlab("Year") +
  scale_color_manual(values = c("purple","red"))



# Part 4.
# adding month as the seasonal variable. 
reg_seasonal_month <- lm(Starts ~ Month + Quarter + timeTrend + timeTrend2, data = dat) 
sum_reg_seasonal_month <- summary(reg_seasonal_month)
sum_reg_seasonal_month

# We get Quarters 2, 3 and 4 as NAs. This is because quarters are linearly 
# dependent on the months therefore we get NA values.

# regression after fixing errors
reg_seasonal_month_fixed <- lm(Starts ~ Month + timeTrend + timeTrend2, data = dat) 
sum_reg_seasonal_month_fixed <- summary(reg_seasonal_month_fixed)
sum_reg_seasonal_month_fixed

# using the quadratic time trend to predict starts
yhat_seasonal_month <- predict(reg_seasonal_month_fixed, dat)

# Residuals
pred_reg_seasonal_month_resid <- dat$Starts - yhat_seasonal_month

# RMSE of Model
RMSE.model.month <- sum_reg_seasonal_month_fixed$sigma 

# MSE & RMSE of predictions
MSE.preds.month <- sum(pred_reg_seasonal_month_resid^2)
MSE.preds.month
RMSE.preds.month <- sqrt(sum(pred_reg_seasonal_month_resid^2)/742)
RMSE.preds.month


# we can see that the R^2 of this model is 0.3404, which is the new highest we have 
# seen. Even our RMSE of month regression is lower at 31.1 compared to the RMSE of 
# our quarter model at 32.67 and timetrend square (quadratic) model at 35.83. 
# we can also see that so far the RMSEs of our predictions is almost similar to our 
# regression RMSEs.

# plotting the actual housing starts and the fitted values on the same graph
pred_reg_seasonal_month <- data.frame(dat$Date, dat$Starts, yhat_seasonal_month)
names(pred_reg_seasonal_month) <- c("Date", "Starts", "StartsHat")
str(pred_reg_seasonal_month)

ggplot(pred_reg_seasonal_month) +
  geom_line(aes(x = Date, y = Starts, color = "Starts")) +
  geom_line(aes(x = Date, y = StartsHat, color = "Forecast")) +
  ggtitle("Monthly Home Starts 1959 - 2021") +
  ylab("Starts in 1000s") +
  xlab("Year") +
  scale_color_manual(values = c("purple","red"))


# Part 5.
# creating a tsibble
dat1 <- mutate(dat, Month = yearmonth(Date))

HSdat <- as_tsibble(dat1[,c(2,4)], index = Month)
str(HSdat)

# autoplot of Hsdat
autoplot(HSdat,Starts) +
  ggtitle("Monthly Home Starts 1959 - 2021") +
  ylab("Starts in 1000s") +
  xlab("Years")

# Seasonal Plot
gg_season(HSdat) +
  ggtitle("Monthly Home Starts 1959 - 2021") +
  ylab("Starts in 1000s") +
  xlab("Year")

# autocorrelation plot
autoplot(ACF(HSdat, Starts, lag_max = 20))

# looking at the plots we see that the data has a high amplitude and frequency.
# There is a pretty significant autocorrelation in the housing starts data.
# This chart is not surprising for data that is trending and has seasonal components.
# The correlation decreases as the lag increases.



# Part 6.
# Random Walk forecast with drift
mod_drift <- model(HSdat, RW(Starts ~ drift()))
forc_drift <- forecast(mod_drift, h = "5 years")
aug_mod_drift <- augment(mod_drift)
mod_drift_res <- accuracy(mod_drift)

# RMSE
RMSE_drift <- mod_drift_res$RMSE
RMSE_drift


# Seasonal Naive forecast
mod_snaive <- model(HSdat, SNAIVE(Starts))
forc_snaive <- forecast(mod_snaive, h = 60)
aug_mod_snaive <- augment(mod_snaive)
mod_snaive_res <- accuracy(mod_snaive)

# RMSE
RMSE_snaive <- mod_snaive_res$RMSE
RMSE_snaive

# combined model
mod_drift_snaive <- model(HSdat, 
                     Drift = RW(Starts ~ drift()), 
                     SNaive = SNAIVE(Starts)
)

forc_drift_snaive <- forecast(mod_drift_snaive, h = 60)
aug_drift_snaive <- augment(mod_drift_snaive)


# plotting both models in the same graph
autoplot(forc_drift_snaive, HSdat, level=NULL) +
  autolayer(aug_drift_snaive, .resid)

# we can see from the lower half of the plot that the residuals of the drift 
# model are slightly more evenly distributed compared to the snaive model, and the
# residuals for the drift model are slightly lower.


# Part 7.
# calculating. moving averages for k = 5, 9, 11 & 17
HS.MA <-  mutate(HSdat, 
                  MA5 = slider::slide_dbl(Starts, mean, 
                                                 .before = 2, .after = 2,
                                                 .complete = TRUE),
                  MA9 = slider::slide_dbl(Starts,mean,.before = 4, 
                                          .after = 4,complete = TRUE),
                  MA11 = slider::slide_dbl(Starts,mean,.before = 5, 
                                           .after = 5,complete = TRUE),
                  MA17 = slider::slide_dbl(Starts,mean,.before = 8, 
                                           .after = 8,complete = TRUE))

autoplot(HS.MA, Starts) + 
  geom_line(aes(y = MA5), color = "Red") + 
  geom_line(aes(y = MA9), color = "Green") +
  geom_line(aes(y = MA11), color = "Orange") + 
  geom_line(aes(y = MA17), color = "Blue")

# calculating the residuals
MA_resid <- mutate(HS.MA,
                   resid5 = Starts - MA5,
                   resid9 = Starts - MA9,
                   resid11 = Starts - MA11,
                   resid17 = Starts - MA17)


# RMSEs of the Moving Averages
RMSE_MA5 <- sqrt(mean(MA_resid$resid5^2, na.rm=TRUE))
RMSE_MA5

RMSE_MA9 <- sqrt(mean(MA_resid$resid9^2, na.rm=TRUE))
RMSE_MA9

RMSE_MA11 <- sqrt(mean(MA_resid$resid11^2, na.rm=TRUE))
RMSE_MA11

RMSE_MA17 <- sqrt(mean(MA_resid$resid17^2, na.rm=TRUE))
RMSE_MA17

# We can see that the RMSE for Moving Average 5 is the lowest at 10.68. 
# this is because larger spans smooth the time series more than smaller spans 
# by averaging many ups and downs in each calculation. Smaller spans tend to 
# follow the ups and downs of the time series more closely.


# Part 8. 
# additive 
HSdat_cda <- model(HSdat, classical_decomposition(Starts, 
                                                  type = "additive"))

comp_cda <- components(HSdat_cda)
autoplot(comp_cda)

MSE_cda <- mean(comp_cda$random^2, na.rm = TRUE)
MSE_cda
RMSE_cda <- MSE_cda^0.5
RMSE_cda


# multiplicative
HSdat_cdm <- model(HSdat, classical_decomposition(Starts, 
                                                  type = "multiplicative"))

comp_cdm <- components(HSdat_cdm)
autoplot(comp_cdm)

MSE_cdm <- mean(comp_cdm$random^2, na.rm = TRUE)
RMSE_cdm <- MSE_cdm^0.5
RMSE_cdm

# We can see from the two plots that the trend and seasonal effect for both the
# additive and multiplicative model is very similar, however, the random plot
# in the data is different for the two models. The random plot for the multiplicative 
# model is more evenly distributed with lower values as compared to the additive model. 



# Part 9.
# STL Model 1
mod_STL1 <- model(HSdat, STL(Starts ~ trend(window = 13) +
                              season(window = "periodic"),
                            robust = TRUE))

comp_STL1 <- components(mod_STL1)
autoplot(comp_STL1)

# RMSE
err_STL1 <- comp_STL1$remainder
RMSE_STL1 <- (mean(err_STL1^2))^0.5
RMSE_STL1


# STL model 2
mod_STL2 <- model(HSdat, STL(Starts ~ trend(window = 13) +
                               season(window = 11),
                             robust = TRUE))

comp_STL2 <- components(mod_STL2)
autoplot(comp_STL2)

# RMSE
err_STL2 <- comp_STL2$remainder
RMSE_STL2 <- (mean(err_STL2^2))^0.5
RMSE_STL2


# STL Model 3
mod_STL3 <- model(HSdat, STL(Starts ~ trend(window = 5) +
                               season(window = "periodic"),
                             robust = TRUE))

comp_STL3 <- components(mod_STL3)
autoplot(comp_STL3)

# RMSE
err_STL3 <- comp_STL3$remainder
RMSE_STL3 <- (mean(err_STL3^2))^0.5
RMSE_STL3


# STL model 4
mod_STL4 <- model(HSdat, STL(Starts ~ trend(window = 5) +
                               season(window = 11),
                             robust = TRUE))

comp_STL4 <- components(mod_STL4)
autoplot(comp_STL4)

# RMSE
err_STL4 <- comp_STL4$remainder
RMSE_STL4 <- (mean(err_STL4^2))^0.5
RMSE_STL4

# RMSE STL4 is the preferred model with trend window = 5 and seasonal window = 11
# as it gives us the lowest RMSE of 7.0138 and the remainder plot has the lowest values.



# Part 10.
# table showing all RMSE values
RMSE.table <- data.frame(matrix(0,1,18))
names(RMSE.table) <- c("RMSE Quadratic Model", "RMSE Quadratic Model Preds", "RMSE Quarter Model",
                       "RMSE Quarter Model Preds", "RMSE Monthly Model", "RMSE Monthly Preds",
                       "RMSE Random Walk", "RMSE SNAIVE", "RMSE MA5", "RMSE MA9", "RMSE MA11",
                       "RMSE MA17", "RMSE CDA", "RMSE CDM", "RMSE_STL1", "RMSE_STL2",
                       "RMSE_STL3", "RMSE_STL4")

RMSE.table[1,] <- c(RMSE.model.timesq, RMSE.preds.timesq, RMSE.model.quart,
                    RMSE.preds.quart, RMSE.model.month, RMSE.preds.month,
                    RMSE_drift, RMSE_snaive, RMSE_MA5, RMSE_MA9, RMSE_MA11, RMSE_MA17,
                    RMSE_cda, RMSE_cdm, RMSE_STL1, RMSE_STL2, RMSE_STL3, RMSE_STL4)


# lets look at all our RMSE values
RMSE.table

# There is a lot of seasonality in this data set whith high frequency and amplitude.
# Our Classical Decomposition STL4 model has the lowest RMSE value of 7.0138. Therefore,
# this is our preferred 'best' model. Thus, our best model STL4 is actually a decomposition. 
# Although decomposition is not a forecasting model it can be used to make forecasts. 
# on the other hand if we are to ignore the decomposition models then my best model is 
# Moving Average 5 Model with RMSE of 10.686. 
# I believe both my best models should perform adequately, however none of the models
# will be fabulous as there is a lot of variation in the y variable which makes it 
# difficult to predict very accurately.

# Note: We have ignored the RMSE of our CDM Model in our analysis as it is on a different
# scale and we cannot thus compare.
  




