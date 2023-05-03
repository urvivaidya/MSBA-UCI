# HW6 - Urvi Vaidya

library(fpp3)
setwd('~/OneDrive/MAC/UCI MSBA Coursework/Winter 2022/BANA 288 Predictive Analytics/HW 6')

# Part 1. 
# setting seed
set.seed(92925329)

# part 1a.
# creating our error term matrix - white noise
wn_time <- ts(data.frame(rnorm(200)))

wnts <- as_tsibble(wn_time)
names(wnts)

#  Plotting our white noise series 
autoplot(wnts, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(wnts))
autoplot(PACF(wnts))

# looking at both acf and pacf plots we can see that there is not 
# autocorrelation in this series which makes sense as we have randomly,
# generated this series.

# we will continue to use the white noise from this part as our error term for
# the next couple simulations.



# part 1b.
# creating our empty ts matrix
ysim1 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first error term
ysim1[1,1] <- wn_time[1]

# using a loop to create our AR(1) with parameter, phi = 0.6
for (i in 2:200) {
  ysim1[i,1] <- 0.6*ysim1[i-1,1] + wn_time[i]
}

# making our series a tsibble
ysim1 <- as_tsibble(ysim1)

# plotting our series
autoplot(ysim1, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim1))
autoplot(PACF(ysim1))

# we can use the pacf plot to see that this is an AR(1) model, the plot is showing
# one bar/spike and our AR parameter is also clearly visible. 
# The acf plot shows that there is possibly some MA terms too.



# part 1c.
# creating our empty ts matrix
ysim2 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first and second error term
ysim2[1,1] <- wn_time[1]
ysim2[2,1] <- wn_time[2]

# using a loop to create our AR(2) with parameters, phi1 = 0.6 and phi2 = 0.3
for (i in 3:200) {
  ysim2[i,1] <- 0.6*ysim2[i-1,1] + 
    0.3*ysim2[i-2,1] + wn_time[i] 
}

# making our series a tsibble
ysim2 <- as_tsibble(ysim2)

# plotting our series
autoplot(ysim2, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim2))
autoplot(PACF(ysim2))

# our pacf plot is showing two bars/spikes consistent with our AR parameters.
# The acf plot shows that there is possibly several MA possibilities.


# part 1d.
# creating our empty ts matrix
ysim3 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first and second error term
ysim3[1,1] <- wn_time[1]
ysim3[2,1] <- wn_time[2]

# using a loop to create our AR(2) with parameters, phi1 = 0.8 and phi2 = -0.3
for (i in 3:200) {
  ysim3[i,1] <- 0.8*ysim3[i-1,1] + 
    -0.3*ysim3[i-2,1] + wn_time[i] 
}

# making our series a tsibble
ysim3 <- as_tsibble(ysim3)

# plotting our series
autoplot(ysim3, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim3))
autoplot(PACF(ysim3))

# our pacf plot is showing two bars/spikes consistent with our AR parameters.
# The acf plot shows that there is possibly one MA term.




# part 1e.
# creating our empty ts matrix
ysim4 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first error term
ysim4[1,1] <- wn_time[1]

# using a loop to create our MA(1) with parameter, theta = 0.6
for (i in 2:200) {
  ysim4[i,1] <- wn_time[i] + 0.6*wn_time[i-1] 
}

# making our series a tsibble
ysim4 <- as_tsibble(ysim4)

# plotting our series
autoplot(ysim4, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim4))
autoplot(PACF(ysim4))

# Our acf plot shows that there is one MA term. Our pacf plot shows us the 
# possible AR terms.


# part 1f.
# creating our empty ts matrix
ysim5 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first error term
ysim5[1,1] <- wn_time[1]

# using a loop to create our ARMA(1,1) with parameters, phi = 0.5 and theta = 0.4
for (i in 2:200) {
  ysim5[i,1] <- 0.5 * ysim5[i-1,1] + wn_time[i] + 0.4 * wn_time[i-1]
}

# making our series a tsibble
ysim5 <- as_tsibble(ysim5)

# plotting our series
autoplot(ysim5, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim5))
autoplot(PACF(ysim5))

# both out acf and and pacf plots show us one MA and one AR term respectively.


# part 1g.
# creating our empty ts matrix
ysim6 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first two error terms
ysim6[1,1] <- wn_time[1]
ysim6[2,1] <- wn_time[2]

# using a loop to create our ARIMA(1,1,1) with parameters, phi = 0.5 and theta = 0.4
for (i in 3:200) {
  ysim6[i,1] <- ysim6[i-1,1]+0.5*(ysim6[i-1,1]-ysim6[i-2,1]) + wn_time[i] + 
    0.4*wn_time[i-1] 
}


# making our series a tsibble
ysim6 <- as_tsibble(ysim6)

# plotting our series
autoplot(ysim6, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim6))
autoplot(PACF(ysim6))

# we can see our acf plot has several spikes above the cutoff line, this aligns
# with our expectations since we have also added a difference term to our series.
# our pacf plot has two spikes of which one is very significant. 




# part 1h.
# creating our empty ts matrix
ysim7 <- ts(data.frame(matrix(rep(0),200,1)))

# giving our ts the first two error terms
ysim7[1,1] <- wn_time[1]
ysim7[2,1] <- wn_time[2]

# using a loop to creates our ARIMA(1,1,1)(0,1,0)[4] with parameters, phi = 0.5 and theta = 0.4
for (i in 3:200) {
  ysim7[i,1] <- ysim7[i-1,1]+0.5*(ysim7[i-1,1]-ysim7[i-2,1]) + 
    wn_time[i] + 0.4*wn_time[i-1] 
}

# making our series a tsibble
ysim7 <- as_tsibble(ysim7)

# plotting our series
autoplot(ysim7, value)

# Autocorrelation and partial autocorrelation plots
autoplot(ACF(ysim7))
autoplot(PACF(ysim7))

# our acf plot has several spikes and there appears to be a seasonal pattern
# this aligns with our expectations since we have added difference and seasonal 
# parameters when we set our series.
# our pacf plot shows one spike which is probably our AR parameter.




# Part 2.
dat <- read.csv('hw6_USGDP.csv')
str(dat)

# part 2a.
# setting up our tsibble
GDPt <- ts(dat[,2], start = 1947, frequency = 4)
GDPts <- as_tsibble(GDPt)
names(GDPts)[2] <- "GDP"
str(GDPts)

# plotting our data
autoplot(GDPts, GDP) +
  ylab("GDP") +
  xlab("Year/Quarter")

# we can see that the US quarterly gross domestic product (GDP) has consistently
# been on the rise over the years. There is one small dip at around 2008, which 
# makes sense as it coincides with the housing crisis/recession. That dip was
# temporary however, since the GDP began rising up around 2010. We can also see
# some seasonality as the pattern of dips repeats year over year. There is a drop 
# somewhere around Q4 to Q1. 

# box-cox transformation
features(GDPts, GDP, features = guerrero)
lambda <- pull(features(GDPts, GDP, features = guerrero), lambda_guerrero)

# we now have a new column in our time series storing out box-cox transformed GDP
GDPts <- mutate(GDPts, GDPT = box_cox(GDP, lambda)) 
str(GDPts)

# lets plot our transformed data
autoplot(GDPts, GDPT) +
  ylab("Log of GDP") +
  xlab("Year/Quarter")


#  In Arima Modeling we following the following steps
#  1.  Check if series needs to be transformed
#  2.  Identify in the series is stationary - if not
#        compute difference series
#  3.  Figure out the best AR and MA models to 
#        incorporate
#  4.  Make forecasts using ARIMA models
#  5.  Compare ARIMA models to ETS models

# lets look if the series needs to be differenced
#  The null and alternative hypothesis for the KPSS test are
#  Ho:  Data is stationary
#  Ha:  Data is non-stationary

features(GDPts, GDP, unitroot_kpss)

#  A large value of the test statistic, beyond all values given as critical, 
# indicates we would reject the null hypothesis, in this case  our p-value is
# 0.01, thus we do not reject ho. The GDP is stationary. 


# we check how many differences are required
features(GDPts, GDP, unitroot_ndiffs)

# we difference our GDP
GDPts <- mutate(GDPts, GDP_d = difference(GDP,2))


# lets look if the transformed (logged) series needs to be differenced
features(GDPts, GDPT, unitroot_kpss)

# Even in this case  our p-value is 0.01, thus we do not reject ho. 
# The GDPT is stationary. 

# we check how many differences are required
features(GDPts, GDPT, unitroot_ndiffs)

# in this case we only need 1 difference 
GDPts <- mutate(GDPts, GDPT_d = difference(GDPT,1))


# We check if more differences are needed
features(GDPts, GDP_d, unitroot_ndiffs)
features(GDPts, GDPT_d, unitroot_ndiffs)


# we will difference our GDP_d series again
GDPts <- mutate(GDPts, GDP_dd = difference(GDP_d,1))
# we check again
features(GDPts, GDP_dd, unitroot_ndiffs)


#  Both of the series appear stationary  
#  Now test for stationary series with KPSS
features(GDPts, GDP_dd, unitroot_kpss)
features(GDPts, GDPT_d, unitroot_kpss)

#  there is no evidence that either series is non-stationary
#  Even in the already series of differences, there still may be seasonal 
# differences. Thus, we create the seasonal differences. Take the seasonal 
# differences of the already differenced data

GDPts <- mutate(GDPts, GDP_d12 = difference(GDP_dd,12))
GDPts <- mutate(GDPts, GDPT_d12 = difference(GDPT_d,12))

# lets look at our plots
autoplot(PACF(GDPts,GDP_d12))
autoplot(PACF(GDPts,GDPT_d12))

# Both series are still showing autocorrelation

# lets look at the tsdiplay
gg_tsdisplay(GDPts,GDP_d12)
gg_tsdisplay(GDPts,GDPT_d12)

# Our GDP_d12 series appears to indicate an AR5 or MA4 model although there 
# are some other bars on the acf and pacf plots that are significant 

# Our GDPT_d12 series appears to indicate an AR3 or MA2 model although there 
# are some other bars on the acf and pacf plots that are significant 




# part 2b.
# for the purpose of this question we will use the auto and search methods to find
# the best model using our regular and transformed data. 

# fitting a model on our GDP data
fit_1 <- model(GDPts, 
               search = ARIMA(GDP),
               auto = ARIMA(GDP, stepwise = FALSE, approx = FALSE))

#  Let's check the statistics
glance(fit_1)
report(select(fit_1,search))
report(select(fit_1,auto))

RMSE_GDP_search <- accuracy(select(fit_1,search))$RMSE
RMSE_GDP_search
RMSE_GDP_auto <- accuracy(select(fit_1,auto))$RMSE
RMSE_GDP_auto

#  Lets look at the fitted values:
aug_1search <- augment(select(fit_1,search))

aug_1auto <- augment(select(fit_1,auto))

autoplot(aug_1search, color='green') + 
  autolayer(aug_1search, color='red')

# we can see that search gave us an ARIMA(0,1,4)(0,1,2)[4] model and auto gave us an 
# ARIMA(4,1,0)(2,1,0)[4] model. the AIC for our search model is 6448 and auto model is 6444. 
# We can also see from our plot that our fitted values for both models are almost identical.
# RMSE for search is 16978.94 and auto is 16856.92.


# we will also fit a model on our box-cox transformed data GDPT.
fit_1l <- model(GDPts, 
               search = ARIMA(box_cox(GDP, lambda)),
               auto = ARIMA(box_cox(GDP, lambda), stepwise = FALSE, approx = FALSE))


#  Let's check the statistics
glance(fit_1l)
report(select(fit_1l,search))
report(select(fit_1l,auto))

RMSE_GDPT_search <- accuracy(select(fit_1l,search))$RMSE
RMSE_GDPT_search
RMSE_GDPT_auto <- accuracy(select(fit_1l,auto))$RMSE
RMSE_GDPT_auto

#  Lets look at the fitted values:
aug_1lsearch <- augment(select(fit_1l,search))

aug_1lauto <- augment(select(fit_1l,auto))

autoplot(aug_1lsearch, color='green') + 
  autolayer(aug_1lsearch, color='red')

# We can see that search and auto both gave us an ARIMA(0,1,2)(0,1,2)[4] model. 
# The AIC for our search model is 35.3 and auto model is 22.3. 
# We can also see from our plot that our fitted values for both models are 
# almost identical.
# RMSE for search is 16915.28 and auto is 16574.79.


# of the four we find the GDPT_auto model which is ARIMA(0,1,2)(0,1,2)[4]
# model on our box-cox transformed data to be the best with AIC 22.3 and RMSE 16574. 



# part 2c.
# alternative models on our GDP data
fit_2 <- model(GDPts, GDPa4 = ARIMA(GDP ~ pdq(4,0,0) + PDQ(0,1,2)), 
               GDPa2m2 = ARIMA(GDP ~ pdq(2,0,2) + PDQ(0,1,0)),
               GDPa1m1 = ARIMA(GDP ~ pdq(1,0,1) + PDQ(0,1,0)))

#  Let's check the statistics
glance(fit_2)
report(select(fit_2,GDPa4))
report(select(fit_2,GDPa2m2))
report(select(fit_2,GDPa1m1))

RMSE_GDPa4 <- accuracy(select(fit_2,GDPa4))$RMSE
RMSE_GDPa4
RMSE_GDPa2m2 <- accuracy(select(fit_2,GDPa2m2))$RMSE
RMSE_GDPa2m2
RMSE_GDPa1m1 <- accuracy(select(fit_2,GDPa1m1))$RMSE
RMSE_GDPa1m1



# we will also fit a model on our box-cox transformed data GDPT
fit_2l <- model(GDPts, GDPTa4 = ARIMA(box_cox(GDP, lambda) ~ pdq(4,0,0) + PDQ(0,1,2)), 
               GDPTa2m2 = ARIMA(box_cox(GDP, lambda) ~ pdq(2,0,2) + PDQ(0,1,0)),
               GDPTa1m1 = ARIMA(box_cox(GDP, lambda) ~ pdq(1,0,1) + PDQ(0,1,0)))

#  Let's check the statistics
glance(fit_2l)
report(select(fit_2l,GDPTa4))
report(select(fit_2l,GDPTa2m2))
report(select(fit_2l,GDPTa1m1))

RMSE_GDPTa4 <- accuracy(select(fit_2l,GDPTa4))$RMSE
RMSE_GDPTa4
RMSE_GDPTa2m2 <- accuracy(select(fit_2l,GDPTa2m2))$RMSE
RMSE_GDPTa2m2
RMSE_GDPTa1m1 <- accuracy(select(fit_2l,GDPTa1m1))$RMSE
RMSE_GDPTa1m1

# we can see that so far our GDPT (box-xox transformed GDP) auto model - ARIMA(0,1,2)(0,1,2)[4]
# from part 2b performs the best so far with AIC 22.3 and the lowest RMSE of 16574.



# part 2d.
#  Let's check the residuals of our chosen model
gg_tsresiduals(select(fit_1l,auto))

# from our residuals plot we can see that lag 9 and 16 appear to be the only ones
# beyond our threshold. Our reiduals (.resid) appear to be normally distributed and our
# innovation residuals also apear to be normally distributed around 0. Our model appears
# to be doing relatively well.



# part 2e.
# forecasts for the next five years using the chosen model
forc_arima <- forecast(select(fit_1l,auto), h = 20)
autoplot(forc_arima, GDPts)



# part 2f.
# for this section we will compute the ETS on our box-cox transformed data
# since that is what we used for our ARIMA model, for better comparison.

# ETS model
fit_ETS <- model(GDPts,ETS(box_cox(GDP, lambda)))

# lets look at our model
report(fit_ETS)

# ETS chosen Model: ETS(A,A,A) with Smoothing parameters:
# alpha = 0.811596 
# beta  = 0.2500265 
# gamma = 0.1884039 

# lets compare the accuracy for our ETS and ARIMA models
accuracy(fit_ETS)
accuracy(select(fit_1l,auto))

# fit_ETS RMSE - 18101
# fit_arima RMSE - 16575

# lets create our forecasts
forc_ETS <- forecast(fit_ETS, h = 20)
autoplot(forc_ETS, GDPts)

# lets look at both our forecasts
autoplot(GDPts, color = 'black') +
  autolayer(forc_ETS, color = 'green', level = NULL) +
  autolayer(forc_arima, color = 'blue', level = NULL)


# we can see from our model ac curacies that the RMSE of our ARIMA model is lower at 
# 16575 compared to that of the ETS model at 18101, therefore in terms of error our ARIMA
# model has performed better. 





# Part 3.
# lets read our family homes data
dat2 <- read.csv('hw6_one_family_homes.csv')
str(dat2)

# we can see that our data is set up monthly however we only have 3 datapoints for the 
# last year of the data

# part 3a.
# setting up our tsibble
homes <- ts(dat2[,2], start = 1963, frequency = 12)
homests <- as_tsibble(homes)
names(homests)[2] <- "sales"
str(homests)

# plotting our data
autoplot(homests, sales) +
  ylab("Sales") +
  xlab("Year/Month")

# we can see that the US singly family home sales do not appear to have any
# fixed cycle/pattern, the seem volatile. We can also see a huge dip around 2007-08
# which aligns with the recession after the housing bubble. 

# box-cox transformation
features(homests, sales, features = guerrero)
lambda <- pull(features(homests, sales, features = guerrero), lambda_guerrero)

# we now have a new column in our time series storing out box-cox transformed sales
homests <- mutate(homests, salesT = box_cox(sales, lambda)) 
str(homests)

# lets plot our transformed data
autoplot(homests, salesT) +
  ylab("Log of Sales") +
  xlab("Year/Month")


# lets look if the series needs to be differenced
#  The null and alternative hypothesis for the KPSS test are
#  Ho:  Data is stationary
#  Ha:  Data is non-stationary

features(homests, sales, unitroot_kpss)
# In this case  our p-value is 0.01, thus we do not reject ho. The sales are stationary. 


# we check how many differences are required
features(homests, sales, unitroot_ndiffs)

# we difference our GDP
homests <- mutate(homests, sales_d = difference(sales,1))


# lets look if the transformed (logged) series needs to be differenced
features(homests, salesT, unitroot_kpss)

# Even in this case  our p-value is 0.01, thus we do not reject ho. 
# The salesT are stationary. 

# we check how many differences are required
features(homests, salesT, unitroot_ndiffs)

# in this case we only need 1 difference 
homests <- mutate(homests, salesT_d = difference(salesT,1))


# We check if more differences are needed
features(homests, sales_d, unitroot_ndiffs)
features(homests, salesT_d, unitroot_ndiffs)


#  Both of the series appear stationary  
#  Now test for stationary series with KPSS
features(homests, sales_d, unitroot_kpss)
features(homests, salesT_d, unitroot_kpss)

# There is no evidence that either series is non-stationary
# We create the seasonal differences. Take the seasonal 
# differences of the already differenced data

homests <- mutate(homests, sales_d12 = difference(sales_d,12))
homests <- mutate(homests, salesT_d12 = difference(salesT_d,12))

# lets look at our plots
autoplot(PACF(homests,sales_d12))
autoplot(PACF(homests,salesT_d12))

# Both series are still showing autocorrelation

# lets look at the tsdiplay
gg_tsdisplay(homests,sales_d12)
gg_tsdisplay(homests,salesT_d12)

# Our sales_d12 series appears to indicate an AR5 or MA2 model although there 
# are some other bars on the acf and pacf plots that are significant 

# Our salesT_d12 series appears to indicate an AR5 or MA2 model although there 
# are some other bars on the acf and pacf plots that are significant 




# part 3b.
# for the purpose of this question we will use the auto and search methods to find
# the best model using our regular and transformed data. 

# fitting a model on our GDP data
fit_11 <- model(homests, 
               search = ARIMA(sales),
               auto = ARIMA(sales, stepwise = FALSE, approx = FALSE))

#  Let's check the statistics
glance(fit_11)
report(select(fit_11,search))
report(select(fit_11,auto))

RMSE_sales_search <- accuracy(select(fit_11,search))$RMSE
RMSE_sales_search
RMSE_sales_auto <- accuracy(select(fit_11,auto))$RMSE
RMSE_sales_auto

#  Lets look at the fitted values:
aug_11search <- augment(select(fit_11,search))

aug_11auto <- augment(select(fit_11,auto))

autoplot(aug_11search, color='green') + 
  autolayer(aug_11search, color='red')

# we can see that search gave us an ARIMA(1,1,2)(1,0,2)[12] model and auto gave us an 
# ARIMA(2,1,2)(1,0,1)[12] model. the AIC for our search model is 7158 and auto model is 7151. 
# RMSE for search is 44.1032 and auto is 43.8910. We can also see from our plot that our fitted 
# values for both models are almost identical.


# we will also fit a model on our box-cox transformed data GDPT.
fit_11l <- model(homests, 
                search = ARIMA(box_cox(sales, lambda)),
                auto = ARIMA(box_cox(sales, lambda), stepwise = FALSE, approx = FALSE))


#  Let's check the statistics
glance(fit_11l)
report(select(fit_11l,search))
report(select(fit_11l,auto))

RMSE_salesT_search <- accuracy(select(fit_11l,search))$RMSE
RMSE_salesT_search
RMSE_salesT_auto <- accuracy(select(fit_11l,auto))$RMSE
RMSE_salesT_auto

#  Lets look at the fitted values:
aug_11lsearch <- augment(select(fit_11l,search))
aug_11lauto <- augment(select(fit_11l,auto))

autoplot(aug_11lsearch, color='green') + 
  autolayer(aug_11lsearch, color='red')

# We can see that search gave us an ARIMA(0,1,1)(1,0,2)[12] model and auto gave us
# ARIMA(2,1,2)(1,0,1)[12]. We can see that both the sales and transformed sales have
# had the same models in the automatic model.
# The AIC for our search model is 3529 and auto model is 3524. 
# RMSE for search is 44.086 and auto is 43.9424
# We can also see from our plot that our fitted values for both models are very similar.


# of the four we find the salesT_auto model which is ARIMA(2,1,2)(1,0,1)[12]
# model on our box-cox transformed data to be the best with AIC 3524 and RMSE 43.9424



# part 3c.
# alternative models on our GDP data
fit_22 <- model(homests, salesa4 = ARIMA(sales ~ pdq(4,0,0) + PDQ(0,1,0)), 
               salesa2m2 = ARIMA(sales ~ pdq(2,0,2) + PDQ(0,1,0)),
               salesa1m1 = ARIMA(sales ~ pdq(1,0,1) + PDQ(0,1,0)))

#  Let's check the statistics
glance(fit_22)
report(select(fit_22,salesa4))
report(select(fit_22,salesa2m2))
report(select(fit_22,salesa1m1))

RMSE_salesa4 <- accuracy(select(fit_22,salesa4))$RMSE
RMSE_salesa4
RMSE_salesa2m2 <- accuracy(select(fit_22,salesa2m2))$RMSE
RMSE_salesa2m2
RMSE_salesa1m1 <- accuracy(select(fit_22,salesa1m1))$RMSE
RMSE_salesa1m1



# we will also fit a model on our box-cox transformed data
fit_22l <- model(homests, salesTa4 = ARIMA(box_cox(sales, lambda) ~ pdq(4,0,0) + PDQ(0,1,0)), 
                salesTa2m2 = ARIMA(box_cox(sales, lambda) ~ pdq(2,0,2) + PDQ(0,1,0)),
                salesTa1m1 = ARIMA(box_cox(sales, lambda) ~ pdq(1,0,1) + PDQ(0,1,0)))

#  Let's check the statistics
glance(fit_22l)
report(select(fit_22l,salesTa4))
report(select(fit_22l,salesTa2m2))
report(select(fit_22l,salesTa1m1))

RMSE_salesTa4 <- accuracy(select(fit_22l,salesTa4))$RMSE
RMSE_salesTa4
RMSE_salesTa2m2 <- accuracy(select(fit_22l,salesTa2m2))$RMSE
RMSE_salesTa2m2
RMSE_salesTa1m1 <- accuracy(select(fit_22l,salesTa1m1))$RMSE
RMSE_salesTa1m1


# we can see that so far our salesT (box-xox transformed sales) auto model - ARIMA(2,1,2)(1,0,1)[12] 
# from part 3b is performs the best so far with AIC 3524 and lowest RMSE of 43.9424



# part 3d.
#  Let's check the residuals of our chosen model
gg_tsresiduals(select(fit_11l,auto))

# from our residuals plot we can see that lag 20 and 21 appear to be the only ones
# beyond our threshold. Our reiduals (.resid) appear to be normally distributed and our
# innovation residuals also apear to be almost normally distributed around 0. Our model appears
# to be doing all right.




# part 3e.
# forecasts for the next five years using the chosen model
forc_arima_homes <- forecast(select(fit_11l,auto), h = 60)
autoplot(forc_arima_homes, homests)



# part 3f.
# for this section we will compute the ETS on our box-cox transformed data
# since that is what we used for our ARIMA model, for better comparison.

# ETS model
fit_ETS_homes <- model(homests,ETS(box_cox(sales, lambda)))

# lets look at our model
report(fit_ETS_homes)

# ETS chosen Model:ETS(A,N,N) with Smoothing parameter alpha = 0.7465332

# lets compare the accuracy for our ETS and ARIMA models
accuracy(fit_ETS_homes)
accuracy(select(fit_11l,auto))

# fit_ETS RMSE - 45
# fit_arima RMSE - 43

# lets create our forecasts
forc_ETS_homes <- forecast(fit_ETS_homes, h = 60)
autoplot(forc_ETS_homes, homests)

# lets look at both our forecasts
autoplot(homests, color = 'black') +
  autolayer(forc_ETS_homes, color = 'green', level = NULL) +
  autolayer(forc_arima_homes, color = 'blue', level = NULL)


# we can see from our model accuracies that the RMSE of our ARIMA model is lower at 
# 43 compared to that of the ETS model at 45, therefore in terms of error our ARIMA
# model has performed better. 






