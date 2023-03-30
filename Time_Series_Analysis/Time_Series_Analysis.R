################################
# DS 809 Time Series
# Spring 2023
# Pegah Karimi & Amie Rowland
# Final Project: Apex Legends
################################

# libraries
library(tidyverse)
library(car) # vif
library(skedastic) # white test for heteroskedasticity
library(TSA) # periodogram
library(fGarch) # garch

# read in the data
data <- read.csv("/Users/pegahkarimi/MSBA/Term 3/Time series analysis/Project/data.csv")

head(data)
data$Weekday <- as.factor(data$Weekday)
data$Month <- as.factor(data$Month)
data$Season <- as.factor(data$Season)
data$RankedMap <- as.factor(data$RankedMap)
data$ALGSEvent <- as.factor(data$ALGSEvent)
data$Release <- as.factor(data$Release)
data$InGameEvent <- as.factor(data$InGameEvent)

data <- data %>% 
  select(-DateTime)


# split: 800 train, last 42 for test
train <- data[1:800,]
test <- data[801:842,]

# confirm the dependent variable is not white noise
plot.ts(train$ApexPlayers, ylab="Apex Players")
acf(train$ApexPlayers, main="ACF Apex Players")
pacf(train$ApexPlayers)
acf(diff(train$ApexPlayers), main="ACF Diff Apex Players")
pacf(diff(train$ApexPlayers))
Box.test(diff(train$ApexPlayers), type="Ljung", lag=160)
# visually not WN
# small p-value confirms not WN

# Regression model

model2.1 <- lm(ApexPlayers~.-Weekday-Month-Season, data=train)
summary(model2.1)

# remove PUBG twitch (not statistically significant)
model2.2 <- lm(ApexPlayers~.-Weekday-Month-Season-PUBGTwitchViewers, data=train)
summary(model2.2)

model2.3 <- lm(ApexPlayers~.-Release-Weekday-Month-Season-PUBGTwitchViewers, data=train)
summary(model2.3)
BIC(model2.1)
BIC(model2.2)
BIC(model2.3)

AIC(model2.1)
AIC(model2.2)
AIC(model2.3)

# check multicollinearity 
vif(model2.3)

# get residuals to check assumptions
residuals2.2 <- model2.2$residuals
residuals2.2

residuals2.3 <- model2.3$residuals

# check residual assumption: normality
shapiro.test(residuals2.2)
shapiro.test(residuals2.3)
hist(residuals2.2,breaks=20, col="steelblue")
# check if applies to non-standardized residuals
# small p-value so not normal (fail assumption)


# check residual assumption: non-constant variance
white(model2.2, interactions = TRUE)
# large p-value
# this is fine

# check residual assumption: autocorrelation
plot(residuals2.3)
acf(residuals2.3)
acf(residuals2.3, plot=FALSE)
Box.test(residuals2.3, lag=50, type = 'Ljung-Box')

 # not white noise (fail assumption)


# plot
plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Linear Regression (Purple)")
lines(predict(model2.3),col="purple4")
grid()

# REPORT PART 3: DETERMINISTIC TIME SERIES MODELS

# 3a: Indicator

# only time and indicators
model3a.0 <- lm(ApexPlayers~Index + Season + Weekday + Month, data=train)
summary(model3a.0)

# checking and solving multicollinearity
vif(model3a.0)
model3a.01 <- lm(ApexPlayers~ Season + Weekday + Month, data=train)
summary(model3a.01)
vif(model3a.01)

AIC(model3a.01)

plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Indicator")
lines(predict(model3a.01),col="red")
grid()

# standardized residuals
sres3<-rstandard(model3a.01)
sres3

# check residual assumption: normality
shapiro.test(sres3)
hist(sres3,breaks=20, col="steelblue", main="Normality")
# check if applies to non-standardized residuals
# small p-value so not normal (fail assumption)


# check residual assumption: non-constant variance
white(model3a.01, interactions = TRUE)
# large p-value
# this is fine

# check residual assumption: autocorrelation
acf(sres3, main="ACF Residuals")
acf(sres3, plot=FALSE)
Box.test(sres3, lag=20, type = 'Ljung-Box')
# they are autocorrelated

# indicators season, day of week, month added to regression model

model3a.1 <- lm(ApexPlayers~., data=train)
summary(model3a.1)

# solve collinearity issue
model3a.2 <- lm(ApexPlayers~.-RankedMap, data=train)
summary(model3a.2)

# check multicollinearity
vif(model3a.2)

# remove index due to multicollinearity
model3a.3 <- lm(ApexPlayers~.-RankedMap-Index, data=train)
summary(model3a.3)

# remove PUBG twitch (not statistically significant)
# also remove Release
# also remove CSGO Twitch (singularity)
model3a.4 <- lm(ApexPlayers~.-RankedMap-PUBGTwitchViewers-Release-Index-CSGOTwitchViewers, data=train)
summary(model3a.4)
AIC(model3a.4)

plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="")
lines(predict(model3a.4),col="red")
grid()

# check multicollinearity again
vif(model3a.4)

# get residuals to check assumptions
sres3a<-rstandard(model3a.4)
sres3a
# NaN problem
# solved by not standardizing
residuals3a.4 <- model3a.4$residuals
residuals3a.4

# check residual assumption: normality
shapiro.test(residuals3a.4)
hist(residuals3a.4,breaks=20, col="steelblue")
# check if applies to non-standardized residuals
# small p-value so not normal (fail assumption)


# check residual assumption: non-constant variance
white(model3a.4, interactions = TRUE)
# large p-value
# this is fine

# check residual assumption: autocorrelation
acf(residuals3a.4, main="ACF Residuals")
acf(residuals3a.4, plot=FALSE)
Box.test(residuals3a.4, lag=20, type = 'Ljung-Box')
# not white noise (fail assumption)

# 3b: Polynomial

k=16
model3b <- lm(ApexPlayers~poly(Index,k), data=train)
summary(model3b)

AIC(model3b)

# investigate if residuals are autocorrelated
sres3b<-rstandard(model3b)
acf(sres3b, main="ACF Residuals")
acf(sres3b, plot=FALSE)
Box.test(sres3b, lag=20, type = 'Ljung-Box')
# they are autocorrelated

# plot
plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Polynomial")
lines(predict(model3b),col="red")
grid()


# 3c: Cyclical (Harmonic)

# check if de-trending is needed
model3c.1 <- lm(ApexPlayers ~ Index, data = train)
summary(model3c.1)

# de-trend is needed
periodogram(model3c.1$residuals)
periodogram(model3c.1$residuals)$spec
periodogram(model3c.1$residuals)$freq
order(periodogram(model3c.1$residuals)$spec)

n <- length(train$ApexPlayers)
data$sin2 <- sin(2*pi*data$Index*2/n)
data$cos2 <- cos(2*pi*data$Index*2/n)
data$sin5 <- sin(2*pi*data$Index*5/n)
data$cos5 <- cos(2*pi*data$Index*5/n)
data$sin9 <- sin(2*pi*data$Index*9/n)
data$cos9 <- cos(2*pi*data$Index*9/n)
data$sin4 <- sin(2*pi*data$Index*4/n)
data$cos4 <- cos(2*pi*data$Index*4/n)

train.harmonic = data[1:800,]
test.harmonic = data[801:842,]

model3c.2 <- lm(ApexPlayers~Index + sin2 + cos2 + sin5 + cos5 + sin9 + cos9 + sin4 + cos4, data=train.harmonic)
summary(model3c.2)

periodogram(model3c.2$residuals)
periodogram(model3c.2$residuals)$spec
periodogram(model3c.2$residuals)$freq
order(periodogram(model3c.2$residuals)$spec)

# the order of periodogram has changed compared to the past one

data$sin8 <- sin(2*pi*data$Index*8/n)
data$cos8 <- cos(2*pi*data$Index*8/n)
data$sin17 <- sin(2*pi*data$Index*17/n)
data$cos17 <- cos(2*pi*data$Index*17/n)
data$sin3 <- sin(2*pi*data$Index*3/n)
data$cos3 <- cos(2*pi*data$Index*3/n)
data$sin7 <- sin(2*pi*data$Index*7/n)
data$cos7 <- cos(2*pi*data$Index*7/n)
data$sin6 <- sin(2*pi*data$Index*6/n)
data$cos6 <- cos(2*pi*data$Index*6/n)
data$sin10 <- sin(2*pi*data$Index*10/n)
data$cos10 <- cos(2*pi*data$Index*10/n)
data$sin1 <- sin(2*pi*data$Index*1/n)
data$cos1 <- cos(2*pi*data$Index*1/n)
data$sin14 <- sin(2*pi*data$Index*14/n)
data$cos14 <- cos(2*pi*data$Index*14/n)
data$sin114 <- sin(2*pi*data$Index*114/n)
data$cos114 <- cos(2*pi*data$Index*114/n)
data$sin19 <- sin(2*pi*data$Index*19/n)
data$cos19 <- cos(2*pi*data$Index*19/n)
data$sin26 <- sin(2*pi*data$Index*26/n)
data$cos26 <- cos(2*pi*data$Index*26/n)
data$sin22 <- sin(2*pi*data$Index*22/n)
data$cos22 <- cos(2*pi*data$Index*22/n)
data$sin18 <- sin(2*pi*data$Index*18/n)
data$cos18 <- cos(2*pi*data$Index*18/n)
data$sin35 <- sin(2*pi*data$Index*35/n)
data$cos35 <- cos(2*pi*data$Index*35/n)
data$sin11 <- sin(2*pi*data$Index*11/n)
data$cos11 <- cos(2*pi*data$Index*11/n)
data$sin12 <- sin(2*pi*data$Index*12/n)
data$cos12 <- cos(2*pi*data$Index*12/n)
data$sin229 <- sin(2*pi*data$Index*229/n)
data$cos229 <- cos(2*pi*data$Index*229/n)
data$sin115 <- sin(2*pi*data$Index*115/n)
data$cos115 <- cos(2*pi*data$Index*115/n)
data$sin43 <- sin(2*pi*data$Index*43/n)
data$cos43 <- cos(2*pi*data$Index*43/n)
data$sin20 <- sin(2*pi*data$Index*20/n)
data$cos20 <- cos(2*pi*data$Index*20/n)
data$sin36 <- sin(2*pi*data$Index*36/n)
data$cos36 <- cos(2*pi*data$Index*36/n)
data$sin23 <- sin(2*pi*data$Index*23/n)
data$cos23 <- cos(2*pi*data$Index*23/n)
data$sin45 <- sin(2*pi*data$Index*45/n)
data$cos45 <- cos(2*pi*data$Index*45/n)
data$sin28 <- sin(2*pi*data$Index*28/n)
data$cos28 <- cos(2*pi*data$Index*28/n)
data$sin69 <- sin(2*pi*data$Index*69/n)
data$cos69 <- cos(2*pi*data$Index*69/n)
data$sin228 <- sin(2*pi*data$Index*228/n)
data$cos228 <- cos(2*pi*data$Index*228/n)
data$sin97 <- sin(2*pi*data$Index*97/n)
data$cos97 <- cos(2*pi*data$Index*97/n)
data$sin21 <- sin(2*pi*data$Index*21/n)
data$cos21 <- cos(2*pi*data$Index*21/n)
data$sin24 <- sin(2*pi*data$Index*24/n)
data$cos24 <- cos(2*pi*data$Index*24/n)
data$sin52 <- sin(2*pi*data$Index*52/n)
data$cos52 <- cos(2*pi*data$Index*52/n)
data$sin54 <- sin(2*pi*data$Index*54/n)
data$cos54 <- cos(2*pi*data$Index*54/n)
data$sin132 <- sin(2*pi*data$Index*132/n)
data$cos132 <- cos(2*pi*data$Index*132/n)
data$sin37 <- sin(2*pi*data$Index*37/n)
data$cos37 <- cos(2*pi*data$Index*37/n)
data$sin13 <- sin(2*pi*data$Index*13/n)
data$cos13 <- cos(2*pi*data$Index*13/n)
data$sin33 <- sin(2*pi*data$Index*33/n)
data$cos33 <- cos(2*pi*data$Index*33/n)
data$sin51 <- sin(2*pi*data$Index*51/n)
data$cos51 <- cos(2*pi*data$Index*51/n)
data$sin62 <- sin(2*pi*data$Index*62/n)
data$cos62 <- cos(2*pi*data$Index*62/n)
data$sin80 <- sin(2*pi*data$Index*80/n)
data$cos80 <- cos(2*pi*data$Index*80/n)
data$sin106 <- sin(2*pi*data$Index*106/n)
data$cos106 <- cos(2*pi*data$Index*106/n)
data$sin30 <- sin(2*pi*data$Index*30/n)
data$cos30 <- cos(2*pi*data$Index*30/n)
data$sin78 <- sin(2*pi*data$Index*78/n)
data$cos78 <- cos(2*pi*data$Index*78/n)
data$sin88 <- sin(2*pi*data$Index*88/n)
data$cos88 <- cos(2*pi*data$Index*88/n)
data$sin77 <- sin(2*pi*data$Index*77/n)
data$cos77 <- cos(2*pi*data$Index*77/n)
data$sin211 <- sin(2*pi*data$Index*211/n)
data$cos211 <- cos(2*pi*data$Index*211/n)
data$sin61 <- sin(2*pi*data$Index*61/n)
data$cos61 <- cos(2*pi*data$Index*61/n)
data$sin40 <- sin(2*pi*data$Index*40/n)
data$cos40 <- cos(2*pi*data$Index*40/n)
data$sin105 <- sin(2*pi*data$Index*105/n)
data$cos105 <- cos(2*pi*data$Index*105/n)
data$sin29 <- sin(2*pi*data$Index*29/n)
data$cos29 <- cos(2*pi*data$Index*29/n)
data$sin79 <- sin(2*pi*data$Index*79/n)
data$cos79 <- cos(2*pi*data$Index*79/n)
data$sin25 <- sin(2*pi*data$Index*25/n)
data$cos25 <- cos(2*pi*data$Index*25/n)
data$sin131 <- sin(2*pi*data$Index*131/n)
data$cos131 <- cos(2*pi*data$Index*131/n)
data$sin57 <- sin(2*pi*data$Index*57/n)
data$cos57 <- cos(2*pi*data$Index*57/n)
data$sin116 <- sin(2*pi*data$Index*116/n)
data$cos116 <- cos(2*pi*data$Index*116/n)
data$sin15 <- sin(2*pi*data$Index*15/n)
data$cos15 <- cos(2*pi*data$Index*15/n)
data$sin96 <- sin(2*pi*data$Index*96/n)
data$cos96 <- cos(2*pi*data$Index*96/n)
data$sin119 <- sin(2*pi*data$Index*119/n)
data$cos119 <- cos(2*pi*data$Index*119/n)
data$sin160 <- sin(2*pi*data$Index*160/n)
data$cos160 <- cos(2*pi*data$Index*160/n)
data$sin75 <- sin(2*pi*data$Index*75/n)
data$cos75 <- cos(2*pi*data$Index*75/n)
data$sin53 <- sin(2*pi*data$Index*53/n)
data$cos53 <- cos(2*pi*data$Index*53/n)
data$sin95 <- sin(2*pi*data$Index*95/n)
data$cos95 <- cos(2*pi*data$Index*95/n)
data$sin123 <- sin(2*pi*data$Index*123/n)
data$cos123 <- cos(2*pi*data$Index*123/n)
data$sin31 <- sin(2*pi*data$Index*31/n)
data$cos31 <- cos(2*pi*data$Index*31/n)
data$sin27 <- sin(2*pi*data$Index*27/n)
data$cos27 <- cos(2*pi*data$Index*27/n)
# these were not significant so removed from model and stopped here
# data$sin230 <- sin(2*pi*data$Index*230/n)
# data$cos230 <- cos(2*pi*data$Index*239/n)

train.harmonic = data[1:800,]
test.harmonic = data[801:842,]

model3c.3 <- lm(ApexPlayers~Index + sin2 + cos2 + sin5 + cos5 + sin9 + cos9
              + sin4 + cos4 + sin8 + cos8 + sin17 + cos17 + sin3 + cos3 + sin7 + cos7
              + sin6 + cos6 + sin10 + cos10 + sin1 + cos1 + sin14 + cos14 + sin114 + cos114
              + sin19 + cos19 + sin26 + cos26 + sin22 + cos22 + sin18 + cos18
              + sin35 + cos35 + sin11 + cos11 + sin12 + cos12 + sin229 + cos229
              + sin115 + cos115 + sin43 + cos43 + sin20 + cos20 + sin36 + cos36
              + sin23 + cos23 + sin45 + cos45 + sin28 + cos28 + sin69 + cos69
              + sin228 + cos228 + sin97 + cos97 + sin21 + cos21 + sin24 + cos24
              + sin52 + cos52 + sin54 + cos54 + sin132 + cos132 + sin37 + cos37
              + sin13 + cos13 + sin33 + cos33 + sin51 + cos51 + sin62 + cos62
              + sin80 + cos80 + sin106 + cos106 + sin30 + cos30 + sin78 + cos78
              + sin88 + cos88 + sin77 + cos77 + sin211 + cos211 + sin61 + cos61
              + sin40 + cos40 + sin105 + cos105 + sin29 + cos29 + sin79 + cos79
              + sin25 + cos25 + sin131 + cos131 + sin57 + cos57 + sin116 + cos116
              + sin15 + cos15 + sin96 + cos96 + sin119 + cos119 + sin160 + cos160
              + sin75 + cos75 + sin53 + cos53 + sin95 + cos95 + sin123 + cos123
              + sin31 + cos31 + sin27 + cos27, data=train.harmonic)
summary(model3c.3)
AIC(model3c.2)
AIC(model3c.3)

model3c.4 <- lm(ApexPlayers~Index + sin2 + cos2 + sin5 + cos5 + sin9 + cos9
                + sin4 + cos4 + sin8 + cos8 + sin17 + cos17 + sin3 + cos3 + sin7 + cos7
                + sin6 + cos6 + sin10 + cos10 + sin1 + cos1 + sin14 + cos14 + sin114 + cos114
                + sin19 + cos19 + sin26 + cos26 + sin22 + cos22 + sin18 + cos18
                + sin35 + cos35 + sin11 + cos11 + sin12 + cos12 + sin229 + cos229
                + sin115 + cos115 + sin43 + cos43 + sin20 + cos20 + sin36 + cos36
                + sin23 + cos23 + sin45 + cos45 + sin28 + cos28 + sin69 + cos69
                + sin228 + cos228 + sin97 + cos97 + sin21 + cos21 + sin24 + cos24
                + sin52 + cos52 + sin54 + cos54 + sin132 + cos132 + sin37 + cos37
                + sin13 + cos13 + sin33 + cos33 + sin51 + cos51 + sin62 + cos62
                + sin80 + cos80 + sin106 + cos106, data=train.harmonic)
summary(model3c.4)
AIC(model3c.4)


periodogram(model3c.3$residuals)
periodogram(model3c.3$residuals)$spec
periodogram(model3c.3$residuals)$freq
order(periodogram(model3c.3$residuals)$spec)

# check if residuals are autocorrelated
sres3c<-rstandard(model3c.3)
acf(sres3c, main="ACF Residuals")
acf(sres3c, plot=FALSE)
Box.test(sres3c, lag=20, type = 'Ljung-Box')
# they are autocorrelated

# plot
plot.ts(train.harmonic$ApexPlayers, col="darkgray", ylab="Players", main="Harmonic (Red)")
lines(predict(model3c.4),col="red")
grid()



# REPORT PART 4: STOCHASTIC TIME SERIES MODEL

# 4a: AR, MA, ARMA, ARIMA, SARIMA

plot.ts(diff(train$ApexPlayers))

acf(diff(train$ApexPlayers), lag=160, main="ACF Difference")
acf(diff(train$ApexPlayers), plot=FALSE,lag=160)
pacf(diff(train$ApexPlayers), main="PACF Difference")

par(mfrow=c(1,2))
acf(diff(diff(train$ApexPlayers), lag=7),lag=100)
pacf(diff(diff(train$ApexPlayers), lag=7),lag=100)

model4a <- arima(train$ApexPlayers, order = c(1, 1, 1), seasonal=list(order=c(1,1,1), period=91) )
model4a

model4ap <- arima(train$ApexPlayers, order = c(1, 1, 1), seasonal=list(order=c(0,1,1), period=91) )
model4ap


# I tried a bunch of other options (some of them took a while to run)
# this is the best AIC I think
# at first I thought lag 7 would be the option but then I extended the lag
# presumably it's picking up the season starts

par(mfrow=c(1,1))
acf(model4a$residuals, lag=160)
Box.test(model4a$residuals,lag=160)

acf(model4ap$residuals, lag=160)
Box.test(model4ap$residuals,lag=160)

Box.test(model4a$residuals,type="Ljung",lag=20)

acf(model4ap$residuals, lag=20, main="ACF Residuals")
Box.test(model4ap$residuals,type="Ljung",lag=20)
# residuals are white noise

plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Sarima")
lines(train$ApexPlayers - model4ap$residuals,col="red")
grid()


# 4b: ARIMA corrections for Part 2 and Part 3 models

# 2, 3a, 3b, 3c all autocorrelated

# MAYBE THESE WANT TO BE SARIMA??? none of them actually get to true white noise
# they all result in seasonal spikes


# model 3a.01 correction

par(mfrow=c(1,1))
acf(model3a.01$residuals, main="ACF Residuals")
pacf(model3a.01$residuals, main="PACF Residuals")
acf(diff(model3a.01$residuals), main="ACF Diff(1) Residuals")
pacf(diff(model3a.01$residuals), main="PACF Diff(1) Residuals")
Box.test(diff(model3a.01$residuals), type="Ljung", lag=30)

arima3a.01.1 <- arima(model3a.01$residuals, order=c(0,1,2))
arima3a.01.1
acf(arima3a.01.1$residuals, lag.max = 20)
Box.test(arima3a.01.1$residuals, type="Ljung", lag=30)

arima_corrected3a.01.1 <- arima(train$ApexPlayers, order=c(0,1,2), seasonal=list(order=c(1,0,1), period=7),
                               xreg=cbind(train$Weekday, train$Month, train$Season))


arima_corrected3a.01.1
acf(arima_corrected3a.01.1$residuals, lag.max=20, main="Corrected ACF Residuals")
pacf(arima_corrected3a.01.1$residuals, lag=20)
Box.test(arima_corrected3a.01.1$residuals, type="Ljung", lag=20)


plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Corrected Indicator")
lines(train$ApexPlayers - arima_corrected3a.01.1$residuals,col="red")
grid()


# model 3a.4 correction

acf(residuals3a.4, main="ACF Residuals")
pacf(residuals3a.4, main="PACF Residuals")
acf(diff(residuals3a.4), lag.max = 30, main="ACF Diff(1) Residuals")
pacf(diff(residuals3a.4), lag.max = 30,  main="PACF Diff(1) Residuals")
Box.test(diff(residuals3a.4), type="Ljung", lag=30)

arima3a.4.1 <- arima(residuals3a.4, order=c(1,0,0))
arima3a.4.1
acf(arima3a.4.1$residuals, lag.max = 20)
Box.test(arima3a.4.1$residuals, type="Ljung", lag=30)

arima_corrected3a.4.1 <- arima(train$ApexPlayers, order=c(0,1,1), seasonal=list(order=c(1,0,1), period=7),
                            xreg=cbind(train$ApexTwitchViewers, train$PUBGPlayers,train$CSGOPlayers, 
                                       train$Weekday, train$Month, train$Season, train$FirstDayOfSeason, 
                                       train$RankedSplitStart, train$ALGSEvent, train$InGameEvent))
# Singularities!!
# CSGO Twitch Viewers
# Season
# FirstDayOfSeason

arima_corrected3a.4.1
acf(arima_corrected3a.4.1$residuals, lag=20, main="Corrected ACF Residuals")
pacf(arima_corrected3a.4.1$residuals, lag=100)
Box.test(arima_corrected3a.4.1$residuals, type="Ljung",lag=20)

plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Corrected Regression")
lines(train$ApexPlayers - arima_corrected3a.4.1$residuals,col="red")
grid()


# model 2.3 correction 

acf(residuals2.3)
acf(diff(residuals2.3))
acf(diff(residuals2.3, lag=7))

acf(diff(diff(residuals2.3, lag=7)),lag=50)
acf(diff(diff(residuals2.3)),lag=30)
pacf(diff(diff(residuals2.3)),lag=30)
pacf(diff(diff(residuals2.3, lag=7)),lag=20)


arima2.3 <- arima(model2.3$residuals, order=c(0,2,3))
arima2.3
acf(arima2.3$residuals, lag.max = 20)

arima_corrected2.3 <- arima(train$ApexPlayers, order=c(0,2,3),
                            xreg=cbind(train$Index, train$ApexTwitchViewers, train$PUBGPlayers,
                                       train$CSGOPlayers, train$CSGOTwitchViewers, train$FirstDayOfSeason,
                                       train$RankedSplitStart, train$RankedMap, train$ALGSEvent, train$InGameEvent))

arima_corrected2.3
acf(arima_corrected2.3$residuals, lag=100)
pacf(arima_corrected2.3$residuals, lag=100)
Box.test(arima_corrected2.3$residuals, lag=20)


# Polynomial correction 3b

acf(model3b$residuals)
pacf(model3b$residuals)

acf(diff(model3b$residuals))
pacf(diff(model3b$residuals))

acf(diff(model3b$residuals, 2))
pacf(diff(model3b$residuals, 2))


arima3b.1 <- arima(model3b$residuals, order=c(2,1,0), seasonal=list(order=c(1,0,1),period=7))
arima3b.1
acf(arima3b.1$residuals, lag.max = 20)
Box.test(arima3b.1$residuals, type="Ljung", lag=20)


arima_corrected3b.1 <- arima(train$ApexPlayers, order=c(0,2,1), #seasonal=list(order=c(1,0,1), period=7), 
                             xreg=poly(train$Index,1))

arima_corrected3b.1
acf(arima_corrected3b.1$residuals, lag=100)
pacf(arima_corrected3b.1$residuals, lag=100)
Box.test(arima_corrected3b.1$residuals, lag=20)


# 3c.4

plot(model3c.4$residuals)
acf(model3c.4$residuals, main="ACF Residuals")
pacf(model3c.4$residuals, main="PACF Residuals")

acf(diff(model3c.4$residuals))
pacf(diff(model3c.4$residuals))

arima3c.4 <- arima(model3c.4$residuals, order=c(0,1,3))
arima3c.4

acf(arima3c.4$residuals)

Box.test(arima3c.4$residuals, type="Ljung",lag=20)

arima_corrected3c.4 <-arima(train.harmonic$ApexPlayers, order=c(1,0,2), 
                           xreg=cbind(train.harmonic$Index, train.harmonic$sin2, train.harmonic$cos2, 
                                      train.harmonic$sin5, train.harmonic$cos5, train.harmonic$sin9, 
                                      train.harmonic$cos9, train.harmonic$sin4, train.harmonic$cos4, 
                                      train.harmonic$sin8, train.harmonic$cos8, train.harmonic$sin17, 
                                      train.harmonic$cos17, train.harmonic$sin3, train.harmonic$cos3, 
                                      train.harmonic$sin7, train.harmonic$cos7, train.harmonic$sin6, 
                                      train.harmonic$cos6, train.harmonic$sin10, train.harmonic$cos10, 
                                      train.harmonic$sin1, train.harmonic$cos1, train.harmonic$sin14, 
                                      train.harmonic$cos14, train.harmonic$sin114, train.harmonic$cos114, 
                                      train.harmonic$sin19, train.harmonic$cos19, train.harmonic$sin26, 
                                      train.harmonic$cos26, train.harmonic$sin22, train.harmonic$cos22, 
                                      train.harmonic$sin18, train.harmonic$cos18, train.harmonic$sin35, 
                                      train.harmonic$cos35, train.harmonic$sin11, train.harmonic$cos11, 
                                      train.harmonic$sin12, train.harmonic$cos12, train.harmonic$sin229, 
                                      train.harmonic$cos229, train.harmonic$sin115, train.harmonic$cos115, 
                                      train.harmonic$sin43, train.harmonic$cos43, train.harmonic$sin20, 
                                      train.harmonic$cos20, train.harmonic$sin36, train.harmonic$cos36,
                                      train.harmonic$sin23, train.harmonic$cos23, train.harmonic$sin45, 
                                      train.harmonic$cos45, train.harmonic$sin28, train.harmonic$cos28, 
                                      train.harmonic$sin69, train.harmonic$cos69, train.harmonic$sin228, 
                                      train.harmonic$cos228, train.harmonic$sin97, train.harmonic$cos97, 
                                      train.harmonic$sin21, train.harmonic$cos21, train.harmonic$sin24, 
                                      train.harmonic$cos24, train.harmonic$sin52, train.harmonic$cos52, 
                                      train.harmonic$sin54, train.harmonic$cos54, train.harmonic$sin132, 
                                      train.harmonic$cos132, train.harmonic$sin37, train.harmonic$cos37,
                                      train.harmonic$sin13, train.harmonic$cos13, train.harmonic$sin33, 
                                      train.harmonic$cos33, train.harmonic$sin51, train.harmonic$cos51, 
                                      train.harmonic$sin62, train.harmonic$cos62, train.harmonic$sin80, 
                                      train.harmonic$cos80, train.harmonic$sin106, train.harmonic$cos106))
arima_corrected3c.4

acf(arima_corrected3c.4$residuals, lag=20, main="Corrected ACF Residuals")
Box.test(arima_corrected3c.4$residuals, type="Ljung",lag=20)


plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="Corrected Harmonic")
lines(train$ApexPlayers - arima_corrected3a.4.1$residuals,col="red")
grid()


# 4c: ARCH/GARCH

# we need to correct the other models as our data cannot be reduced to white noise

acf(diff(diff(diff(diff(diff(train$ApexPlayers))))))
Box.test(diff(diff(diff(diff(diff(train$ApexPlayers))))), type="Ljung")

Box.test(diff(diff(diff(diff(train$ApexPlayers)))), type="Ljung")

Box.test(diff(diff(diff(train$ApexPlayers))), type="Ljung")

Box.test(diff(diff(train$ApexPlayers)), type="Ljung")

Box.test(diff(train$ApexPlayers), type="Ljung")

Box.test(train$ApexPlayers, type="Ljung")


acf(diff(train$ApexPlayers,10), main="ACF 10th difference")
Box.test(diff(train$ApexPlayers, 10), type="Ljung", lag=20)

# 4d: ARCH/GARCH corrections for Part 2, 3, 4a, 4b


# 2 and 3 do not get white noise results

# 4a 

# check if white noise squared residuals
acf(model4ap$residuals^2, main="ACF Squared Residuals")
pacf(model4ap$residuals^2, main="PACF Squared Residuals")
Box.test(model4ap$residuals^2, type="Ljung", lag=20)

# we should fit a garch SARIMA here but the garchFit function doesn't take seasonality
# so fit to residuals!
garch.fit3c4<-garchFit(~garch(1,1),data=model4ap$residuals, trace=FALSE)
summary(garch.fit3c4)

acf(garch.fit3c4@residuals/garch.fit3c4@sigma.t, main="Corrected ACF Residuals")
acf((garch.fit3c4@residuals/garch.fit3c4@sigma.t)^2, main="Corrected ACF Sq Residuals")

Box.test(garch.fit3c4@residuals/garch.fit3c4@sigma.t, type="Ljung", lag=20)
Box.test((garch.fit3c4@residuals/garch.fit3c4@sigma.t)^2, type="Ljung", lag=20)

# this model works! (except that it is not one model)


# 4b 

acf(arima_corrected3a.01.1$residuals^2, main="ACF Squared Residuals")
Box.test(arima_corrected3a.01.1$residuals^2, type="Ljung", lag=20)

# squared residuals are white noise so ARCH/GARCH not needed


# 3a 4c

acf(arima_corrected3a.4.1$residuals^2, main="ACF Squared Residuals")
pacf(arima_corrected3a.4.1$residuals^2, main="PACF Squared Residuals")
Box.test(arima_corrected3a.4.1$residuals^2, lag=20)

# squared resid not WN so need ARCH/GARCH!!!

garch.fit3a4<-garchFit(~garch(1,0),data=arima_corrected3a.4.1$residuals, trace=FALSE)
summary(garch.fit3a4)

acf(garch.fit3a4@residuals/garch.fit3a4@sigma.t, main="Corrected ACF Residuals")
acf((garch.fit3a4@residuals/garch.fit3a4@sigma.t)^2, main="Corrected ACF Sq Residuals")

Box.test(garch.fit3a4@residuals/garch.fit3a4@sigma.t, type="Ljung", lag=20)
Box.test((garch.fit3a4@residuals/garch.fit3a4@sigma.t)^2, type="Ljung", lag=20)



# model 3c.4 check if white noise squared residuals

acf(arima_corrected3c.4$residuals^2)
pacf(arima_corrected3c.4$residuals^2)

Box.test(arima_corrected3c.4$residuals^2, type="Ljung", lag=20)

garch.fit3c4<-garchFit(~garch(1,0),data=arima_corrected3c.4$residuals, trace=FALSE, cond.dist="std")

# singularity error


# REPORT PART 5: PREDICTIVE PERFORMANCE COMPARISON

test2=test[1:10,]
pred2.3 = predict(model2.3, test2)
mape2.3<- mean(abs(test2$ApexPlayers - pred2.3)/test2$ApexPlayers)

pred3a.4 = predict(model3a.4, test2)
mape3a.4<- mean(abs(test2$ApexPlayers - pred3a.4)/test2$ApexPlayers)

pred3a.01 = predict(model3a.01, test2)
mape3a.01<- mean(abs(test2$ApexPlayers - pred3a.01)/test2$ApexPlayers)

pred2.3 = predict(model2.3, test2)
mape2.3<- mean(abs(test2$ApexPlayers - pred2.3)/test2$ApexPlayers)

pred4a = predict(model4ap,n.ahead=10)$pred
mape4a<- mean(abs(test2$ApexPlayers - pred4a)/test2$ApexPlayers)

# could also do 4a incrementally

pred3b = predict(model3b, test2)
mape3b<- mean(abs(test2$ApexPlayers - pred3b)/test2$ApexPlayers)

test.harmonic2=test.harmonic[1:10,]

pred3c.4_42 = predict(model3c.4, test.harmonic)
mape3c.4_42<- mean(abs(test.harmonic$ApexPlayers - pred3c.4)/test.harmonic$ApexPlayers)

pred3c.4 = predict(model3c.4, test.harmonic2)
mape3c.4<- mean(abs(test.harmonic2$ApexPlayers - pred3c.4)/test.harmonic2$ApexPlayers)

pred.ac.3a01 = predict(arima_corrected3a.01.1, n.ahead=10, newxreg=cbind(test2$Weekday, test2$Month, test2$Season))$pred
mape.ac.3a01<- mean(abs(test2$ApexPlayers - pred.ac.3a01)/test2$ApexPlayers)

pred.ac.3c4_42 = predict(arima_corrected3c.4, n.ahead=42, newxreg=cbind(
  test.harmonic$Index, test.harmonic$sin2, test.harmonic$cos2, 
  test.harmonic$sin5, test.harmonic$cos5, test.harmonic$sin9, 
  test.harmonic$cos9, test.harmonic$sin4, test.harmonic$cos4, 
  test.harmonic$sin8, test.harmonic$cos8, test.harmonic$sin17, 
  test.harmonic$cos17, test.harmonic$sin3, test.harmonic$cos3, 
  test.harmonic$sin7, test.harmonic$cos7, test.harmonic$sin6, 
  test.harmonic$cos6, test.harmonic$sin10, test.harmonic$cos10, 
  test.harmonic$sin1, test.harmonic$cos1, test.harmonic$sin14, 
  test.harmonic$cos14, test.harmonic$sin114, test.harmonic$cos114, 
  test.harmonic$sin19, test.harmonic$cos19, test.harmonic$sin26, 
  test.harmonic$cos26, test.harmonic$sin22, test.harmonic$cos22, 
  test.harmonic$sin18, test.harmonic$cos18, test.harmonic$sin35, 
  test.harmonic$cos35, test.harmonic$sin11, test.harmonic$cos11, 
  test.harmonic$sin12, test.harmonic$cos12, test.harmonic$sin229, 
  test.harmonic$cos229, test.harmonic$sin115, test.harmonic$cos115, 
  test.harmonic$sin43, test.harmonic$cos43, test.harmonic$sin20, 
  test.harmonic$cos20, test.harmonic$sin36, test.harmonic$cos36,
  test.harmonic$sin23, test.harmonic$cos23, test.harmonic$sin45, 
  test.harmonic$cos45, test.harmonic$sin28, test.harmonic$cos28, 
  test.harmonic$sin69, test.harmonic$cos69, test.harmonic$sin228, 
  test.harmonic$cos228, test.harmonic$sin97, test.harmonic$cos97, 
  test.harmonic$sin21, test.harmonic$cos21, test.harmonic$sin24, 
  test.harmonic$cos24, test.harmonic$sin52, test.harmonic$cos52, 
  test.harmonic$sin54, test.harmonic$cos54, test.harmonic$sin132, 
  test.harmonic$cos132, test.harmonic$sin37, test.harmonic$cos37,
  test.harmonic$sin13, test.harmonic$cos13, test.harmonic$sin33, 
  test.harmonic$cos33, test.harmonic$sin51, test.harmonic$cos51, 
  test.harmonic$sin62, test.harmonic$cos62, test.harmonic$sin80, 
  test.harmonic$cos80, test.harmonic$sin106, test.harmonic$cos106))$pred
mape.ac.3c4_42<- mean(abs(test.harmonic$ApexPlayers - pred.ac.3c4_42)/test.harmonic$ApexPlayers)

pred.ac.3c4 = predict(arima_corrected3c.4, n.ahead=10, newxreg=cbind(
  test.harmonic2$Index, test.harmonic2$sin2, test.harmonic2$cos2, 
  test.harmonic2$sin5, test.harmonic2$cos5, test.harmonic2$sin9, 
  test.harmonic2$cos9, test.harmonic2$sin4, test.harmonic2$cos4, 
  test.harmonic2$sin8, test.harmonic2$cos8, test.harmonic2$sin17, 
  test.harmonic2$cos17, test.harmonic2$sin3, test.harmonic2$cos3, 
  test.harmonic2$sin7, test.harmonic2$cos7, test.harmonic2$sin6, 
  test.harmonic2$cos6, test.harmonic2$sin10, test.harmonic2$cos10, 
  test.harmonic2$sin1, test.harmonic2$cos1, test.harmonic2$sin14, 
  test.harmonic2$cos14, test.harmonic2$sin114, test.harmonic2$cos114, 
  test.harmonic2$sin19, test.harmonic2$cos19, test.harmonic2$sin26, 
  test.harmonic2$cos26, test.harmonic2$sin22, test.harmonic2$cos22, 
  test.harmonic2$sin18, test.harmonic2$cos18, test.harmonic2$sin35, 
  test.harmonic2$cos35, test.harmonic2$sin11, test.harmonic2$cos11, 
  test.harmonic2$sin12, test.harmonic2$cos12, test.harmonic2$sin229, 
  test.harmonic2$cos229, test.harmonic2$sin115, test.harmonic2$cos115, 
  test.harmonic2$sin43, test.harmonic2$cos43, test.harmonic2$sin20, 
  test.harmonic2$cos20, test.harmonic2$sin36, test.harmonic2$cos36,
  test.harmonic2$sin23, test.harmonic2$cos23, test.harmonic2$sin45, 
  test.harmonic2$cos45, test.harmonic2$sin28, test.harmonic2$cos28, 
  test.harmonic2$sin69, test.harmonic2$cos69, test.harmonic2$sin228, 
  test.harmonic2$cos228, test.harmonic2$sin97, test.harmonic2$cos97, 
  test.harmonic2$sin21, test.harmonic2$cos21, test.harmonic2$sin24, 
  test.harmonic2$cos24, test.harmonic2$sin52, test.harmonic2$cos52, 
  test.harmonic2$sin54, test.harmonic2$cos54, test.harmonic2$sin132, 
  test.harmonic2$cos132, test.harmonic2$sin37, test.harmonic2$cos37,
  test.harmonic2$sin13, test.harmonic2$cos13, test.harmonic2$sin33, 
  test.harmonic2$cos33, test.harmonic2$sin51, test.harmonic2$cos51, 
  test.harmonic2$sin62, test.harmonic2$cos62, test.harmonic2$sin80, 
  test.harmonic2$cos80, test.harmonic2$sin106, test.harmonic2$cos106))$pred
mape.ac.3c4<- mean(abs(test.harmonic2$ApexPlayers - pred.ac.3c4)/test.harmonic2$ApexPlayers)


pred.ac.3a4 = predict(arima_corrected3a.4.1, n.ahead=10, 
        newxreg=cbind(test2$ApexTwitchViewers, test2$PUBGPlayers, test2$CSGOPlayers,
                      test2$Weekday, test2$Month, test2$Season, test2$FirstDayOfSeason,
                      test2$RankedSplitStart, test2$ALGSEvent, test2$InGameEvent))$pred
mape.ac.3a4<- mean(abs(test2$ApexPlayers - pred.ac.3a4)/test2$ApexPlayers)


# REPORT PART 6: MULTIVARIATE TIME SERIES MODELS

# Transfer Function
# Pre-whitening of the input process for the Transfer Function

plot.ts(data.frame(train$ApexPlayers, train$ApexTwitchViewers), 
        plot.type = "single", col=c(1:2), ylab="",
        main="Apex players (black) and Apex Twicth viewers(red)")
grid()

model6tf1 <- lm(ApexTwitchViewers~Weekday+Month+Season, data = train)
summary(model6tf1)
acf(train$ApexTwitchViewers)

# investigate if it became stationary
sres6tf1<-diff(model6tf1$residuals)
acf(sres6tf1)
pacf(sres6tf1)
Box.test(sres6tf1, lag=20, type = 'Ljung-Box')
# We managed to successfully make it stationary, now we 
# need to use ARIMA to pre-whiten it as well.

model6tf2 <- arima(model6tf1$residuals, order = c(1, 1, 1))
model6tf2

acf(model6tf2$residuals)
pacf(model6tf2$residuals)
Box.test(model6tf2$residuals, lag=30, type = 'Ljung-Box')
# Great! We've got white noise! Now let's combine them.

arima_corrected6tf2 <- arima(train$ApexTwitchViewers, order=c(1,1,1),
                             xreg=cbind(train$Weekday, train$Month, train$Season))

acf(arima_corrected6tf2$residuals)
pacf(arima_corrected6tf2$residuals)
Box.test(arima_corrected6tf2$residuals, lag=20, type = 'Ljung-Box')
# Now our Xt is pre-whitened.

# We also need to make yt stationary, not necessarily whit noise though

model6ft3 <- lm(ApexPlayers~ Season + Weekday + Month, data=train)
acf(model6ft3$residuals)
pacf(model6ft3$residuals)
acf(diff(model6ft3$residuals))

# It is stationary now. Now we can use pre-whitened-x to estimate a model

x2=arima_corrected6tf2$residuals
y=diff(model6ft3$residuals)

# ccf(x2,y)
ccf(y,x2)

# It looks like an MA(2), so we go for b=0, r=0, s=2

# Running a Transfer Function model using a stationary-x and a 
# stationary-y, not the whitened-x

# x=diff(model6tf1$residuals)
x=diff(train$ApexTwitchViewers)

acf(x, lag=50)
Y=y-mean(y)
X=x-mean(x)
m<-arimax(Y, order=c(1,0,0), fixed=c(0,NA,NA), xtransf=X, transfer=list(c(1,0)), include.mean = FALSE)
m

acf(m$residuals)
pacf(m$residuals)
#Corrected TF Model
m<-arimax(Y, order=c(0,0,2),xtransf=X, transfer=list(c(1,0)), include.mean = FALSE)
m

acf(m$residuals)
pacf(m$residuals)


Box.test(m$residuals, lag=20, type = 'Ljung-Box')

# par(mfrow=c(1,1))
plot.ts(y, ylab="Y")
lines(Y-m$residuals, col="purple")

# plot
plot.ts(train$ApexPlayers, col="darkgray", ylab="Players", main="")
lines(predict(m),col="purple4")
grid()

mapetf1_player<- mean(abs(test$ApexPlayers[1:10] - forecast$pred[,1])/test$ApexPlayers[1:10])

mapetf2_Twitch<- mean(abs(test$ApexTwitchViewers[1:10] - forecast$pred[,2])/test$ApexTwitchViewers[1:10])

###############################################

# VARMA

acf(train$ApexPlayers)
y1_varma <- as.numeric(y)

acf(train$ApexTwitchViewers, lag=30)
acf(diff(train$ApexTwitchViewers), lag=50)

y2_varma <- as.numeric(diff(train$ApexTwitchViewers))

# I didn't use the stationary-twitch-viewers I used for TF, 
# because if I used this one there, the ARIMAX function didn't work.

# Now that our y1 and y2 are stationary, we can run a VARMA.

ccf(y1_varma,y2_varma)

difdata <- cbind(y1_varma, y2_varma)

varma01 <- VARMA(difdata, p=0, q=1, include.mean = FALSE)
varma10 <- VARMA(difdata, p=1, q=0, include.mean = FALSE)
varma11 <- VARMA(difdata, p=1, q=1, include.mean = FALSE)


VARorder(difdata)
VMAorder(difdata)

# By running the above VARorder and VMAorder, it says we should run a
# VARMA(9,0) or VARMA(9,1), but running them would even give us a 
# worse AIC, so we would just go with VARMA(1,1) which gives us 
# white noise and has  the lowest AIC, although it produces NaN for
# some of the standard errors.

acf(varma01$residuals[,1])
acf(varma01$residuals[,2])

acf(varma10$residuals[,1])
acf(varma10$residuals[,2])

acf(varma11$residuals[,1])
acf(varma11$residuals[,2])

varma01$aic
varma10$aic
varma11$aic

# VARMA11 has the lowest AIC and results in white noise.

Box.test(varma11$residuals[,1], type="Ljung-Box", lag=20)
Box.test(varma11$residuals[,2], type="Ljung-Box", lag=20)

# Residuals for both variables are white noise.

# Model fit
plot.ts(y1_varma, ylab='Apex players')
lines(y1_varma-varma11$residuals[,1],col="lightblue")

plot.ts(y2_varma, ylab='Apex Twitch Viewers')
lines(y2_varma-varma11$residuals[,2],col="lightblue")

# Prediction for test sample
forecast<-VARMApred(varma11,h=10)

# plot.ts(test$ApexPlayers[1:10])
# lines(forecast$pred[,1], col="red")
# 
# plot.ts(test$ApexTwitchViewers[1:10])
# lines(forecast$pred[,2], col="red")


mape11_player<- mean(abs(test$ApexPlayers[1:10] - forecast$pred[,1])/test$ApexPlayers[1:10])

mape11_Twitch<- mean(abs(test$ApexTwitchViewers[1:10] - forecast$pred[,2])/test$ApexTwitchViewers[1:10])

# REPORT PART 7: CONCLUSION
# wrote in the report

