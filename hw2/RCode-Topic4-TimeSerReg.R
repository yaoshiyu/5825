##############################################################
### R Commands for Topic 4: Time Series Regression
### Ref. Shumway and Stoffer 2011
##############################################################

library(astsa)

##############################################################
### Global mean land-ocean temperature deviations 
### (from 1951-1980 average), measured in degrees centigrade, 
### for the years 1880-2009. 

data(gtemp)
help(gtemp)

gtemp=ts(gtemp)         # save as a time series object
length(gtemp)
ts.plot(gtemp)          # Time Series Plot

###############################################################

### Linear Trend Model fit to gtemp from 1900:1997 (n=98)

x=gtemp[11:108]  # select a portion of the time series
length(x)
t=c(1:98)        # set up a time vector as indep var
### OR use   
### t=time(x)


fit1=lm(x~t)     # linear time trend regression model
summary(fit1)
aov(fit1)        # ANOVA table for regression fit
anova(fit1)      # or use anova function

plot(t,x,type="o",xlab="Time (year)",ylab="Temp deviations",main="Global Temperatures and Fit 1")
abline(fit1)


### residual diagnostics for fit
par(mfrow = c(2, 2))      # plot 4 figures, 2 in each of 2 rows
plot(fit1, main="Fit 1 Diagnostics",which = 1:4)

##############################################################################

### Quadratic Trend Model fit and Extra Sum of Squares *SS) F test
### Extra SS test must be done by hand using output from linear and quadratic trend model fits

tsq=t^2/factorial(2)     # set up quandratic indep var
length(tsq)              # check its length (all indep vars must have same length as response)  

fit2=lm(x~t+tsq)          # time trend regression model with linear and quadratic time terms
summary(fit2)
anova(fit2)
##############################################################################

### Model Selection Criteria

AIC(fit1)/length(x)     # Akaike Information Criterion
### OR 
AIC(logLik(fit1))/length(x)

BIC(logLik(fit1))/length(x)  # Bayesian Information Criterion

AIC(logLik(fit2))/length(x)
BIC(logLik(fit2))/length(x)  


##############################################################################

### Forecast Evaluation for Linear trend and Quadratic trend models
### Fit linear trend model to portion of data from 1900-1987  (n=88)
### This is the Calibration or Training portion of the data

xs=gtemp[11:98]
length(xs)
ts=1900:1987    # set up a time vector as indep var
length(ts)
### OR
### ts=time(xs)


### Set up and predict for next 10 times in the future.
### That is, predict the forecast validation portion of data
### from 1988 to 1997

new <- data.frame(ts=c(1988:1997))
pn=predict(lm(xs ~ ts), new, se.fit = TRUE)
pn$fit

### Observed data for 1988-1997
xn=gtemp[99:108]
xn

### Prediction errors: Observed - Predicted
en=xn-pn$fit
en
 
### Forecast evaluation criteria
me=mean(en)                    # Mean Error
mpe=100*(mean(en/xn))          # Mean Percent Error
mse=sum(en**2)/length(en)      # Mean Squared Error
mae=mean(abs(en))              # Mean Absolute Error
mape=100*(mean(abs((en)/xn)))  # Mean Absolute Percent Error 
me
mpe
mse
mae
mape

##############################################################################

### Quadratic trend model with t and t^2/2! as independent variables
### fit to calibration portion

tsq=ts**2/factorial(2)
tsn=c(1988:1997)
mat=matrix(c(tsn=c(1988:1997),tsn**2/factorial(2)),nrow=10,ncol=2,dimnames = list(c(),c("ts", "tsq")))
mat

newn <- data.frame(mat)

pn2=predict(lm(xs ~ ts+tsq), newn, se.fit = TRUE)
pn2$fit
pn2$se.fit
pn2$df
pn2$residual.scale


en2=xn-pn2$fit
en2
me2=mean(en2)
mpe2=100*(mean(en2/xn))
mse2=sum(en2**2)/length(en2)
mae2=mean(abs(en2))
mape2=100*(mean(abs((en2)/xn)))
me2
mpe2
mse2
mae2
mape2

##############################################################################
### Detrending and Differencing Time Series

par(mfrow = c(2, 1))

### Detrended series
plot(t,fit1$resid,type="o",xlab="Time (year)",ylab="Detrended Temp deviations")

### Differenced (order 1) series
x1=gtemp[10:108] # 1899-1997; length = 99
difx=diff(x1)
plot(t,difx,type="o",xlab="Time (year)",ylab="Differenced Temp deviations")
##############################################################################

### Moving Average for estimating Trend in Data - smoothes data
### Use simulated White Noise Process

w=rnorm(500,0,1)
v=filter(w,sides=2,rep(1,3)/3)
par(mfrow=c(2,1))
plot.ts(w)
plot.ts(v)
par(mfrow=c(2,1))
plot.ts(w,xlab="t",ylab="White Noise")
plot.ts(v,xlab="t",ylab="Moving Average")
##############################################################################

### Moving Average of Global Temperatures
par(mfrow=c(2,2))

### original data
plot.ts(x,xlab="time",ylab="Temp Deviations")

### 3 point MA
x_movavg_3=filter(x,sides=2,rep(1,3)/3)
plot.ts(x_movavg_3,xlab="time",ylab="3 point Moving Average")

### 5 point MA
x_movavg_5=filter(x,sides=2,rep(1,5)/5)
plot.ts(x_movavg_5,xlab="time",ylab="5 point Moving Average")

### 9 point MA
x_movavg_9=filter(x,sides=2,rep(1,9)/9)
plot.ts(x_movavg_9,xlab="time",ylab="9 point Moving Average")

##############################################################################

### Accidental Deaths data

accd=read.table("C://Users//nra02001//Dropbox//ST45825//Data//accdeaths.csv")
accd=ts(accd)
length(accd)

plot(accd, xlab="Time",ylab="Deaths",main="Accidental Deaths", type="o", col="blue", lty="dashed")

### Fit a regression with trigonometric indicators to accd data

t=1:length(accd)
per=12

### Model 1
c1=cos(2*pi*t/per)
s1=sin(2*pi*t/per)
accd_trig1=lm(accd~c1+s1)
summary(accd_trig1)

### Model 2:
c2=cos(2*pi*2*t/per)
s2=sin(2*pi*t*2/per)
accd_trig2=lm(accd~c1+s1+c2+s2)
summary(accd_trig2)

### Plot data and fits from two models
plot(t,accd,type="o",xlab="Time",ylab="deaths",main="Accidental deaths: Trig functions Fit")
lines(accd_trig1$fit,col="blue", lty="dashed")
lines(accd_trig2$fit,col="red", lty="dotted")

### Polynomial trend and trig functions as indep vars
t=1:length(accd)
per=12

t2=t^2
t3=t^3

c1=cos(2*pi*t/per)
s1=sin(2*pi*t/per)
c2=cos(2*pi*2*t/per)
s2=sin(2*pi*t*2/per)

accd_poltrig=lm(accd~t+t2+t3+c1+c2+s1+s2)
summary(accd_poltrig)
##############################################################################

### Fit a regression with dummy (monthly) indicators to accd data
### set up a categorical variable to indicate monthly seasonal 
### dummy variables for regression

sets=length(accd)/per
month=factor(rep(1:per,sets))
### regression model without intercept, and all 12 seasonal 
### dummy variables
accd_dumvar=lm(accd~t+t2+t3+month -1) 
summary(accd_dumvar)


### End of Code
##############################################################################