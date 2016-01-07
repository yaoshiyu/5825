##############################################################
### R Commands for Topic 9: Regression with ARMA Errors
### Ref: Shumway and Stoffer 2011
##############################################################

library(astsa)

### 

data(cmort)
data(tempr)
data(part)

cmort=ts(cmort)
tempr=ts(tempr)
part=ts(part)

dim(cmort)
dim(tempr)
dim(part)

par(mfrow=c(2,2))
ts.plot(cmort,main="CMORT")          # Time Series Plot
ts.plot(tempr,main="TEMPR")
ts.plot(part,main="PART")
all=cbind(cmort,tempr,part)
ts.plot(all,main="CMORT, TEMPR, PART",lty=1:3)


###########################################################

windows()    
### pairwise MV scatterplot 
pairs(cbind(cmort,tempr,part))
###########################################################

### S&S Example 2.2: A Time Series Regression Model

### Note: Model is 
### Cmort_t = beta0 + beta1*t + beta2*temp_t + beta3*temp^2_t
###   + beta4*part_t + x_t
### where we assume x_t are WN(0, sigma^2_x)  
### (x_t is like the w_t we have been using, but with 
### possibly more stochastic structure than  White Noise!)

temp=tempr-mean(tempr)   # center tempr data
temp2=temp^2             # form a new var temp^2
trend=time(cmort)        # OR trend=c(1:length(cmort))


### Fit an MLR model
regfit = lm(cmort ~ trend +temp+temp2 +part,na.action=NULL)
summary(regfit)
summary(aov(regfit))
n=length(cmort)
AIC(regfit)/n -log(2*pi)                           #AIC
AIC(regfit, k=log(n))/n -log(2*pi)                 # BIC
AICc = log(sum(resid(regfit)^2)/n) +(n+5)/(n-5-2)  # AICc
AICc

#############################################

### S&S Example 5.7: Regression Model with ARMA Errors
### Look at ACF and PACF plots of residuals from regfit

resid=resid(regfit)   # residuals from regression fit
length(resid)

### Plots of the residuals 
par(mfrow=c(2,2))
acf(resid,main="ACF of Regression Resids")
pacf(resid, main="PACF of Regression Resids")

### These suggest an AR(2) would be a reasonable fit to 
### resid. So, we now fit the model
### Cmort_t = beta0 + beta1*t + beta2*temp_t + beta3*temp^2_t
###    + beta4*part_t + x_t
### where we assume x_t are from an AR(2) (i.e., ARIMA(2,0,0)) process.
### NOTE: use of the arima or sarima function is OK here, since we would
### assume the errors x_t have zero mean anyways (and beta_0 
### will take care of estimating the level in cmort)

### Fit an ARIMA model to the regression residuals. Si
regarmafit = arima(cmort, order=c(2,0,0),xreg=cbind(trend,temp,temp2,part))
regarmafit


### Now check model adequacy etc. for this model 
resid2=resid(regarmafit)
length(resid2)

### verify these residuals are WN
acf(resid2,main="ACF of RegAR2 Resids")
pacf(resid2, main="PACF of RegAR2 Resids")
Q=Box.test(resid2,12,type="Ljung")$statistic  # Ljung Box statistic
Q
pchisq(as.numeric(Q),df=(12-2),lower=FALSE)    # obs p-value
### NOTE: Coefficient of Trend no longer SIG.

###  Can refit a model without Trend and compare to previous model(s)
reg3fit = arima(cmort, order=c(2,0,0),xreg=cbind(temp,temp2,part))
reg3fit

### Now check model adequacy etc. for this model 
resid3=resid(reg3fit)
length(resid3)
### verify these residuals are WN
acf(resid3,main="ACF of Reg3fit Resids")
pacf(resid3, main="PACF of Reg3fit Resids")
Q=Box.test(resid3,12,type="Ljung")$statistic  # Ljung Box statistic
Q
pchisq(as.numeric(Q),df=(12-2),lower=FALSE)    # obs p-value

##################################################################################################








   



