##############################################################
### R Commands for Topic 8&9: Fit and Forecast ARIMA Models
### Ref: Shumway and Stoffer 2011
##############################################################

library(astsa)

##############################################################



#############################################################
### R Commands for Fitting and Forecasting with AR models
#############################################################

### Example: Recruitment Data:453 months from 1950-1987. 
### See S&S Example 1.5

data(rec)
rec=ts(rec)

### Model Identification - Sample ACF and PACF plots 
par(mfrow=c(3,1))
ts.plot(rec,main="Recruitment Data; n=453")
acf(rec,48,main="Sample ACF")
pacf(rec,48,main="Sample PACF")

### See that AR(2) model seems a reasonable choice

### Model Estimation: Yule-Walker (YW) estimation and Forecasting
rec.yw=ar.yw(rec,order=2)

### YW estimate of Mean of the time series
rec.yw$x.mean    

### YW Estimates of phi1 and phi2 (AR parameters)
rec.yw$ar

### S.E. of YW estimates of phi1 and phi2
sqrt(diag(rec.yw$asy.var.coef))

### Error variance estimate
rec.yw$var.pred

### Forecast 24 months ahead}
rec.pr = predict(rec.yw, n.ahead=24)
rec.pr$pred
rec.pr$se

### Prediction Limits
### This is what S&S do.
### You can instead use 95% limits as
### U = rec.pr$pred + 1.96*rec.pr$se
### L = rec.pr$pred - 1.96*rec.pr$se
U = rec.pr$pred + rec.pr$se
L = rec.pr$pred - rec.pr$se
month=360:453
plot(month,rec[month],type="o",xlim=c(360,480),ylab="recruits")
lines(rec.pr$pred,col="red",type="o")
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")


### Model Estimation: Least Squares Estimation and Forecasting

fitlse = ar.ols(rec,order=2,demean=T,intercept=F)
fitlse

### Order selected is p=2;  sigma^2 estimated as  89.72 
### OLS estimate of Time Series Mean 
fitlse$x.mean

### OLS estimate of AR parameters phi1 and phi2
fitlse$ar

### Forecast 24 months ahead using OLS estimates
rec.pr.lse = predict(fitlse, n.ahead=24)
rec.pr.lse 
rec.pr.lse$pred
rec.pr.lse$se
### Prediction Limits based on OLS estimates
U.lse = rec.pr.lse$pred + rec.pr.lse$se
L.lse = rec.pr.lse$pred - rec.pr.lse$se
plot(month,rec[month],type="o",xlim=c(360,480),ylab="recruits")
lines(rec.pr.lse$pred,col="red",type="o")
lines(U.lse,col="blue",lty="dashed")
lines(L.lse,col="blue",lty="dashed")

### Model Estimation: MLE and Forecasting

rec.mle=ar.mle(rec,order=2)

### ML estimate of the mean of the time series
rec.mle$x.mean
### ML estimate of AR parameters 
rec.mle$ar
### ML estimate of standard errors of AR coeffs
sqrt(diag(rec.mle$asy.var.coef))
### ML estimate of error variance
rec.mle$var.pred


### Alternate R command for MLE of ARIMA process:
recmle=arima(rec,order=c(2,0,0))
  
### compute the psi weights (MA(infty) representation) 
### for this AR(2) process
### using as inputs the estimated AR coeffs
ARMAtoMA(ar=rec.mle$ar,ma=0,10) 

### Residual Diagnostics
length(recmle$resid)

par(mfrow=c(3,3))
ts.plot(rec)
ts.plot(recmle$resid)
hist(recmle$resid,main="Recruit Data:AR(2) Resids",sub="histogram",xlab="resid")
qqnorm(recmle$resid,main="Recruit Data:AR(2) Resids",sub="qqplot",xlab="resid")
acf(recmle$resid,48)
pacf(recmle$resid,48)

###Shapiro-Wilk test for normality
shapiro.test(recmle$resid)  

### Ljung-Box Portmanteau test for Model Adequacy
tsdiag(recmle,gof.lag=48) 

### Alternate way to compute Box-Ljung Statistic

Box.test (recmle$resid, lag = 48,type="Ljung")
     
### call different portions of this output
bl=Box.test (recmle$resid, lag = 48,type="Ljung")
bl$statistic
bl$parameter
bl$p.value

### Box-Pierce Statistic for Model Adequacy
Box.test (recmle$resid, lag = 48)
        
### McLeod-Li Portmanteau statistic for Model Adequacy
### square the ML residuals 
r2=recmle$resid^2
length(r2)

### McLeod-Li portmanteau statistic is
### Box-Ljung statistic for squared resids
bl2=Box.test (r2, lag = 48,type="Ljung") 
bl2
        
### Model Selection
AIC(recmle)/length(rec)      # AIC

### or alternately use
recmle$aic/length(rec)      # alternate formula for AIC

AIC(recmle, k=log(length(rec)))/length(rec)   # BIC


### End of Code
##########################################################




