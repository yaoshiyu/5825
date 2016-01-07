##############################################################
### R Commands for Topic 8&9: Fit and Forecast ARIMA Models
### Ref: Shumway and Stoffer 2011
##############################################################

library(astsa)

##############################################################
### Example: Quarterly U.S. GNP from 1947(1) to 2002(3).
### See Shumway and Stoffer, 2011. 
##############################################################

data(gnp)  # it is of class "ts"
length(gnp)   # [1] 223

windows()  # open a new window for plotting
par(mfrow=c(2,2))   # make a 2 by 2 grid
ts.plot(gnp,main="Quarterly US GNP")
acf(gnp)   # autocorrelation function
pacf(gnp)  # partial autocorrelation function

##############################################################
### Work with Growth rate: (1-B) log(gnp)
##############################################################

gnpgr =diff(log(gnp))
length(gnpgr)
windows()     
par(mfrow=c(2,2))
ts.plot(gnp,main="Quarterly US GNP")
ts.plot(gnpgr,main="Growth Rate of Quarterly US GNP")
acf(gnpgr)   # autocorrelation function
pacf(gnpgr)  # partial autocorrelation function


##############################################################
### Model Identification: Sample ACF and PACF Plots - use patterns
### for different processes - AR(1) or MA(2).
##############################################################

### Split the time series into calibration portion and holdout portion
### Calibration data: gnpgr[1:216]
### Holdout data: gnpgr[217:222]

gnpgr.all=gnpgr
gnpgr.calib=gnpgr.all[1:216]
gnpgr.hold=gnpgr.all[217:222]



##############################################################
### AR(1) Model Fit and Forecast using Yule-Walker estimation  
##############################################################

gnpgr.yw=ar.yw(gnpgr.calib,order=1)

### YW estimate of Mean of the time series
gnpgr.yw$x.mean    

### YW Estimate of phi1 (AR parameter)
gnpgr.yw$ar

### S.E. of YW estimates of phi1 
sqrt(diag(gnpgr.yw$asy.var.coef))

### Error variance estimate
gnpgr.yw$var.pred

### Forecast 6 months ahead
gnpgr.pr = predict(gnpgr.yw, n.ahead=6)
gnpgr.pr$pred
gnpgr.pr$se

windows()
### 1-sig Prediction Limits
#U = gnpgr.pr$pred + gnpgr.pr$se
#L = gnpgr.pr$pred - gnpgr.pr$se
U = gnpgr.pr$pred + gnpgr.pr$se
L = gnpgr.pr$pred - gnpgr.pr$se
qtr=217:222
plot(qtr,gnpgr.hold,type="o",ylab="gnpgr",ylim=c(-0.030,0.030),main="AR(1) YW. black:data,red:preds,blue:limits")
lines(gnpgr.pr$pred,col="red",type="o")
lines(U,col="blue",lty="dashed")
lines(L,col="blue",lty="dashed")



##############################################################
### AR(1) Model Fit and Forecast using Least Squares (OLS) estimation  
##############################################################

gnpgr.ols = ar.ols(gnpgr.calib,order=1,demean=T,intercept=F)
gnpgr.ols

### OLS estimate of Time Series Mean 
gnpgr.ols$x.mean

### OLS estimate of AR parameter phi1 
gnpgr.ols$ar

### Forecast 6 months ahead using OLS estimate
gnpgr.ols.pr = predict(gnpgr.ols, n.ahead=6)
gnpgr.ols.pr
gnpgr.ols.pr$pred
gnpgr.ols.pr$se

### Prediction Limits based on OLS estimates
U.ols = gnpgr.ols.pr$pred + gnpgr.ols.pr$se
L.ols = gnpgr.ols.pr$pred - gnpgr.ols.pr$se
plot(qtr,gnpgr.hold,type="o",ylab="gnpgr",ylim=c(-0.030,0.030),main="AR(1) OLS. black:data,red:preds,blue:limits")
lines(gnpgr.ols.pr$pred,col="red",type="o")
lines(U.ols,col="blue",lty="dashed")
lines(L.ols,col="blue",lty="dashed")




##############################################################
### AR(1) Model Fit and Forecast using MLE 
##############################################################

gnpgr.mle=ar.mle(gnpgr.calib,order=1)

### ML estimate of the mean of the time series
gnpgr.mle$x.mean
### ML estimate of AR parameter 
gnpgr.mle$ar
### ML estimate of standard error of AR coeff
sqrt(diag(gnpgr.mle$asy.var.coef))
### ML estimate of error variance
gnpgr.mle$var.pred

### Forecast 6 months ahead using ML estimate
gnpgr.mle.pr = predict(gnpgr.mle, n.ahead=6)
gnpgr.mle.pr
gnpgr.mle.pr$pred
gnpgr.mle.pr$se

### Prediction Limits based on ML estimates
U.mle = gnpgr.mle.pr$pred + gnpgr.mle.pr$se
L.mle = gnpgr.mle.pr$pred - gnpgr.mle.pr$se
plot(qtr,gnpgr.hold,type="o",ylab="gnpgr",ylim=c(-0.030,0.030),main="AR(1) MLE. black:data,red:preds,blue:limits")
lines(gnpgr.mle.pr$pred,col="red",type="o")
lines(U.mle,col="blue",lty="dashed")
lines(L.mle,col="blue",lty="dashed")




##############################################################
### AR(1) Model Fit using sarima function from astsa  
##############################################################

### Fit an AR(1) model to gnpgr.calib using the sarima function in the
### astsa package. Unless the model involves a mean in a differenced 
### context, sarima should behave similar to the arima function in R.
  
gnpgr.ar1= sarima(gnpgr.calib,1,0,0)   # MLEs from an AR(1)=ARIMA(1,0,0) model 
gnpgr.ar1  # write out the fitted model


### Verify condittion on roots of AR polynomial
ar1est=gnpgr.ar1$fit$coef[[1]]

u<-polyroot(c(1,-ar1est))
u
# Modulus > 1; stationary process
Mod(u)


### psi weights
ARMAtoMA(ar=c(gnpgr.ar1$fit$coef[[1]]),ma=0,lag.max=25)


### Recover the residuals
gnpgr.ar1.resid=resid(gnpgr.ar1$fit)    
length(gnpgr.ar1.resid)    # [1] 216

### Residual Diagnostics - Plots and Statistics
windows()     
par(mfrow=c(3,3))
ts.plot(gnpgr.ar1.resid,main="Residuals from AR(1) fit" )
hist(gnpgr.ar1.resid,main="histogram",xlab="resid")
qqnorm(gnpgr.ar1.resid,main="Normal Q-Q plot",xlab="resid")
acf(gnpgr.ar1.resid,48)
pacf(gnpgr.ar1.resid,48) 
#Shapiro-Wilk test for normality
shapiro.test(gnpgr.ar1$resid$resid)   


### Ljung-Box Portmanteau test for Model Adequacy
lbox=Box.test(gnpgr.ar1.resid, lag = 48,type="Ljung")
# You can look at different portions of lbox
lbox$statistic
lbox$parameter
lbox$p.value

### Box-Pierce Statistic for Model Adequacy.
### Older versiion of Ljung-Box.
Box.test (gnpgr.ar1.resid, lag = 48)

###  McLeod-Li Portmanteau statistic for Model Adequacy
### is the Ljung-Box statistic for squared residuals 
r2=gnpgr.ar1.resid^2
bl2=Box.test (r2, lag = 48,type="Ljung") 
bl2

##########################################################        
### Model Selection Criteria based on calibration data
##########################################################  
### Minimum value of these criteria gives best model 
gnpgr.ar1$AIC     # AIC Akaike Information Criterion
gnpgr.ar1$AICc  # AICc or Corrected AIC
gnpgr.ar1$BIC   # BIC Bayesian Information Criterion




##########################################################        
### Forecast Holdout Observations 6 months ahead
##########################################################

n.ahead=6
gnpgr.ar1.fore=sarima.for(gnpgr.calib,n.ahead=6,1,0,0)
gnpgr.ar1.fore


##########################################################        
### Forecast Evaluation Criteria based on Holdout Prediction
##########################################################

err.ar1=gnpgr.hold-gnpgr.ar1.fore$pred
err.ar1
me.ar1=mean(err.ar1)
mpe.ar1=100*(mean(err.ar1/gnpgr.hold))
mse.ar1=sum(err.ar1**2)/length(err.ar1)
mae.ar1=mean(abs(err.ar1))
mape.ar1=100*(mean(abs((err.ar1)/gnpgr.hold)))
me.ar1
mpe.ar1
mse.ar1
mae.ar1
mape.ar1






##############################################################
### MA(2) Model Fit
##############################################################

### Fit an MA(2) model to gnpgr.calib using the sarima function in the
### astsa package. Unless the model involves a mean in a differenced 
### context, sarima should behave similar to the arima function in R.
  
gnpgr.ma2= sarima(gnpgr.calib,0,0,2)   # MLEs 
gnpgr.ma2  # write out the fitted model


### Verify condition on roots of MA polynomial
ma2est.1=gnpgr.ma2$fit$coef[[1]]
ma2est.2=gnpgr.ma2$fit$coef[[2]]

u<-polyroot(c(1,-ma2est.1,-ma2est.2))
u  #complex vector of length q=2
# Modulus > 1; invertible  process
Mod(u)


### Recover the residuals
gnpgr.ma2.resid=resid(gnpgr.ma2$fit)    
length(gnpgr.ma2.resid)    # [1] 216

### Residual Diagnostics - Plots and Statistics
windows()     
par(mfrow=c(3,3))
ts.plot(gnpgr.ma2.resid,main="Residuals from MA(2) fit" )
hist(gnpgr.ma2.resid,main="histogram",xlab="resid")
qqnorm(gnpgr.ma2.resid,main="Normal Q-Q plot",xlab="resid")
acf(gnpgr.ma2.resid,48)
pacf(gnpgr.ma2.resid,48)  


### Ljung-Box Portmanteau test for Model Adequacy
lbox.ma2=Box.test(gnpgr.ma2.resid, lag = 48,type="Ljung")
lbox.ma2

###  McLeod-Li Portmanteau statistic for Model Adequacy
### is the Ljung-Box statistic for squared residuals 
r2=gnpgr.ma2.resid^2
bl2.ma2=Box.test (r2, lag = 48,type="Ljung") 
bl2.ma2

##########################################################        
### Model Selection Criteria based on calibration data
##########################################################  
### Minimum value of these criteria gives best model 
gnpgr.ma2$AIC     # AIC Akaike Information Criterion
gnpgr.ma2$AICc  # AICc or Corrected AIC
gnpgr.ma2$BIC   # BIC Bayesian Information Criterion


##########################################################        
### Forecast Holdout Observations
##########################################################

n.ahead=6
gnpgr.ma2.fore=sarima.for(gnpgr.calib,n.ahead=6,0,0,2)
gnpgr.ma2.fore

##########################################################        
### Forecast Evaluation Criteria based on Holdout Prediction
##########################################################

err.ma2=gnpgr.hold-gnpgr.ma2.fore$pred
err.ma2
me.ma2=mean(err.ma2)
mpe.ma2=100*(mean(err.ma2/gnpgr.hold))
mse.ma2=sum(err.ma2**2)/length(err.ma2)
mae.ma2=mean(abs(err.ma2))
mape.ma2=100*(mean(abs((err.ma2)/gnpgr.hold)))
me.ma2
mpe.ma2
mse.ma2
mae.ma2
mape.ma2





##########################################################
### Automatic Fitting of ARIMA models 
##########################################################
### BIC is written out for all fitted models. Choose the
### model(s) with lowest BIC, and focus on those for a closer look.
### You can also compute the MAPE within the loop and write out
### BOTH the BIC and MAPE, and select models.

d=0   # no differencing needed
np=3    #maximum AR order
nq=3    # maximum MA order
# Set up loops to run several possible models
for (p in 0:np){
   for (q in 0:nq) {
     gnpgr.fit = sarima(gnpgr.calib,p,d,q)
      outarima <- c(p,q,gnpgr.fit$BIC)
         write(outarima,file="C://Users//nra02001//Dropbox//ST45825//outarima",append=TRUE)
          }
 }
outarima.all <- read.table("C://Users//nra02001//Dropbox//ST45825//outarima")
colnames(outarima.all) = c("p", "q", "BIC")
outarima.all


### End of Code
##############################################################






