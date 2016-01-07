##############################################################
### R Commands for Topic 10: Seasonal ARIMA Models
### Ref: Shumway and Stoffer 2011
##############################################################

library(astsa)
##############################################################

### Seasonal ARIMA(p,d,q)x(P,D,Q)s fitting - using sarima funtion in R

### Example: FRB Data:372 months from 1948-1978. 
### See S&S Example 3.46 
#######################################################

data(prodn)
frbprod=ts(prodn)

#######################################################
### Model Identification
### Sample ACF and PACF plots of raw data 
par(mfrow=c(3,1))
ts.plot(frbprod,main="FRB Data; n=372")
acf(frbprod,60,main="Sample ACF")   
pacf(frbprod,60,main="Sample PACF")

### Nonseasonal Differencing, and check ACF, PACF of
### differenced data (1-B)frbprod
par(mfrow=c(2,2))
frbprod_dif1=diff(frbprod)
ts.plot(frbprod_dif1,main="First Diff: FRB Production Index")
acf(frbprod_dif1,60)
pacf(frbprod_dif1,60)

### And seasonal Differencing, and check ACF, PACF of
### the differenced data (1-B^12)(1-B)frbprod
frbprod_dif1dif12=diff(frbprod_dif1,12)
par(mfrow=c(2,2))
ts.plot(frbprod_dif1dif12,main="(1-B)(1-B^12) of FRB Production Index")
acf(frbprod_dif1dif12,60)
pacf(frbprod_dif1dif12,60)

#######################################################
### Model Estimation and Model Adequacy
### Note:
### If d > 0, and/or D > 0, and 
### a)if the differenced data may be assumed to have zero mean: 
###   can use arima function to estimate and forecast; but
### b)if the differenced data may be assumed to have nonzero mean: 
###   use sarima function to estimate and forecast, because the 
###   arima command assumes zero meabs and hence gives
###   incorrect answers. 
#######################################################

### Use sarima function to fit, and compare to previous result. 
### Should be the same if option is no.constant=TRUE, i.e.,you
### do not include an intercept delta in the model.
### Note: if you use the option no.constant=FALSE, the results from
### sarima and arima function calls will be different!

### (i) Fit ARIMA(1,1,1)*(0,1,1)_12 to FRBPROD data, no constant
frbfit1<- sarima(frbprod,1,1,1,0,1,1,12,no.constant=TRUE)
frbfit1
### Need to save the model residuals 
frbfit1$resid=frbfit1$fit$resid

### Model Adequacy:  
### Need to do Residual Plots: time series plot, histogram, Normal Q-Q plot,
### ACF, PACF
### Portmanteau test for model adequacy:
Box.test (frbfit1$resid, lag = 48,type="Ljung")
#########################################################

### Fit ARIMA(1,1,1)*(2,1,0)_12 to FRBPROD data
frbfit2<- sarima(frbprod,1,1,1,2,1,0,12,no.constant=TRUE)
frbfit2
frbfit2$resid=frbfit2$fit$resid
### Again Check residuals: Residual Plots: time series plot, histogram, 
### Normal Q-Q plot,ACF, PACF
### Portmanteau test for model adequacy: 
Box.test (frbfit2$resid, lag = 48,type="Ljung")

####################################################### 

### (iii) Fit ARIMA(1,1,1)*(2,1,1)_12 to FRBPROD data
frbfit3<- sarima(frbprod,1,1,1,2,1,1,12,no.constant=TRUE)
frbfit3
frbfit3$resid=frbfit3$fit$resid
### Again Check residuals: Residual Plots: time series plot, histogram, 
### Normal Q-Q plot,ACF, PACF
### Portmanteau test for model adequacy: 
Box.test (frbfit3$resid, lag = 48,type="Ljung")
##############################################################

### Forecasts from Model (iii): SARIMA(1,1,1)*(2,1,1)_12 
### Also plots the data and forecasts with prediction intervals
prod.pr = sarima.for(frbprod, n.ahead=12,1,1,1,2,1,1,12)
prod.pr 

### To compute and plot the point forecasts and forecast
### intervals manually along with a portion of the data
U = prod.pr$pred + 1.96*prod.pr$se 
L = prod.pr$pred - 1.96*prod.pr$se
month=337:372
plot(month,frbprod[month],type="o",xlim=c(337,384),ylim=c(100,180),ylab="FRB Production")
lines(prod.pr$pred,col="red",type="o")
lines(U, col="blue",lty="dashed")
lines(L, col="blue",lty="dashed")
abline(v=372.5,lty="dotted")
lines(U, col="blue", lty="dashed") 
lines(L, col="blue", lty="dashed") 

#########################################################

### Automatic Fitting of several low-order models.
### You may use this instead of trying to choose p,q, P, Q 
### from ACF and PACF plots!
### AIC is written out for all fitted models. Choose the
### model(s) with lowest AIC, and focus on those for closer
### look. You can also use BIC instead of AIC.
### OR, if you are really enterprising,
### you can also compute the MAPE within the loop and write out
### BOTH the AIC and MAPE, and select models.


per=12     # seasonal period
d=1        # nonseasonal degree of differencing 
sd=1	   # seasonal degree of differencing
np=1	   # nonseasonal AR order
nq=3       # nonseasonal MA order
nsp=1      # seasonal AR order
nsq=2      # seasonal AR order


### Set up loops to run several possible models
### out is a file which will be saved on your local directory
### Here we let p=0,1;  q=0,1,2,3;  P=0,1; and Q=0,1,2 
for (p in 0:np){
   for (q in 0:nq) {
    for (sp in 0:nsp){
      for (sq in 0:nsq){
         frbfit<-sarima(frbprod,p,d,q,sp,sd,sq,per,no.constant=TRUE)
         outsarima <- c(p, q, sp, sq, frbfit$AIC)
         write(outsarima,file="C://Users//nra02001//Dropbox//ST45825//RCommands//outsarima",append=TRUE)
          }
 }
 }
 }
outsarima.all <- read.table("C://Users//nra02001//Dropbox//ST45825//RCommands//outsarima")
colnames(outsarima.all) = c("p", "q", "sp", "sq", "AIC")
outsarima.all





#########################################################

### We use the arima function instead of the sarima function
#########################################################


### Fit ARIMA(1,1,1)*(0,1,1)_12 to FRBPROD data
### Note: We may assume safely that after a nonseasonal and 
### a seasonal differencing, the frbprod_dif1dif12 is a stationary 
### time series, and has zero mean. 

### The arima function should be OK here, since nonzero mean 
### may be assumed
### NOTE: AIC = -2*max loglik +2*no. of params

### (i) Fit ARIMA(1,1,1)*(0,1,1)_12 to FRBPROD data
frbfit1<- arima(frbprod, order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
frbfit1
Box.test (frbfit1$resid, lag = 48,type="Ljung")

### (ii) Fit ARIMA(1,1,1)*(2,1,0)_12 to FRBPROD data
frbfit2<- arima(frbprod, order=c(1,1,1),seasonal=list(order=c(2,1,0),period=12))
frbfit2
Box.test (frbfit2$resid, lag = 48,type="Ljung")

### (iii) Fit SARIMA(1,1,1)*(2,1,1)_12 to FRBPROD data
frbfit3<- arima(frbprod, order=c(1,1,1),seasonal=list(order=c(2,1,1),period=12))
frbfit3
Box.test (frbfit3$resid, lag = 48,type="Ljung")

### Forecasts from Model (iii)
prod.pr = predict(frbfit3, n.ahead=12)
prod.pr 
U = prod.pr$pred + 1.96*prod.pr$se 
L = prod.pr$pred - 1.96*prod.pr$se
month=337:372
plot(month,frbprod[month],type="o",xlim=c(337,384),ylim=c(100,180),ylab="FRB Production")
lines(prod.pr$pred,col="red",type="o")
lines(U, col="blue",lty="dashed")
lines(L, col="blue",lty="dashed")
abline(v=372.5,lty="dotted")
lines(U, col="blue", lty="dashed") 
lines(L, col="blue", lty="dashed") 

#########################################################

### Automatic Fitting of several low-order models.
### You may use this instead of trying to choose p,q, P, Q 
### from ACF and PACF plots!
### AIC is written out for all fitted models. Choose the
### model(s) with lowest AIC, and focus on those for closer
### look. You can also use BIC instead of AIC.
### OR, if you are really enterprising,
### you can also compute the MAPE within the loop and write out
### BOTH the AIC and MAPE, and select models.

per=12
d=1
sd=1

### Set up loops to run several possible models
for (p in 0:1){
   for (q in 0:3) {
    for (sp in 0:1){
      for (sq in 0:2){
         frbfit<-arima(frbprod, order=c(p,d,q),seasonal=list(order=c(sp,sd,sq),period=per))
         outarima <- c(p, q, sp, sq, frbfit$aic)
         write(outarima,file="C://Users//nra02001//Dropbox//ST45825//RCommands//outarima",append=TRUE)
          }
 }
 }
 }
outarima.all <- read.table("C://Users//nra02001//Dropbox//ST45825//RCommands//outarima")
colnames(outarima.all) = c("p", "q", "sp", "sq", "aic")
outarima.all


### End of Code
#########################################################



























