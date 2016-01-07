##############################################################
### R Commands for Topic 12: ARCH/GARCH Moddel fit using fGarch
### Ref: Shumway and Stoffer 2011
##############################################################

library(astsa)
library(fGarch)

###############################################################
### NYSE data
### r_t:  n=2000 daily returns from NYSE from 02/02/1984 to 12/31/1991. 
### Note: Oct 19, 1987 corresponds to t=938
### Ref: Shumway and Stoffer, 2011; data is in astsa package 

###############################################################

data(nyse)

###############################################################
### R notation == My notation
### omega ==  alpha0
### alpha1 == alpha1
### beta1   == beta1
###############################################################


###############################################################
### Case 1: ARCH(1) model fit with std Normal Errors (default).
### Fit includes a mean mu (default).
###############################################################

nyse.arch1.t = garchFit(~garch(1, 0), data = nyse)

### Fit description and summary
nyse.arch1.t

summary(nyse.arch1.t)

### Recover output and Plot
nyse.arch1.fit=nyse.arch1@fitted	# fitted time series
nyse.arch1.res=nyse.arch1@residuals	# residuals time series
nyse.arch1.h.t=nyse.arch1@h.t		# conditional volatilities 
nyse.arch1.sigma.t=nyse.arch1@sigma.t	# conditional variances

###
windows()
par(mfrow=c(3,2))
ts.plot(nyse,main="nyse data")
ts.plot(nyse.arch1.fit,main="ARCH(1) Fits")
ts.plot(nyse.arch1.res,main="ARCH(1) resids")
ts.plot(nyse.arch1.h.t,main="ARCH(1) cond volatilities: h.t")
ts.plot(nyse.arch1.sigma.t,main="ARCH(1) cond variances: sigma.t")

### Prediction into the future
nyse.arch1.pred=predict(nyse.arch1,n.ahead=10,trace=F)
nyse.arch1.pred
   



###############################################################
### Case 2: GARCH(1,1) Model with Std. Normal Errors
### See S&S Example 5.5 
###############################################################

nyse.g11 = garchFit(~garch(1, 1), data = nyse, include.mean=F)
formula(nyse.g11)
nyse.g11

### Recover output and Plot
nyse.g11.fit=nyse.g11@fitted	      # fitted values time series
nyse.g11.res=nyse.g11@residuals	      # residuals time series
nyse.g11.h.t=nyse.g11@h.t             # cond volatilities 
nyse.g11.sigma.t=nyse.g11@sigma.t     # conditional variances

windows()
par(mfrow=c(3,2))
ts.plot(nyse,main="nyse data")
ts.plot(nyse.g11.fit,main="GARCH(1,1) Fits")
ts.plot(nyse.g11.res,main="GARCH(1,1) resids")
ts.plot(nyse.g11.h.t,main="GARCH(1,1) cond. volatilities: h.t")
ts.plot(nyse.g11.sigma.t,main="GARCH(1,1) cond. variances: sigma.t")

### Prediction into the future
predict(nyse.g11,n.ahead=10,trace=F)
   
###############################################################



###############################################################
### Case 3: AR(1)+ GARCH(1,1) Model with Std. Normal Errors
### See S&S Example 5.5 
###############################################################

nyse.ar1g11 = garchFit(~arma(1,0)+garch(1, 1), data = nyse, include.mean=F)
formula(nyse.ar1g11)
nyse.g11

### Recover output and Plot
nyse.ar1g11.fit=nyse.ar1g11@fitted	      # fitted values time series
nyse.ar1g11.res=nyse.ar1g11@residuals	      # residuals time series
nyse.ar1g11.h.t=nyse.ar1g11@h.t             # cond volatilities 
nyse.ar1g11.sigma.t=nyse.ar1g11@sigma.t     # conditional variances

windows()
par(mfrow=c(3,2))
ts.plot(nyse,main="nyse data")
ts.plot(nyse.ar1g11.fit,main="AR(1)+GARCH(1,1) Fits")
ts.plot(nyse.ar1g11.res,main="GARCH(1,1) resids")
ts.plot(nyse.ar1g11.h.t,main="GARCH(1,1) cond. volatilities: h.t")
ts.plot(nyse.ar1g11.sigma.t,main="GARCH(1,1) cond. variances: sigma.t")

### Prediction into the future
predict(nyse.ar1g11,n.ahead=10,trace=F)
###############################################################
   


###############################################################
### Case 4: ARCH(1) model fit with Student t errors 
### Fit includes a mean mu (default).
###############################################################

nyse.arch1.t = garchFit(~garch(1, 0), data = nyse,cond.dist="std")

### Fit description and summary
nyse.arch1.t

summary(nyse.arch1.t)

### Recover output and Plot
nyse.arch1.fit=nyse.arch1@fitted	# fitted time series
nyse.arch1.res=nyse.arch1@residuals	# residuals time series
nyse.arch1.h.t=nyse.arch1@h.t		# conditional volatilities 
nyse.arch1.sigma.t=nyse.arch1@sigma.t	# conditional variances

###
windows()
par(mfrow=c(3,2))
ts.plot(nyse,main="nyse data")
ts.plot(nyse.arch1.fit,main="ARCH(1) Fits")
ts.plot(nyse.arch1.res,main="ARCH(1) resids")
ts.plot(nyse.arch1.h.t,main="ARCH(1) cond volatilities: h.t")
ts.plot(nyse.arch1.sigma.t,main="ARCH(1) cond variances: sigma.t")

### Prediction into the future
nyse.arch1.pred=predict(nyse.arch1,n.ahead=10,trace=F)
nyse.arch1.pred
   



### End of Code
###############################################################

