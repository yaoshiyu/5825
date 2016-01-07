library(stats)
help(EuStockMarkets)
data=EuStockMarkets
SMI=ts(data[,2])
smi=diff(log(SMI))
par(mfrow=c(2,2))
ts.plot(smi)
acf(smi)
acf(smi**2)
pacf(smi**2)
library(fGarch)
smi.arch1.t = garchFit(~garch(1, 0), data = smi)
smi.arch1.t
summary(smi.arch1.t)

smi.arch1 = garchFit(~garch(1, 1), data = smi)
smi.arch1
summary(smi.arch1)

smi.arch1.fit=smi.arch1@fitted	# fitted time series
smi.arch1.res=smi.arch1@residuals	# residuals time series
smi.arch1.h.t=smi.arch1@h.t		# conditional volatilities 
smi.arch1.sigma.t=smi.arch1@sigma.t	# conditional variances


windows()
par(mfrow=c(3,2))
ts.plot(smi,main="smi data")
ts.plot(smi.arch1.fit,main="GARCH(1,1) Fits")
ts.plot(smi.arch1.res,main="GARCH(1,1) resids")
ts.plot(smi.arch1.h.t,main="GARCH(1,1) cond volatilities: h.t")
ts.plot(smi.arch1.sigma.t,main="GARCH(1,1) cond variances: sigma.t")

