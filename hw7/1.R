par(mfrow=c(3,1))
set.seed(123456789)
prob1_arma11=arima.sim(list(order=c(1,0,1),ar=c(0.9),ma=c(-0.9)),n=500)
ts.plot(prob1_arma11,main="Simulated ARMA(1,1)",xlab="Time",ylab="ARMA(1,1)")
prob1_sacf=acf(prob1_arma11,lag.max=36,plot=TRUE,main="Sample ACF for ARMA(1,1)")
prob1_spacf=acf(prob1_arma11,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF for ARMA(1,1)")
abline(h=0)

par(mfrow=c(3,1))
fit=arima(prob1_arma11,c(1,0,1))
fit
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2
resid=residuals(fit)
ts.plot(resid,main="residuals",xlab="Time",ylab="residuals")
qqnorm(resid,ylab="Residuals")
qqline(resid)
acf(resid,125)
r2=resid^2
lb=Box.test(resid,lag=125,type="Ljung")
lb
lb2=Box.test(r2,lag=125,type="Ljung")
lb2

par(mfrow=c(3,1))
set.seed(987654321)
prob1_arma11=arima.sim(list(order=c(1,0,1),ar=c(0.9),ma=c(-0.9)),n=500)
ts.plot(prob1_arma11,main="Simulated ARMA(1,1)",xlab="Time",ylab="ARMA(1,1)")
prob1_sacf=acf(prob1_arma11,lag.max=36,plot=TRUE,main="Sample ACF for ARMA(1,1)")
prob1_spacf=acf(prob1_arma11,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF for ARMA(1,1)")
abline(h=0)
par(mfrow=c(3,1))
fit=arima(prob1_arma11,c(1,0,1))
fit
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2

par(mfrow=c(3,1))
set.seed(66666666)
prob1_arma11=arima.sim(list(order=c(1,0,1),ar=c(0.9),ma=c(-0.9)),n=500)
ts.plot(prob1_arma11,main="Simulated ARMA(1,1)",xlab="Time",ylab="ARMA(1,1)")
prob1_sacf=acf(prob1_arma11,lag.max=36,plot=TRUE,main="Sample ACF for ARMA(1,1)")
prob1_spacf=acf(prob1_arma11,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF for ARMA(1,1)")
abline(h=0)
par(mfrow=c(3,1))
fit=arima(prob1_arma11,c(1,0,1))
fit
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2