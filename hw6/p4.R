#ar1 u=0 phi=0.6#
par(mfrow=c(3,2))
set.seed(12346789)
prob1_ar1=arima.sim(list(order=c(1,0,0),ar=c(0.6)),n=100)
prob1_sacf=acf(prob1_ar1,lag.max=36,plot=TRUE,main="Sample ACF for AR(1)")
prob1_spacf=acf(prob1_ar1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF for AR(1)")
abline(h=0)

prob1_ma1=arima.sim(list(order=c(0,0,1),ma=c(0.9)),n=100)
prob1_sacf=acf(prob1_ma1,lag.max=36,plot=TRUE,main="Sample ACF for MA(1)")
prob1_spacf=acf(prob1_ma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF for MA(1)")
abline(h=0)


prob5_arma1=arima.sim(list(order=c(1,0,1),ar=c(0.6),ma=c(0.9)),n=100)
prob5_sacf=acf(prob5_arma1,lag.max=36,plot=TRUE,main="Sample ACF")
abline(h=0)
prob5_spacf=acf(prob5_arma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
abline(h=0)
