par(mfrow=c(2,2))

ar1acf=ARMAacf(ar=c(0.6),ma=0,lag.max=36,pacf=FALSE)
plot(ar1acf,type="h",xlab="lag",main="ACF of ARMA(1,0)")
abline(h=0)


ma1acf=ARMAacf(ar=0,ma=c(0.9),lag.max=36,pacf=FALSE)
plot(ma1acf,type="h",xlab="lag",main="ACF of ARMA(0,1)")
abline(h=0)


armaacf=ARMAacf(ar=c(0.6),ma=c(0.9),lag.max=36,pacf=FALSE)
plot(armaacf,type="h",xlab="lag",main="ACF of ARMA(1,1)")
abline(h=0)