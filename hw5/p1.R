#ar1 u=10 phi=0.8#
par(mfrow=c(3,2))
set.seed(1234567)
prob1_ar1=arima.sim(list(order=c(1,0,0),ar=c(0.8)),n=500)
prob1_ar1=prob1_ar1+10
ts.plot(prob1_ar1,main="Simulated AR(1) with mu=10 and phi=0.8",xlab="Time",ylab="AR(1)")

prob1_sacf=acf(prob1_ar1,lag.max=36,plot=TRUE,main="Sample ACF")
prob1_tacf=ARMAacf(ar=c(0.8),ma=0,lag.max=36,pacf=FALSE)
plot(prob1_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob1_spacf=acf(prob1_ar1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob1_tpacf=ARMAacf(ar=c(0.8),ma=0,lag.max=36,pacf=TRUE)
plot(prob1_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)



#ar1 u=0 phi=-0.8#
par(mfrow=c(3,2))
set.seed(1234567)
prob2_ar1=arima.sim(list(order=c(1,0,0),ar=c(-0.8)),n=500)
ts.plot(prob2_ar1,main="simulated AR(1) with mu=0 and phi=-0.8",xlab="Time",ylab="AR(1)")

prob2_sacf=acf(prob2_ar1,lag.max=36,plot=TRUE,main="Sample ACF")
prob2_tacf=ARMAacf(ar=c(-0.8),ma=0,lag.max=36,pacf=FALSE)
plot(prob2_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob2_spacf=acf(prob2_ar1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob2_tpacf=ARMAacf(ar=c(-0.8),ma=0,lag.max=36,pacf=TRUE)
plot(prob2_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)



#ma1 u=10 theta=0.5#
par(mfrow=c(3,2))
set.seed(1234567)
prob3_ma1=arima.sim(list(order=c(0,0,1),ma=c(0.5)),n=500)
prob3_ma1=prob3_ma1+10
ts.plot(prob3_ma1,main="simulated MA(1) with mu=10 and theta=0.5",xlab="Time",ylab="MA(1)")

prob3_sacf=acf(prob3_ma1,lag.max=36,plot=TRUE,main="Sample ACF")
prob3_tacf=ARMAacf(ar=0,ma=c(0.5), lag.max=36,pacf=FALSE)
plot(prob3_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob3_spacf=acf(prob3_ma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob3_tpacf=ARMAacf(ar=0,ma=c(0.5) ,lag.max=36,pacf=TRUE)
plot(prob3_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)


#ma1 u=0 theta=-0.5#
par(mfrow=c(3,2))
set.seed(1234567)
prob4_ma1=arima.sim(list(order=c(0,0,1),ma=c(-0.5)),n=500)
ts.plot(prob4_ma1,main="simulated MA(1) with mu=0 and theta=-0.5",xlab="Time",ylab="MA(1)")

prob4_sacf=acf(prob4_ma1,lag.max=36,plot=TRUE,main="Sample ACF")
prob4_tacf=ARMAacf(ar=0,ma=c(-0.5), lag.max=36,pacf=FALSE)
plot(prob4_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob4_spacf=acf(prob4_ma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob4_tpacf=ARMAacf(ar=0,ma=c(-0.5) ,lag.max=36,pacf=TRUE)
plot(prob4_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)


#ARMA(1,1) with mu= 0.0, phi = 0.6 and theta = 0.3#
par(mfrow=c(3,2))
set.seed(1234567)
prob5_arma1=arima.sim(list(order=c(1,0,1),ar=c(0.6),ma=c(0.3)),n=500)
ts.plot(prob5_arma1,main="simulated ARMA(1,1) with mu=0 phi=0.6 and theta=0.3",xlab="Time",ylab="ARMA(1,1)")

prob5_sacf=acf(prob5_arma1,lag.max=36,plot=TRUE,main="Sample ACF")
prob5_tacf=ARMAacf(ar=c(0.6),ma=c(0.3), lag.max=36,pacf=FALSE)
plot(prob5_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob5_spacf=acf(prob5_arma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob5_tpacf=ARMAacf(ar=c(0.6),ma=c(0.3) ,lag.max=36,pacf=TRUE)
plot(prob5_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)


#ARMA(1,1) with mu= 0.0, phi= 0.6 and theta= 0.6#
par(mfrow=c(3,2))
set.seed(1234567)
prob6_arma1=arima.sim(list(order=c(1,0,1),ar=c(0.6),ma=c(0.6)),n=500)
ts.plot(prob6_arma1,main="simulated ARMA(1,1) with mu=0 phi=0.6 and theta=0.6",xlab="Time",ylab="ARMA(1,1)")

prob6_sacf=acf(prob6_arma1,lag.max=36,plot=TRUE,main="Sample ACF")
prob6_tacf=ARMAacf(ar=c(0.6),ma=c(0.6), lag.max=36,pacf=FALSE)
plot(prob6_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob6_spacf=acf(prob6_arma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob6_tpacf=ARMAacf(ar=c(0.6),ma=c(0.6) ,lag.max=36,pacf=TRUE)
plot(prob6_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)


#ARMA(1,1) with mu= 0.0, phi= 0.6 and theta= -0.6 #
par(mfrow=c(3,2))
set.seed(1234567)
prob7_arma1=arima.sim(list(order=c(1,0,1),ar=c(0.6),ma=c(-0.6)),n=500)
ts.plot(prob7_arma1,main="simulated ARMA(1,1) with mu=0 phi=0.6 and theta=-0.6",xlab="Time",ylab="ARMA(1,1)")


prob7_sacf=acf(prob7_arma1,lag.max=36,plot=TRUE,main="Sample ACF")
prob7_tacf=ARMAacf(ar=c(0.6),ma=c(-0.6), lag.max=36,pacf=FALSE)
plot(prob7_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)

prob7_spacf=acf(prob7_arma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")
prob7_tpacf=ARMAacf(ar=c(0.6),ma=c(-0.6) ,lag.max=36,pacf=TRUE)
plot(prob7_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)


#ARIMA(1,1,1) with mu=0.0, phi=0.8 and theta=0.3 #
par(mfrow=c(2,2))
set.seed(123457)
prob8_arma1=arima.sim(list(order=c(1,1,1),ar=c(0.8),ma=c(0.3)),n=500)
ts.plot(prob8_arma1,main="simulated ARMA(1,1,1) with mu=0 phi=0.8 and theta=0.3",xlab="Time",ylab="ARMA(1,1,1)")

prob8_sacf=acf(prob8_arma1,lag.max=36,plot=TRUE,sub="sample acf")
prob8_spacf=acf(prob8_arma1,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")

prob8_tacf=ARMAacf(ar=c(0.8),ma=c(0.3), lag.max=36,pacf=FALSE)
plot(prob8_tacf,type="h",xlab="Lag",ylab="ACF",main="Theoretical ACF")
abline(h=0)
prob8_tpacf=ARMAacf(ar=c(0.8),ma=c(0.3) ,lag.max=36,pacf=TRUE)
plot(prob8_tpacf,type="h",xlab="Lag",ylab="PACF",main="Theoretical PACF")
abline(h=0)