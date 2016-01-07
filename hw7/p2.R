p2=read.table("P://5825//hw7//SO2.csv",header=TRUE,sep=",")  
so2=p2$so2
par(mfrow=c(3,1))
ts.plot(so2,main="SO2",xlab="Time",ylab="SO2")
d1so2=diff(so2)
par(mfrow=c(3,1))
ts.plot(d1so2,main="SO2",xlab="Time",ylab="SO2")
dim(p2)
so21=so2[1:500]
d1so21=d1so2[1:500]
length(so21)
par(mfrow=c(2,2))
so2_sacf=acf(d1so21,lag.max=36,plot=TRUE,main="Sample ACF")
so2_spacf=acf(d1so21,lag.max=36,type=c("partial"),plot=TRUE,main="Sample PACF")

fit1=arima(so21,order=c(0,1,1))
fit1
(1-pnorm(abs(fit1$coef)/sqrt(diag(fit1$var.coef))))*2
u1=polyroot(c(1,-0.8386))
Mod(u1)
resid1=residuals(fit1)
par(mfrow=c(3,1))
ts.plot(resid1,main="residuals",xlab="Time",ylab="residuals")
qqnorm(resid1,ylab="Residuals")
qqline(resid1)
acf(resid1,125)
lb1=Box.test(resid,lag=125,type="Ljung")
lb1
r21=resid1^2
ml1=Box.test(r21,lag=125,type="Ljung")
ml1
pred1=predict(fit1,n.ahead=8)
pred1
xn1=so2[501:508]
en1=xn1-pred1$pred
mape1=100*(mean(abs((en1)/xn1)))

fit2=arima(so21,order=c(1,1,1))
fit2
(1-pnorm(abs(fit2$coef)/sqrt(diag(fit2$var.coef))))*2
u2=polyroot(c(1,0.0518))
Mod(u2)
v=polyroot(c(1,-0.8182))
Mod(v)
resid2=residuals(fit2)
par(mfrow=c(3,1))
ts.plot(resid2,main="residuals",xlab="Time",ylab="residuals")
qqnorm(resid2,ylab="Residuals")
qqline(resid2)
acf(resid2,125)
lb2=Box.test(resid2,lag=125,type="Ljung")
lb2
r22=resid2^2
ml2=Box.test(r22,lag=125,type="Ljung")
ml2
pred2=predict(fit2,n.ahead=8)
pred2
xn2=so2[501:508]
en2=xn2-pred2$pred
mape2=100*(mean(abs((en2)/xn2)))

fit=arima(so2,order=c(0,1,1))
fit
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2
u1=polyroot(c(1,-0.8396))
Mod(u1)
resid=residuals(fit)
par(mfrow=c(3,1))
ts.plot(resid1,main="residuals",xlab="Time",ylab="residuals")
qqnorm(resid1,ylab="Residuals")
qqline(resid1)
acf(resid1,125)
lb1=Box.test(resid,lag=125,type="Ljung")
lb1
r21=resid1^2
ml1=Box.test(r21,lag=125,type="Ljung")
ml1
pred=predict(fit,n.ahead=4)
pred






