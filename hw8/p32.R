# model 2
library(astsa)
par(mfrow=c(3,1))
ts.plot(jj)
acf(jj,60,main="Sample ACF")   
pacf(jj,60,main="Sample PACF")

jjl1=log(jj)
jjl=jjl1[1:80]
jj.c=jjl1[81:84]

par(mfrow=c(2,2))
jj_dif1=diff(jjl)
ts.plot(jj_dif1,main="First Diff")
acf(jj_dif1,lag=60)
pacf(jj_dif1,lag=60)

jj_dif1dif12=diff(jj_dif1,12)
par(mfrow=c(2,2))
ts.plot(jj_dif1dif12,main="(1-B)(1-B^12)")
acf(jj_dif1dif12,lag=60)
pacf(jj_dif1dif12,lag=60)

per=4
d=1
sd=1

### Set up loops to run several possible models
for (p in 0:2){
   for (q in 0:2) {
    for (sp in 0:2){
      for (sq in 0:2){
         jjfit<-arima(jjl,order=c(p,d,q),seasonal=list(order=c(sp,sd,sq),period=per))
         outarima <- c(p, q, sp, sq, jjfit$aic)
         write(outarima,file="P://5825//hw8//outarima8",append=TRUE)
          }
 }
 }
 }
outarima.all <- read.table("P://5825//hw8//outarima8")
colnames(outarima.all) = c("p", "q", "sp", "sq", "aic")
outarima.all


jjfit<- arima(jjl, order=c(0,1,1),seasonal=list(order=c(2,1,1),period=4))
jjfit
prod.pr = predict(jjfit, n.ahead=4)
prod.pr 
en=prod.pr$pred-jj.c
mape=100*mean(abs((en)/jj.c))



