p1=read.table("P://5825//hw2//skisales.csv",header=TRUE,sep=",")  
p1_slr = lm(p1$sales~p1$disp_income,data=p1) 
resid_slr=residuals(p1_slr)
acf(resid_slr)
pacf(resid_slr)
 
install.packages("car")
library("car")
x<-p1$disp_income
z<-p1$sales
durbinWatsonTest(p1_slr,max.lag=10,simulate=T,alternative=c("positive"))

t=c(1:40)
plot(t,resid_slr,xlab="time",ylab="Residuals",ylim=max(abs(resid_slr))*c(-1,1),pch=16,main="Plot of Residuals vs time")
abline(h=0,lty=2)

install.packages("nlme")
library("nlme")

ski_ar2=gls(z~x,correlation=corARMA(c(0.1,0.1),form=~1,p=2,q=0),method="ML")

summary(ski_ar2)

