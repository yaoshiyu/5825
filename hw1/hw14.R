p4=read.table("P://5825//hw1//salesadvert.csv",header=TRUE,sep=",")  
dim(p4)
p4$Month
p4$Sales
p4$Adver

p4_slr = lm(p4$Sales~p4$Adver,data=p4) 
summary(p4_slr)
coef(p4_slr)           # regression coefficients
varcov=vcov(p4_slr)    # Cov(parameter estimate vector)
varcov
se=sqrt(diag(varcov))   # s.e. of parameter estimates
se


resid_slr=residuals(p4_slr)
plot(p4$Month,resid_slr,xlab="Month",ylab="Residuals",ylim=max(abs(resid_slr))*c(-1,1),pch=16,main="Plot of Residuals vs Month")
abline(h=0,lty=2)

install.packages("car")
library("car")
durbinWatsonTest(p4_slr,max.lag=1,simulate=T,alternative=c("positive"))

install.packages("nlme")
library("nlme")
x<-p4$Adver
z<-p4$Sales
p4_gls <- gls(z~x, data=p4,correlation = corAR1(0.7524,form=~1),method="ML")
summary(p4_gls)
durbinWatsonTest(p4_slr,max.lag=4,simulate=T,alternative=c("positive"))

acf(resid_slr)
pacf(resid_slr)


