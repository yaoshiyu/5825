p1=read.table("P://5825//hw1//skisales.csv",header=TRUE,sep=",")  
dim(p1)
p1$time
p1$sales
p1$disp_income
p1[,1]
p1[,2]
p1[,3]
plot(p1[,2]~p1[,3],pch=16,xlab="sales",ylab="income",main="Scatter plot income vs sales")

p1_slr = lm(p1$sales~p1$disp_income,data=p1) 
summary(p1_slr)
coef(p1_slr)           # regression coefficients
varcov=vcov(p1_slr)    # Cov(parameter estimate vector)
varcov
se=sqrt(diag(varcov))   # s.e. of parameter estimates
se

### scatterplot with fitted regression line overlay
plot(p1$sales~p1$disp_income,data=p1,pch=16)
abline(p1_slr)

resid_slr=residuals(p1_slr)
qqnorm(resid_slr,ylab="Resid_slr")
qqline(resid_slr)

t=c(1:40)
plot(t,resid_slr,xlab="time",ylab="Residuals",ylim=max(abs(resid_slr))*c(-1,1),pch=16,main="Plot of Residuals vs time")
abline(h=0,lty=2)

hist(resid_slr,main='Histogram of residuals')