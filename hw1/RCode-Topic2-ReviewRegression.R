##############################################################
### R Commands for Topic 2: Review of Regression
### 
##############################################################

library(astsa)

##############################################################

### Data Set 1: SLR model

sbp = read.table("C://Users//nra02001//Dropbox//ST45825//Data//SbpAge.csv",header=TRUE,sep=",")  
sbp
 
dim(sbp)
sbp$SBP
sbp$AGE
sbp[,1]
sbp[,2]

### Scatterplot of Y vs X
plot(sbp[,1]~sbp[,2],pch=16,xlab="Age",ylab="SBP",main="Scatter plot SBP vs Age")
###############################################################
 
### Data Set 2: MLR model

psychotic=read.table("C://Users//nra02001//Dropbox//ST45825//Data//psychotic.csv",header=TRUE,sep=",")
dim(psychotic)
psychotic$Y
psychotic$X1
psychotic$X2
###############################################################

### scatterplot matrix of all variables
pairs(psychotic)

###############################################################

### Pairwise Correlation Coefficient
###For SBP vs Age:

corr<-cor(sbp$SBP,sbp$AGE)
corr

###For psychotic data:

corp<-cor(psychotic[,2:4])
corp
           
###############################################################

### Fit an SLR Model to SBP-Age data

sbp_slr = lm(sbp$SBP~sbp$AGE,data=sbp) 
summary(sbp_slr)
coef(sbp_slr)           # regression coefficients
varcov=vcov(sbp_slr)    # Cov(parameter estimate vector)
varcov
se=sqrt(diag(varcov))   # s.e. of parameter estimates
se

### scatterplot with fitted regression line overlay
plot(sbp$SBP~sbp$AGE,data=sbp,pch=16)
abline(sbp_slr)

###############################################################


### Fit MLR Model to psychotic data

Y<-psychotic$Y
X1<-psychotic$X1
X2<-psychotic$X2

psychotic_mlr1 = lm(Y~X1+X2,data=psychotic) 
psychotic_mlr2 = lm(Y~X1+X2+X1*X2,data=psychotic) 
summary(psychotic_mlr1)
summary(psychotic_mlr2)

### Create Vector of Fitted values and Vector of Residuals 
### for each model fit

### Model mlr1
fit_mlr1=fitted(psychotic_mlr1)
resid_mlr1=residuals(psychotic_mlr1)


### Residuals vs Fits Plot and draw y=0 line
plot(fit_mlr1,resid_mlr1,xlab="Fitted values: mlr1",ylab="Residuals: mlr1",ylim=max(abs(resid_mlr1))*c(-1,1),pch=16,main="Plot of Residuals vs Fits: mlr1")
abline(h=0,lty=2)

### Normal Q-Q Plot of residuals for mlr1
qqnorm(resid_mlr1,ylab="Resid_mlr1")
qqline(resid_mlr1)

### Diagnostics Plots: mlr1
par(mfrow = c(2, 2))
plot(psychotic_mlr1, main="mlr1",which = 1:4)
 

### Model mlr2
fit_mlr2=fitted(psychotic_mlr2)
resid_mlr2=residuals(psychotic_mlr2)

### Residuals vs Fits Plot and draw y=0 line
plot(fit_mlr2,resid_mlr2,xlab="Fitted values: mlr2",ylab="Residuals: mlr2",ylim=max(abs(resid_mlr2))*c(-1,1),pch=16,main="Plot of Residuals vs Fits: mlr2")
abline(h=0,lty=2)


### Normal Probability Plot of residuals for mlr2
qqnorm(resid_mlr2,ylab="Resid_mlr2")
qqline(resid_mlr2)


### Diagnostics Plots: mlr2
par(mfrow = c(2, 2))
plot(psychotic_mlr2, main="mlr2",which = 1:4)

### End of Code
###############################################################



