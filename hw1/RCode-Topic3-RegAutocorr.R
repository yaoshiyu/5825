##############################################################
### R Commands for Topic 3: Regression with Autocorrelated Errors
### 
##############################################################

library(astsa)
library(nlme)
library(lmtest)
library(car)
##############################################################

### Expenditure Data
expend = read.table("C://Users//nra02001//Dropbox//ST45825//Data//expend.csv",header=TRUE,sep=",")

dim(expend)
expend[,1]
expend[,2]
expend[,3]

X=expend[,2]
Z=expend[,3]

###############################################################
### Fit a Linear Regression Model
expend_lm = lm(X~Z,data=expend)
summary(expend_lm)

expend_resid=residuals(expend_lm)
plot(expend[,1],expend_resid,xlab="Time",ylab="Residuals",ylim=max(abs(expend_resid))*c(-1,1),pch=16,main="Plot of Residuals vs Time")

###############################################################
### Durbin-Watson test in package "lmtest"
### H1: rho > 0
### p-value is constructed with the "pan" algorithm. 
### See URL given above for details.

dwtest(X~Z)

### Two-sided alternative: H1: rho neq 0
dwtest(X~Z,alternative=c("two.sided"))

###############################################################
### OR
### Durbin-Watson test using "car" package
install.packages("car")
library("car")
### Test lag 1 autocorrelation rho_1 by setting max.lag=1
durbinWatsonTest(expend_lm,max.lag=1,simulate=F,alternative=c("positive"))

###############################################################
### Cochrane-Orcutt Procedure
### Posted by John Fox, McMaster University
### mod is the output from the lm fit of Response Y on 
### predictor X
### Example: set mod=expend_lm


### You may copy this function into R and then run as shown below
################################################################
### Function:

cochrane.orcutt.lm <- function(mod){ 
X <- model.matrix(mod) 
y <- model.response(model.frame(mod)) 
e <- residuals(mod) 
n <- length(e) 
names <- colnames(X) 
rho <- sum(e[1:(n-1)]*e[2:n])/sum(e^2) 
y <- y[2:n] - rho * y[1:(n-1)] 
X <- X[2:n,] - rho * X[1:(n-1),] 
mod <- lm(y ~ X - 1) 
result <- list() 
result$coefficients <- coef(mod) 
names(result$coefficients) <- names 
summary <- summary(mod, corr = F) 
result$cov <-(summary$sigma^2)*summary$cov.unscaled 
dimnames(result$cov) <- list(names, names) 
result$sigma <- summary$sigma 
result$rho <- rho 
class(result) <- 'cochrane.orcutt' 
result 
} 
################################################################

### call this function cochrane.orcutt.lm to get result
### argument of the function is output from original OLS regression

cochrane.orcutt.lm(expend_lm)$coefficients
cochrane.orcutt.lm(expend_lm)$cov
cochrane.orcutt.lm(expend_lm)$sigma
cochrane.orcutt.lm(expend_lm)$rho

###############################################################
### ACF and PACF plot of OLS residuals to assess higher order 
### autocorrelations
acf(expend_resid)
pacf(expend_resid)

###############################################################

### In R, we can use gls function in the nlme package to run
### Generalized Linear Model fit for regression with serially
### correlated errors 



### Get GLS estimates, using method of ML, 
###   and assuming serial correlation.
### Default: form= ~ 1, which corresponds to using the order 
###  of the observations in the data as a covariate, 
###   and no groups.

expend_gls <- gls(X ~ Z, data=expend,correlation = corAR1(0.7524,form=~1),method="ML")
summary(expend_gls)

###############################################################

### Generalized Durbin-watson test for higher order autocorrelations
### Use DurbinWatsonTest function in package "car"
### Test to lag 4 autocorrelation by setting max.lag=4
durbinWatsonTest(expend_lm,max.lag=4,simulate=F,alternative=c("positive"))

### You can now use the gls function with a more general form in correlation = 

### End of Code
###############################################################