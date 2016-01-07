##############################################################
### R Commands for Topic 11: Vector Time Series
### Fitting VAR(p) and VARX Models to Data
### Ref: Shumway and Stoffer 2011

##############################################################

library(astsa)

##############################################################
### Example: econ5 series
### See Shumway and Stoffer, 2011. 
##############################################################

library(astsa)
library(vars)

### Quarterly U.S. unemployment, GNP, consumption, and government 
### and private investment, from Q3 1948 to Q2 1988.
### 161 observations (rows) on unemp, gnp, consum, govinv, prinv
data(econ5)

### Create growth rate
gr = diff(log(ts(econ5, start=1948, frequency=4))) # growth rate

### Plot the series
windows()
plot(gr, main="Growth rates")

windows()
plot(100*gr, main="Growth Rates (%)")  # 100*gr


windows()
### purple: low values, grey: medium values, green: high values.
mvtsplot(gr)


### CCF plots
windows()
acf(gr)


##############################################################
### Fit a VAR(1) model to the 5-variate series using ar.yw
##############################################################
econ.yw = ar.yw(gr,order=1,demean=F,intercept=T)
econ.yw


##############################################################
### Fit a VAR(1) model to the 5-variate series using ar.ols
##############################################################
econ.ols = ar.ols(gr,order=1,demean=F,intercept=T)
econ.ols 


##############################################################
# Note:  ar.mle only implemented for univariate series
# and not for vector time series,
# so the call
# ar.mle(gr,order=1,demean=F,intercept=T) will give an error.
#############################################################




##############################################################
### Fit a VAR(1) model to the 5-variate series using VAR in vars
### Fit a constant and time trend
##############################################################
econ.var1.1=VAR(gr,p=1,type="both")  
econ.var1.1
summary(econ.var1.1)
coef(econ.var1.1)

### Fitted
fit.econ.var1.1=as.vector(fitted(econ.var1.1)[,1])
fit.econ.var1.2=as.vector(fitted(econ.var1.1)[,2])
fit.econ.var1.3=as.vector(fitted(econ.var1.1)[,3])
fit.econ.var1.4=as.vector(fitted(econ.var1.1)[,4])
fit.econ.var1.5=as.vector(fitted(econ.var1.1)[,5])

### Residuals
windows()
res.econ.var1.1=as.vector(resid(econ.var1.1)[,1])
res.econ.var1.2=as.vector(resid(econ.var1.1)[,2])
res.econ.var1.3=as.vector(resid(econ.var1.1)[,3])
res.econ.var1.4=as.vector(resid(econ.var1.1)[,4])
res.econ.var1.5=as.vector(resid(econ.var1.1)[,5])

### cbind all residuals and plot their ACF
res.econ.var1=cbind(res.econ.var1.1,res.econ.var1.2,res.econ.var1.3,res.econ.var1.4,res.econ.var1.5)
acf(res.econ.var1)

### MV Portmanteau test on residuals
serial.test(econ.var1.1,lags.pt=24,type="PT.adjusted")


##############################################################
### Select the VAR(p) order and sumamrize the best model
##############################################################
econ.best=VARselect(gr,lag.max=10,type="both")
econ.best
### SIC selects the VAR(1) model 


##############################################################
### Prediction using a VAR(1) model with Interceppt and trend
##############################################################

pred.econ.var1.1=predict(econ.var1.1,n.ahead=4,ci=0.95)
pred.econ.var1.1

### Fan chart produces time series plots of VAR forecasts with 
### differently shaded confidence regions (fanchart) for each 
### endogenous variable
fanchart(pred.econ.var1.1)


### End of code
##############################################################












##############################################################
### Alternate VAR(p) models
##############################################################

### Note: To fit a model only with constant and no time trend
fit1=VAR(gr, type="const",p=1)


### Note: To fit a model with neither constant and nor time trend
fit2=VAR(gr, type="none",p=1) 




