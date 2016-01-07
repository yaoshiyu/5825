library(astsa)
dim(econ5)
names(econ5)
u=econ5$unemp
g=econ5$gnp
c=econ5$consum
par(mfrow=c(3,1))
ts.plot(u)
ts.plot(g)
ts.plot(c)
par(mfrow=c(3,1))
lune=log(u)
lgnp=log(g)
lcon=log(c)
ts.plot(lune)
ts.plot(lgnp)
ts.plot(lcon)

t=1:length(lune)
fit1=lm(lune~t)
unemp=ts(fit1$resid)
fit2=lm(lgnp~t)
gnp=ts(fit2$resid)
fit3=lm(lcon~t)
consum=ts(fit3$resid)

triv=ts.union(unemp,gnp,consum)
dim(triv)

acf(triv)

library(vars)
econ.best=VARselect(triv,lag.max=10,type="both")
econ.best

fitvar3_ols = ar.ols(triv,order=3,demean=F,intercept=T)
fitvar3_ols

resid=fitvar3_ols$resid[4: length(gnp),]
acf(ts(resid))

install.package("dse")
library("dse")
summary(resid)
dim(resid)
Portmanteau(resid)