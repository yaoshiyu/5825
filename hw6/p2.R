u=polyroot(c(1,0,.9))
Mod(u)
ar2acf=ARMAacf(ar=c(0,-0.9),ma=0,lag.max=36,pacf=FALSE)
plot(ar2acf,type="h",xlab="lag",main="ACF of AR(2)")
abline(h=0)