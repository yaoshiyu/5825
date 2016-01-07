par(mfrow=c(2,1))

s=c(rep(0,100),10*exp(-(1:100)/20)*cos(2*pi*1:100/4))
u1=ts(s)
plot(u1,col="blue",main="mean function of problem a")

s1=c(rep(0,100),10*exp(-(1:100)/200)*cos(2*pi*1:100/4))
u2=ts(s1)
plot(u2,col="red",main="mean function of problem b")