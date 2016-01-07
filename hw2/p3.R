 
x=matrix(scan("P://5825//hw2//eq5exp6.dat"),ncol=2)
plot.ts(x[,1],col="blue",main="Earthquake, Explosion",ylab="")
lines(x[,2],col="red")
par(mfrow=c(2,1))
earthquake=x[,1]
explosion=x[,2]
plot.ts(earthquake,col="blue", main="Earthquake",lty=1)
plot.ts(explosion, col="red",main="Explosion",lty=1)

