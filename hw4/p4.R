par(mfcol = c(3,2)) 
for (i in 1:6)
{
x = ts(cumsum(rnorm(100,.01,1)))
reg = lm(x~0+time(x), na.action=NULL) 
summary(reg)
plot(x)
lines(.01*time(x), col="red", lty="dashed") 
abline(reg, col="blue") 
} 