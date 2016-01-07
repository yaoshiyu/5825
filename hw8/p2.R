p2=read.table("P://5825//hw8//globtemp.dat",sep=",") 
dim(p2)
gt=p2$V1
par(mfrow=c(1,1))
ts.plot(gt)

dgt=diff(gt)
par(mfrow=c(2,2))
ts.plot(dgt)
acf(dgt,lag=36)
pacf(dgt,lag=36)

library(tseries)
adf.test(dgt, alternative=c("stationary"),k=trunc((length(dgt)-1)^(1/3)))

d=0   # no differencing needed
np=4    #maximum AR order
nq=4    # maximum MA order
# Set up loops to run several possible models
for (p in 0:np){
   for (q in 0:nq) {
     dgt.fit = sarima(dgt,p,d,q)
      outarima <- c(p,q,dgt.fit$BIC)
         write(outarima,file="P://5825//hw8//outarima",append=TRUE)
          }
 }
outarima.all <- read.table("P://5825//hw8//outarima")
colnames(outarima.all) = c("p", "q", "BIC")
outarima.all

library(astsa)
dgt012=sarima(gt,0,1,2)  
dgt012
n.ahead=10
dgt.fore=sarima.for(gt,n.ahead=10,0,1,2)
dgt.fore

U = dgt.fore$pred + 1.96*dgt.fore$se
L = dgt.fore$pred - 1.96*dgt.fore$se

