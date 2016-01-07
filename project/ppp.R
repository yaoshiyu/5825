
install.packages("perARMA") 
library(perARMA)

S=12
nlen=480
p=1

a0=0.5
a1=0.5
a2=0.5
a=c(a0,a1,a2)

phi0=matrix(0,S,p)
np=S*p
for (s in 1:np)
{
phi0[s,]=a0+a1*cos(2*pi*s/S-a2*pi)
}

del0=matrix(1,S,1)
PAR1=makepar(nlen,phi0,del0)
par=PAR1$y
plot(ts(par))
y=PAR1$y

#####fitting of PARMA(2,0) model
p=1
q=0
af=matrix(0,S,p)
bf=matrix(0,S,q+1)
af[1:3,1]=1
bf[1,1]=0
parmaf(y,S,p,q,af,bf)


