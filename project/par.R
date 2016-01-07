#parameter
p=1
T=40
S=12
a0=0.5
a1=0.5
a2=0.5
u=c(1,1,1,1,1,1,1,1,1,1,1,1)
y0=1.2

#compute phi
phi0=matrix(0,S,p)
np=S*p
for (s in 1:np)
{
phi0[s,]=a0+a1*cos(2*pi*s/S-a2*pi)
}
w=rnorm(12, mean = 0, sd = 0.2)
phi=phi0+w

#generate et
e=rnorm(T*S, mean = 0, sd = 1)

#generate yt
n=S*T
y=matrix(0,n,1)
y[1,]=u[1]+phi[1,]*y0
for (t in 2:n)
{ 
s=t%%12
if (s==0)
{s=12}
{y[t,]=u[s]+phi[s,]*y[t-1,]+e[t]} 
}

y=ts(y)
ts.plot(y)
