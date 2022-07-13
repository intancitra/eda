setwd('E://AED')

kde=function(f,Xi,h,x)
{
  l=length(x)
  n=length(Xi)
  k=matrix(nrow=l,ncol=n)
  kt=matrix(nrow=l,ncol=1)
  for(i in 1:l)
  {
    for(j in 1:n)
    {
      k[i,j]=f((x[i]-Xi[j])/h)*1/(n*h)
    }
    kt[i]=sum(k[i,])
  }
  for(i in 1:n)
  {
    plot(x,k[,i],type='l')
  }
  plot(x,kt,type='l', main="Plot Kernel")
}

gaussian=function(x)
{
  1/sqrt(2*pi)*exp(-1/2*(x)^2)
}

Domain=seq(0,6,0.1)
Domain

nilai=c(1,1.2,3.2,4,5.1)

kde(gaussian,nilai,0.5,Domain)
