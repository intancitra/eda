library(ggplot2)

mpg <- read.csv("http://goo.gl/uEeRGu",header=TRUE,sep=',')

n=length(mpg[,1])
u=runif(n,-0.5,0.5)
x=mpg[,"cty"]
y=mpg[,"hwy"]
x=x+u

#Plot Manual
jitter=plot(x,y,main="Jitter Plot Manual",xlab="City",ylab="Highway")

#Plot ggplot2
gg=ggplot(mpg, aes(cty, hwy)) + geom_jitter() + labs(y="Highway", x="City", title="Jitter Plot ggplot2")
gg
