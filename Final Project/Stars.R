stars1 = read.csv("E:/AED/Stars.csv",header = T,sep=",",check.names = F)
stars1$Type=as.factor(stars1$Type)
colnames(stars1)=c("Temperature","Luminousity","Radius", "Magnitude", "Color", "Spectral_Class", "Type")
head(stars1)
summary(stars1)
library(ggplot2)
library(dplyr)
libra
p = ggplot(stars1, aes(x=Type, y=Magnitude,show.legend = FALSE,fill=Type)) + geom_boxplot() + ggtitle("2021 NASA Stars Classification") + xlab("Star Type") +
  ylab("Star Magnitude")+ theme(text = element_text(size=8),axis.text.x = element_text(angle=90, hjust=1))
p=p+labs(subtitle="Star Magnitude by Type")+theme_light() +
  scale_fill_manual(values = c('#d9ed92','#99d98c','#52b69a','#34a0a4','#168aad','#1a759f'))
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax)) }
p=p + stat_summary(fun.data=data_summary)
p=p+theme(panel.grid = element_line(color = 'white'))
p


