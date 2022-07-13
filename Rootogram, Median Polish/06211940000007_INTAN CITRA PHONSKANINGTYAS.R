setwd('E:/AED')

library(readxl)

data=read_excel('E:/AED/Data_ETS_AED.xlsx')

#+++++RESISTANT LINE+++++

x=data$HLS_2019
y=data$IPM_2019

#Menggunakan Resistant Line
R=line(x,y,iter=5)
R

residuals(R)
fitted(R)
coef(R)

#Mengambil koefisiennya
intercept=R$coefficients[1]
slope=R$coefficients[2]

par(mfrow=c(1,1))
plot(x,y)
abline(a=intercept,b=slope,col='red')

#Perbandingan tiap iterasi
R1=line(x,y,iter=1)
R2=line(x,y,iter=2)
R3=line(x,y,iter=3)
R4=line(x,y,iter=4)
R5=line(x,y,iter=5)

plot(x,y)
abline(a=R1$coefficients[1],b=R1$coefficients[2],col='red')
abline(a=R2$coefficients[1],b=R2$coefficients[2],col='blue')
abline(a=R3$coefficients[1],b=R3$coefficients[2],col='green')
abline(a=R4$coefficients[1],b=R4$coefficients[2],col='yellow')
abline(a=R5$coefficients[1],b=R5$coefficients[2],col='black')

#+++++ROOTOGRAM+++++

df=read.table('E://AED/data rootogram.csv',header=T,sep=',')
df$sqrt_chest=sqrt(df$Chest)
head(df)
dfr=rep(df$Chest,df$Count)

library(countreg)

#'arg' should be one of "beta", "cauchy", "chi-squared", "chisquared",
#'"exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic",
#'"logseries", "negative binomial", "negbin", "normal", "gaussian", "poisson", "t", "weibull"

par(mfrow=c(1,2))
rootogram(dfr,fitted='normal',style='standing')
rootogram(dfr,fitted='normal',style='standing',scale='raw')

par(mfrow=c(1,1))
rootogram(dfr,fitted='normal',style='hanging')
r=rootogram(dfr,fitted='normal',style='suspended')
r
#Rootogram tanpa plotting
r=rootogram(dfr,fitted='normal',style='suspended',plot=F)
r

#+++++MEDIAN POLISH+++++

df2 <- data.frame(row.names = c("NE","NC","S","W"),
                   ed8       = c(25.3,32.1,38.8,25.4), 
                   ed9to11   = c(25.3,29,31,21.1),
                   ed12      = c(18.2,18.8,19.3,20.3),
                   ed13to15  = c(18.3,24.3,15.7,24),
                   ed16      = c(16.3,19,16.8,17.5)
)

df2

dotchart(as.matrix(df2), cex=0.8)
dotchart(t(as.matrix(df2)), cex=0.8)

df2.med = medpolish(df2,maxiter=4) #Kalo ga konvergen, iterasinya dinaikin
df2.med
