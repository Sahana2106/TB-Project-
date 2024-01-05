library(readxl)
x <- read_excel("C:/Users/Lenovo/OneDrive/Desktop/finals TB/Data/tb data for time series.xls", 
                sheet = "Srilanka")
View(x)

X=x$e_mort_num
xt=ts(X,start =2000,end=2020)
xt
library(forecast)
ggtsdisplay(xt)
m=auto.arima(xt)
m
f=summary(forecast(m,h=5))
f
a=forecast(m,h=5)
a
f1=data.frame(a)
f1[1]
#Testing trend
library(trend)
mk.test(xt)

mod1=auto.arima(xt)
refit=Arima(xt,model = mod1)
accuracy(refit)


#####################

#Cluster Analysis:

library(readxl)
D1 <- read_excel("C:/Users/Lenovo/OneDrive/Desktop/finals TB/Data/tb data for time series.xls", 
                 sheet = "incidence")
View(D1)
D=subset(D1,INC=="e_inc_num_100k")
d=dist(D,method = "euclidean") 
fit <- hclust(d, method="ward.D") 
c=cutree(fit,3)
N=rbind(D$`2000`,D$`2001`,D$`2002`,D$`2003`,D$`2004`,D$`2005`,D$`2006`,D$`2007`,D$`2008`,D$`2009`,D$`2010`,D$`2011`,D$`2012`,D$`2013`,D$`2014`,D$`2015`,D$`2016`,D$`2017`,D$`2018`,D$`2019`,D$`2020`)
N=data.frame(N)
t=c(rep(1,21),rep(1,21),rep(2,21),rep(1,21))
x=rep(1:21,1)
t1=1:21
N=cbind(t1,N)
M=data.frame(N,t,x)
library(ggplot2)
ggplot(M,aes(x=x,y=N)+geom_line(col=t))
#ggplot(M,aes(x=x,y=N))+geom_line(col=t)
ggplot(N,aes(t1))+geom_line(aes(y=X1,colour="red"))+geom_line(aes(y=X2,colour="red"))+geom_line(aes(y=X3,colour="green"))+geom_line(aes(y=X4,colour="blue"))+xlab("Time Index")+ylab("Anual incidence Rate")




library(readxl)
x <- read_excel("C:/Users/Lenovo/OneDrive/Desktop/finals TB/Data/tb data for time series.xls", 
                sheet = "Srilanka")
View(x)
label=("Incidence number")
boxplot(x$e_inc_num,xlab = label)

label1=("Incidence number of TB and HIV")
boxplot(x$e_inc_tbhiv_num,xlab = label1)

label2=("mortality number TB and HIV")
boxplot(x$e_mort_tbhiv_num,xlab = label2)

label3=("mortality number of TB excluding HIV")
boxplot(x$e_mort_exc_tbhiv_num,xlab = label3)

label4=("mortality number")
boxplot(x$e_mort_num,xlab=label4)




