# Projet Statistiques - Kolmogorov-Smirnov (sur les returns apple)

# on ouvre d abord le fichier csv
apple_df=read.csv2('/Users/macbook/R/Stats_M1/AAPL-2.csv')
apple_df
app_ret=apple_df[,'return']
app_ret
df_ret=data.frame(return=app_ret)
length(app_ret) #1863
app_ret

app_ret2=app_ret[0:1862]

app_ret2

app_ret2[931]

mean(app_ret)

mean(app_ret2)

var(app_ret2)

gg=sd(app_ret2)

gg*gg

0.0002/1862*1762.773


0.00026684*1862/0.0002
0.00026684*1862/0.0003
0.00026684*1862/0.00025


?qchisq
qchisq(0.05,1862)
pchisq(2484.28,1862)
pchisq(1656,187,1862)
pchisq(1987.424,1862)

hist(app_ret,breaks=100)
aaa=hist(app_ret)
aaa


set.seed(254)
k=1:1863
length(k)

df<-data.frame(k,app_ret)
ggplot(df, aes(k,app_ret))+geom_line(size=0.3,colour="darkblue")

order(app_ret)

so=app_ret[order(app_ret)]
so

so[931]
#les statistiques d'ordres

l2=1:1863
for (i in 1:1863)
{
  l2[i]=i/1863
}
l2


df2<-data.frame(so,l2)
ggplot(df, aes(so,l2))+geom_step(colour="darkblue")
# fct de repa empirique


ggplot()+geom_point(data=df,aes(so,l2, colour="red"))+geom_step(data=df,aes(so,l2,colour="darkblue")) +stat_function(fun =pnorm, colour="black")
#???


?pnorm

?stat_function

alph=0.05
zalpha=sqrt(1/2*log(2/alph))
zalpha/10

plotdata <- data.frame(x=so, y=l2, lower = (l2-zalpha/10), upper = (l2+zalpha/10), qdown=qnorm((l2-zalpha/10)), qup=qnorm((l2+zalpha/10)))

pp<-ggplot() + geom_step(data=plotdata,aes(so,l2),colour="darkblue")+geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
pp                       


test <- function(x) {pnorm(x,0,1/40)}

a=75
b=0
droite<-function(x){a*x+b}

ggplot(data.frame(x=c(0,0.14)),aes(x=x))+stat_function(fun=droite)



p4 <- ggplot(data.frame(x = c(0, 0.14)), aes(x = x)) +
  stat_function(fun =test, colour="red") +geom_step(data=plotdata,aes(so,l2),colour="darkblue")+geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
p4


p5 <- ggplot(data.frame(x = c(0, 4)), aes(x = x)) +geom_step(data=plotdata,aes(so,lower),colour="darkblue")+geom_step(data=plotdata,aes(so,upper),colour="darkblue")
p5

ptest<- ggplot(data.frame(x = c(0, 0.14)), aes(x=x)) +geom_step(data=plotdata,aes(so,qdown),colour="darkblue")+geom_step(data=plotdata,aes(so,qup),colour="darkblue")+stat_function(fun =droite, colour="red")
ptest

test_norm<-function(x){pnorm(x,-b/a,1/a)}

ppp<- ggplot(data.frame(x = c(0, 0.14)), aes(x = x))+
  stat_function(fun =test_norm, colour="red") +geom_step(data=plotdata,aes(so,l2),colour="darkblue")+geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
ppp



-b/a
sqrt(1/a)
(1/a)^2

1/0.000266

dens_norm<-function(x){dnorm(x,-b/a,1/a)}


ggplot(df_ret,aes(x=return)) + geom_histogram(aes(y=..density..),colour="black" ,fill="darkseagreen2",bins=100,size=0.3)+geom_vline(xintercept=mean(return)) +stat_function(fun =dens_norm, colour="red")

?geom_histogram

library(plotly)
packageVersion('plotly')
install.packages('plotly')

p <- plot_ly(z = ~volcano) %>% add_surface()
p

?plot_ly
library(plot3d)
