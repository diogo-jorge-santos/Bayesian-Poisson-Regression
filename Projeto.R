library(corrplot)
library(ggplot2)
library(gridExtra)
library(rstan)
library(psych)
library("bayesplot")
library("loo")


setwd("C:/Users/LENOVO/Desktop/EC Estatística Computacional/Projeto")

data<-read.table("EC-Trab5-1Sem2021.txt")

data<-data[,c(-1,-2)]
head(data)
colnames(data)<-c("Bird","Equine","Farms","Area","Population","Human_density","Bird_rate","Equine_rate")
head(data)
#analise dos dados

describe(data)

par(mfrow=c(4,2))
boxplot(data$Equine,horizontal = TRUE,main="Equine")
boxplot(data$Equine_rate,horizontal = TRUE,main="Equine_rate")

boxplot(data$Bird,horizontal = TRUE,main="Bird")
boxplot(data$Bird_rate,horizontal = TRUE,main="Bird_rate")


boxplot(data$Farms,horizontal = TRUE,main="Farms")
boxplot(data$Human_density,horizontal = TRUE,main="Area")

boxplot(data$Human_density,horizontal = TRUE,main="Human_density")
boxplot(data$Population,horizontal = TRUE,main="Population")

par(mfrow=c(4,2))
hist(data$Equine,main="Equine")
hist(data$Equine_rate,main="Equine_rate")

hist(data$Bird,main="Bird")
hist(data$Bird_rate,main="Bird_rate")

hist(data$Farms,main="Farms")
hist(data$Human_density,main="Area")

hist(data$Human_density,main="Human_density")
hist(data$Population,main="Population")

par(mfrow=c(1,1))
corrplot(cor(data),col=colorRampPalette(c("white","grey","black"))(100))

a<-ggplot(data=data,aes(x=Human_density,y=Population))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

b<-ggplot(data=data,aes(x=Human_density,y=Area))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

c<-ggplot(data=data,aes(x=Farms,y=Area))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

grid.arrange(a,b,c,ncol=3)


e<-ggplot(data=data,aes(x=Human_density,y=Equine_rate))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

f<-ggplot(data=data,aes(x=Bird_rate,y=Equine_rate))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

grid.arrange(e,f,ncol=2)


i<-ggplot(data=data,aes(y=Equine,x=Bird))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

j<-ggplot(data=data,aes(y=Equine,x=Farms))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

k<-ggplot(data=data,aes(y=Equine,x=Population))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

l<-ggplot(data=data,aes(y=Equine,x=Area))+geom_point(color="blue")+geom_smooth(method="lm",se=FALSE)

grid.arrange(i,j,k,l,ncol=4)

head(data[,c(6,7)])

#estimação do modelo
datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(6,7)],offset=log(data$Farms),p=2)
fit1=stan(file = "model_inf.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=as.matrix(data[,c(6)]),offset=log(data$Farms),p=1)
fit2=stan(file = "model_inf.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=as.matrix(data[,c(7)]),offset=log(data$Farms),p=1)
fit3=stan(file = "model_inf.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")



waic(extract_log_lik(fit1))
waic(extract_log_lik(fit2))
waic(extract_log_lik(fit3))
#fit1 é o mehlor modelo

head(data[,c(1,3,4,5)])
datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(1,3,4,5)],p=4)
fit11=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=11),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(3,4,5)],offset=log(data$Farms),p=3)
fit12=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(3,1,5)],offset=log(data$Farms),p=3)
fit13=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(3,1,4)],offset=log(data$Farms),p=3)
fit14=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(3,4,5)],offset=log(data$Farms),p=3)
fit15=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")


waic(extract_log_lik(fit11))
waic(extract_log_lik(fit12))
waic(extract_log_lik(fit13))
waic(extract_log_lik(fit14))
waic(extract_log_lik(fit15))

#fit14 é o melhor modelo

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(1,4)],offset=log(data$Farms),p=2)
fit16=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(3,4)],offset=log(data$Farms),p=2)
fit17=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

datastan=list(N=length(data[,1]),y=data$Equine,x=data[,c(4,5)],offset=log(data$Farms),p=2)
fit18=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

waic(extract_log_lik(fit14))
waic(extract_log_lik(fit16))
waic(extract_log_lik(fit17))
waic(extract_log_lik(fit18))

#fit16 é o melhor modelo 

datastan=list(N=length(data[,1]),y=data$Equine,x=as.matrix(data[,c(1)]),offset=log(data$Farms),p=1)
fit19=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")
datastan=list(N=length(data[,1]),y=data$Equine,x=as.matrix(data[,c(4)]),offset=log(data$Farms),p=1)
fit20=stan(file = "model_noff.stan",data = datastan,iter=5000,control = list(adapt_delta = 0.99, max_treedepth=10),init="0")

waic(extract_log_lik(fit16))
waic(extract_log_lik(fit19))
waic(extract_log_lik(fit20))

#fit16 é o melhor o modelo.

#interpretação do modelo

traceplot(fit1, pars = c("beta"), inc_warmup = TRUE)

traceplot(fit16, pars = c("beta"), inc_warmup = TRUE)

print(names(data[,c(6,7)]))
print(fit1, pars = c("beta"),digits_summary = 4)
print(names(data[,c(1,4)]))
print(fit16,pars = c("beta"),digits_summary = 4)

plot(data$Equine_rate,colMeans(fit1_aux$y_pred)/data$Farms)
abline(a=0,b=1)

plot(data$Equine,colMeans(fit16_aux$y_pred))
abline(a=0,b=1)

fit1_aux<-extract(fit1)
fit16_aux<-extract(fit16)

hist(fit1_aux$y_pred[,1]/data$Farms[1])

#P(Y<0.001|X=x_{1})
print(sum(fit1_aux$y_pred[,1]/data$Farms[1]<0.001)/length(fit1_aux$y_pred[,1]))

hist(fit16_aux$y_pred[,1],breaks = 5)

#P(Y==0|X=x_{1})
print(sum(fit16_aux$y_pred[,12]==0)/length(fit16_aux$y_pred[,12]))
