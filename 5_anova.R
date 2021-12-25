########
library(rio)
library(rafalib)
library(dplyr)
dat<-import("cardio_train.csv")
dat$age/365
trunc(dat$age/365) # отсекает дробную часть
dat<-dat %>% mutate(age_years=(trunc(age/365))) # 
head(dat)
tidy_set<-dat%>% filter ((ap_lo <200 & ap_lo>20)&(ap_hi<300&ap_hi>40))
head(tidy_set)
#посмотрим,есть ли у нас стоки, где верхнее давление ниже нижнего
head(tidy_set[tidy_set$ap_hi<tidy_set$ap_lo,])
.tidy_set<- tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
dim(.tidy_set)
dim(tidy_set)
#С ПОМОЩЬЮ ЛИН.ЕГРЕССИИ ОПИШЕМ ЗАВИСИМОСТЬ У ОТ Х
set.seed(1)
ind<-sample(seq(1,nrow(.tidy_set)),100)
ts<-.tidy_set[ind,]
head(ts)
dim(ts)
#ПАРНАЯ ЛИНЕЙНАЯ РЕГРЕССИЯ
plot(ts$ap_lo,ts$ap_hi, xlim = c(0,120), ylim=c(0,200),col="red", lwd=2)
fitm<-lm(ts$ap_hi~ts$ap_lo) #построим парную линейную регрессию, в качестве независимой переменной возьмем нижнее давление пациента
fitm

hi_hat<-  16.513 +  1.347*ts$ap_lo
hi_hat    

predict(fitm,factor(ts$ap_lo))
as.numeric( predict(fitm, ts) ) 
? predict
fitm$coefficients
fitm
#сравним
16.512768+  1.347269 *ts$ap_lo
as.numeric(predict(fitm,factor(ts$ap_lo)))

mypar(1,1)
#сравним истинное с предсказанным значнием
plot(seq(1:length(ts$ap_hi)),ts$ap_hi,col="red",type = "l")
lines(seq(1,length(hi_hat)), hi_hat, col="blue",type="l")
legend("topleft",c("ts$ap_hi","hi_hat"),col=c(2,3), lty = c(1,1))

summary(fitm)
qqnorm(fitm$residuals)
qqline(fitm$residuals)
fitm$coefficients
rse<-sqrt(sum(residuals(fitm)^2)/fitm$df.residual) #df=k-n-1, n=1: 1 predictor, rse- resid. stand. er
rse
summary(fitm)



