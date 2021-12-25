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


cor(ts$ap_hi,ts$ap_lo, method = "pearson")^2

qqnorm(lm(.tidy_set$ap_hi~.tidy_set$ap_lo)$residuals)
qqline(lm(.tidy_set$ap_hi~.tidy_set$ap_lo)$residuals)
#########
mypar(1,1)
.tidy_set<-tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
head(.tidy_set)
dim(.tidy_set)
set.seed(1)

ind<-sample(seq(1,nrow(.tidy_set)),100)
ind
ts<-.tidy_set[ind,]
head(ts)
dim(ts)
plot(ts$ap_lo,ts$ap_hi, xlim = c(0,120), ylim=c(0,200),col="red", lwd=2)
fitm<-lm(ts$ap_hi~ts$ap_lo) #построим парную линейную регрессию, в качестве независимой переменной возьмем нижнее давление пациента
fitm

hi_hat<- 16.513 +   1.347*ts$ap_lo
hi_hat   

as.numeric(predict(fitm,ts))
signif(fitm$coefficients,10)


plot(seq(1:length(ts$ap_hi)),ts$ap_hi,col="red",type = "l")
lines(seq(1,length(hi_hat)), hi_hat, col="blue",type="l")
legend("topleft",c("ts$ap_hi","hi_hat"),col=c(2,3), lty = c(1,1))
summary(fitm)

qqnorm(fitm$residuals)
qqline(fitm$residuals)

boxplot(fitm$residuals)
as.numeric(sort(fitm$residuals))
sort(ts$ap_hi)
x<-c(1,5,10,15,20,25,30,35)

y<-c(1,10,20,35,45,60,78,100)

cor(x,y)


f<-lm(y~x)
f

summary(f)
yh<- -6.079+2.82*x

rss<-sum((y-yh)^2)
sqrt(1/7*rss)


rse <- sqrt( sum(residuals(fitm)^2) / fitm$df.residual ) 
rse

Rs<-cor(ts$ap_hi,ts$ap_lo)^2
Rs







hi_hat== as.numeric( predict(fitm, ts) ) 
signif(fitm$coefficients,11)
hi_hat.sign<- signif(18.877877  +  1.314786 *ts$ap_lo,11)
hi_hat.sign

hi_hat<-  101.6786792 + 0.3528327 * ts$weight  # for 10 sample size
hi_hat
predict(fitm)
hi_hat-predict.lm(fitm)
hi_hat<-96.744+0.356*ts$weight - 0.01876*ts$height
hi_hat
predict(fitm)
length(hi_hat)
mypar(1,1)
plot(seq(1,10),hi_hat,type="l", col="red")
lines(seq(1,10),ts$ap_hi,col="blue",type="l")
fitm
summary(fitm)
#выше получили очень большую погрешность, тепеь добавим еще предикторов

head(ts)
fitm<-lm(ts$ap_hi~ts$weight+ts$height+ts$age_years)
hi<- 71.95003+0.23829*ts$weight + 0.04353*ts$height+0.55759*ts$age_years
length(hi)
mypar(1,1)
plot(seq(1,100),hi,type="l", col="red")
lines(seq(1,100),ts$ap_hi,col="blue",type="l")
summary(fitm)
ts<-ts[,-c(1,2)]

head(ts)
dim(ts)
fit<-lm(ts$ap_hi~.,data =ts)
fit
summary(fit)
hi<- 10.25216+0.08300*ts$height -0.06861*ts$weight+ 1.23022*ts$ap_lo-1.12262*ts$cholesterol +0.89636*ts$gluc +0.98949*ts$smoke-4.82969*ts$alco+ 1.76239*ts$active+8.54671*ts$cardio+0.04312*ts$age_years
plot(seq(1,100),hi,type="l", col="red")
lines(seq(1,100),ts$ap_hi,col="blue",type="l")
hi
?predict
as.numeric(predict(fit,ts[,-3]))
identical(hi,as.numeric(predict(fit,ts)))
fit
lm(ts$ap_hi~ts$ap_lo)
library(caret)
tr<-train(ap_hi~ap_lo,data=ts,method="glm")
tr$finalModel
###
predict.lm(lm(ts$ap_hi~ts$ap_lo),data.frame(ts$ap_lo),se.fit = FALSE)
predict(lm(ts$ap_hi~ts$ap_lo),level = 0.95)
y_hat<-18.878+1.315*ts$ap_lo
y_hat

head(ts)
predict(lm(ts$ap_hi~ts$ap_lo),level = 0.95)
y_hat<-18.878+1.315*ts$ap_lo
y_hat

########
set.seed(1)
x <- rnorm(150)
x
y <- x + rnorm(150)
y
lm(y ~ x)

predict(lm(y ~ x))
lm(y ~ x)
y1<-0.04621+0.96320 *x
y1
y<-  0.0551 + 1.0890*x
y
predict(lm(y ~ x), data.frame(x), se.fit = TRUE)$fit

fitmodel<-lm(y~x)
fitmodel
predict(fitmodel)


#сравниваем
head(ts)
y<-ts$ap_hi
x<-ts$ap_lo
fitmodel<-lm(y~x)
predict(fitmodel)
fitmodel
y<-18.878+1.315*x
y


predict(lm(ts$ap_hi~ts$ap_lo))
y_hat<-18.878+1.315*ts$ap_lo
y_hat

predict(lm(ts$ap_hi~ts$ap_lo), data.frame(ts$ap_lo), se.fit = TRUE)$fit





predict(lm(ts$ap_hi~ts$height+ts$weight+ts$ap_lo+ts$cholesterol+ts$gluc+ts$smoke+ts$alco+ts$active+ts$cardio+ts$age_years
             ,data=ts),data.frame(ts[,-3]))
hi

plot(ts$ap_lo,ts$ap_hi, xlim = c(0,120), ylim=c(0,200),col="red", lwd=2)




R.adj<- 1-((1-Rs)*((100-1)/(100-1-1)))
R.adj
           
tsn<-ts[,-c(1,2)]
head(tsn)
fit<-lm(tsn$ap_hi~.,data=tsn)
summary(fit)

fit3<-lm(tsn$ap_hi~tsn$ap_lo+tsn$weight+tsn$cardio)
summary(fit3)


###################
head(.tidy_set)

set.seed(1)
ind<-sample(seq(1,nrow(.tidy_set)),100)
ts<-.tidy_set[ind,]
head(ts)
tsn<-ts[,-c(1,2)]
head(tsn)
fit<-lm(tsn$ap_hi~.,data = tsn)
summary(fit)
fit<-lm(tsn$ap_hi~ ts$ap_lo+tsn$weight+tsn$height)
summary(fit)

mypar(1,1)
##anova 
head(ts)
?anova
stripchart(ap_hi~ gender,data=ts)
ts%>%group_by(gender)%>%summarise(mn=mean(ap_hi),col="red")

boxplot(split(ts$ap_hi,ts$gender))


fitm1<-lm(ts$ap_hi~ts$gender)
anova(fitm1)
unique(ts$alco)
fitm2<-lm(ap_hi~gender)
fitm2
anova(fitm2)
