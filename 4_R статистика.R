set.seed(4)

samp<-rnorm(50,7,3)

rnor
samp
qqnorm(samp)
qqline(samp)
Z<-qnorm(0.975)
Z
SE<-3/sqrt(50)
SE
lolv<-mean(samp)-Z*SE
uplv<- mean(samp)+Z*SE
CI<-c(lolv,uplv)
CI

library(rafalib)
bigpar(1,3)
set.seed(3) 

options(download.file.method="libcurl")

########
install.packages("rio")
install.packages("rafalib")
library(rio)
library(rafalib)
library(dplyr)

dat<-import("cardio_train.csv")
head(dat)
dat$age/365
trunc(dat$age/365) # отсекает дробную часть

dat<-dat %>% mutate(age_years=(trunc(age/365))) # 
head(dat)

tidy_set<-dat%>% filter ((ap_lo <200 & ap_lo>20)&(ap_hi<300&ap_hi>40))
head(.tidy_set)
dim(tidy_set)
#посмотрим,есть ли у нас стоки, где верхнее давление ниже нижнего

head(tidy_set[tidy_set$ap_hi<tidy_set$ap_lo,])
.tidy_set<- tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
dim(.tidy_set)
dim(tidy_set)
68781-68678 # столько строчек ,где верхнее ниже нижнего
# Оценим седнее диастоническое давление мужчин и женщин при помощи доверительного интервала
# для построения доверительного интервала выбираем критерий,должны выполняться следующие условия
# независимотсь измерений, нормальность распределения
.women<-.tidy_set$ap_lo[.tidy_set$gender==1]
.men<-.tidy_set$ap_lo[.tidy_set$gender==2]

mypar(1,2)
qqnorm(.women,main="женщины")
qqline(.women)


qqnorm(.men, main = "мужчины") # сигма неизвестна, и данные плохо аппрокисмированы норм.распед.
qqline(.men)

#рассчитаем характеристики описательной статистики
infer<-.tidy_set%>% group_by(gender)%>%summarise(
  mu = mean(ap_lo),
  k= qt(0.975, length(ap_lo)-1), # критерий Т, зависящий от степеней свободы
  se= sd(ap_lo)/sqrt(length(ap_lo)),
  lolewel=mean(ap_lo)-k*se,
  hilevel=mean(ap_lo)+k*se)
infer
# CI for women
ci_w<-c(infer[1,5], infer[1,6])

ci_w<-as.numeric(ci_w)
ci_w

ci_m<-as.numeric(c(infer[2,5],infer[2,6]))
ci_m
mypar(1,1)
#Построим на графике рассчитанные интервальные оценки
plot(mean(.women),col=2, lwd=2, 
     xlim=c(0.5,2.5), ylim=c(80,83),
       ylab="среднее диастолическое давление",
        main="интервальная оценка среднего диастолического давления для мужчин и женщин")
     interval=c(80.75,80.92)
     lines(x=c(1,1), y=interval, col="red", lwd=3)
points(1.5, mean(.men), col=3, lwd=2)
interval_1<-c(82.05,82.29)
lines(x=c(1.5,1.5), y=interval_1, col="blue", lwd=3)
      legend("topleft",c("women","men"),fill=c("red","blue"))
       
      