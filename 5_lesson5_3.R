
.tidy_set<-tidy_set[tidy_set$ap_hi>tidy_set$ap_lo,]
head(.tidy_set)
set.seed(1)

s1.g1<-sample(.tidy_set$ap_hi[.tidy_set$gluc==1&.tidy_set$gender==1],20)

s2.g1<-sample(.tidy_set$ap_hi[.tidy_set$gluc==1&.tidy_set$gender==2],20)
s2.g1


s1.g2<- sample(.tidy_set$ap_hi[.tidy_set$gluc==2&.tidy_set$gender==1],20)
s2.g2<- sample(.tidy_set$ap_hi[.tidy_set$gluc==2&.tidy_set$gender==2],20)


s1.g3<- sample(.tidy_set$ap_hi[.tidy_set$gluc==3&.tidy_set$gender==1],20)
s2.g3<-sample(.tidy_set$ap_hi[.tidy_set$gluc==3&.tidy_set$gender==2],20)

# новый вектор "gender.new" и "gluc.new"
gender.new<-c(rep(1,20),rep(2,20),rep(1,20),rep(2,20),rep(1,20),rep(2,20))
gender.new
gluc.new<-c(rep(1,40),rep(2,40),rep(3,40))

sam_s<-c(s1.g1,s2.g1,s1.g2,s2.g2,s1.g3,s2.g3)

anovaframe<- data.frame(sam_s,gender.new,gluc.new) # соблюдаются случайность и независимость
head(anovaframe,25)
table(anovaframe$gender.new,anovaframe$gluc.new)

# разведочный анализ
#Графическое представление данных
mypar(1,1)
boxplot(sam_s ~ gender.new, data = anovaframe,
        boxwex = 0.15, at = 1:2-0.3,
        subset = gluc.new == "1", col = "5",
        main = "EDA ANOVA",
        xlab = "пол",
        ylab = "верхнее давление",
        xlim = c(0.5, 2.5), ylim = c(0, 200))
boxplot(sam_s ~ gender.new, data = anovaframe, add = TRUE,
        boxwex = 0.15, at = 1:2-0.1 ,
        subset = gluc.new == "2", col = "2")

boxplot(sam_s ~ gender.new, data = anovaframe, add = TRUE,
        boxwex = 0.15, at= 1:2 +0.1,
        subset = gluc.new == "3", col = "brown")

legend("bottomleft", c("gluc=1", "glic=2", "gluc=3"),
       fill = c("5", "2","brown"))






mypar(2,3)


cochran.test

qqnorm(s1.g1)
qqline(s1.g1)

qqnorm(s1.g2)
qqline(s1.g2)


qqnorm(s1.g3)
qqline(s1.g3)

qqnorm(s2.g1)
qqline(s2.g1)

qqnorm(s2.g2)
qqline(s2.g2)

qqnorm(s2.g3)
qqline(s2.g3)

?bartlett.test()

bartlett.test(list(s1.g1,s2.g1,s1.g2,s2.g2,s1.g3,s2.g3))
# сбалансированные данные не влияют на порядок включения факторов в модель:
summary(aov(sam_s~gender.new+gluc.new+gender.new:gluc.new, data= anovaframe))
summary(aov(sam_s~gluc.new+gender.new+gluc.new:gender.new, data= anovaframe))

summary(aov(sam_s~gender.new*gluc.new, data= anovaframe))

a<-c(-3,-2,-1,0,1,2,3)
b<-a^2
cor(a,b)
summary(lm(b~a))

summary(glm(b~a^2))



## урок  3

# нам понадобится загрузить следующие библиотеки,
# если не установлены ранее, то сначала загружаем пакеты с помощью install.packages("имя пакета")
library(rio)
library(dplyr)
library(rafalib)
# загружаем датасэт в R с помощью import() из пакета rio
dat<-import("cardio_train.csv")
head(dat)
# также получим дополнительные сведения о наборе данных с помощью dim() и str()
dim(dat)

str(dat)

#В столбце age  возраст представлен в днях, что не привычно. 
#Поэтому, для простоты интерпретации, добавим новый столбец, в котором возраст пациента будет в годах
?mutate
dat$age/365
trunc(dat$age/365) # отсекает дробную часть
dat<-dat %>% mutate(age_years=(trunc(age/365))) # ф-ция  trunc() отсекает дробную часть

#добавили столбец "age_years"
head(dat)
#В этом датасэте будем исследовать две переменные из  набора
#«Cardiovascular Disease»: нижние и верхнее давление пациента
# Мы имеем 70 000 измерений для каждой величины и ожидаем увидеть,
#что они следуют нормальному распределению
mypar(1,2)
hist(dat$ap_hi)
hist(dat$ap_lo) # график показал нам то, чего мы не ожидали увидеть!
# также представление о наших данных могут дать нам седнее арифметическое и станд.отклонение
# особенно ценны эти метрики были бы,если бы распределение следовало нормальному, что не соблюдается
mean(dat$ap_hi)
sd(dat$ap_hi)

mean(dat$ap_lo)
sd(dat$ap_lo) # мы наблюдаем большое стандартное отклонение,следовательно,и  большую дисперсию

# boxplot является одним из самых эффективных методов представления данных
#позвлит взглянуть на все значения случайной величины

box_lo<-boxplot(dat$ap_lo)
box_hi<-boxplot(dat$ap_hi) # видим выбросы, которые влияют на sd и mean
# ПРИНИМАЕМ РЕШЕНИЕ О ТОМ,ЧТО ДЕЛАТЬ С ВЫБРОСАМИ


# Построим боксплоты без учета выбросов,которые явно были ошибкой ввода
box_lo<-boxplot(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])
title("Нижнее давление")
box_hi<-boxplot(dat$ap_hi[dat$ap_hi<300&dat$ap_hi>40])
title("верхнее давление")


min(dat$ap_hi[dat$ap_hi<300&dat$ap_hi>40]) #для верхнего давления границы опасные д/жихни 70 до 140
# можно и более глубокое исследование по выбросам провести
#например, у нас были данные с отрицательным знаком, которые просто не вошли в наш интервал
dat$ap_hi[dat$ap_hi<0]

# Научимся понимать информацию, которую несет боксплот
#Как строится
# какие границы и что означают

median(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])
quantile(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20],0.25)

# как получаем квартиль
sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])
length(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])

68994*25/100
ind_p25<-trunc(68994*25/100)+1
ind_p25

sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[ind_p25]

#Разберемся, почему 1й квартиль совпадает с медианой
68994/2
sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34497]
tail(sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34497], 20)

#Воспользуемся функцией  filter(), %>%  из пакета dplyr ,чтобы подготовить датасэт без грубейших ошибок ввода

tidy_set <-dat %>% filter((ap_lo<200&ap_lo>20)&(ap_hi<300&ap_hi>40))
head(tidy_set)
#сравним число значений переменных в 1-м и новом датасэте
dim(tidy_set)
nrow(dat)
#сравним старые и новые стандартные отклонения и средние арифметические
# верхнее давление
mean(tidy_set$ap_hi)
mean(dat$ap_hi)
sd(tidy_set$ap_hi)
sd(dat$ap_hi)
#нижнее давление
mean(tidy_set$ap_lo)
mean(dat$ap_lo)
sd(tidy_set$ap_lo)
sd(dat$ap_lo)

#после небольшой обработки данных взглянем на данные с помощью гистограммы
mypar(2,2)

hist(dat$ap_lo, main="нижнее давление", xlab = "aplo_dat", ylab="частота")
hist(tidy_set$ap_lo, main="нижнее давление.new", xlab = "aplo_tidy", ylab="частота")

hist(dat$ap_hi, main="верхнее давление", xlab = "aphi_dat", ylab="частота")
hist(tidy_set$ap_hi, main="верхнее давление.new", xlab = "aphi_tidy", ylab="частота")

# QQ-график позволяет проверить данные на нормальность
#в основе лежит идея сравнить теоретические квантили с квантилями случайной величины
qqnorm(tidy_set$ap_lo, main="нижнее давление.tidy")
qqline(tidy_set$ap_lo, col="red", lwd=2)

qqnorm(tidy_set$ap_hi, main="верхнее давление.tidy")
qqline(tidy_set$ap_hi, col="red", lwd=2)
abline(h=160, col="green")
#вывод: по нижнему давлению :верхние и нижние значения лежат слишком 
# высоко и низко соответственно,чем предполагалось нормальным распределением
#по верхнему давлению

#Сравним верхнее и нижнее давление по полу
mypar(1,2)
groupss_lo<-split(tidy_set$ap_lo,tidy_set$gender)
str(groupss_lo)
boxplot(groupss_lo)
title("нижнее давление")

groupss_hi<-split(tidy_set$ap_hi,tidy_set$gender)
str(groupss_hi)
boxplot(groupss_hi)
title("верхнее давление")

#вернемся к исследованию величин "верхнее давление" и "нижнее давление" без
#учета пола пациента

plot(density(tidy_set$ap_lo), col=1, lwd=2, main="нижнее давление")
plot(density(tidy_set$ap_hi), col=3, lwd=2, main="верхнее давление")

#Разберемся в причинах пиков
mypar(1,1)
plot(density(tidy_set$ap_lo), col=1, lwd=2, main="нижнее давление")
abline(v=70,col="red")
abline(v=60,col="red")
abline(v=80,col="red")
sort_lo<-sort(tidy_set$ap_lo)
cut_1<-sort_lo[sort_lo>65&sort_lo<75]
cut_1
tail(cut_1,20)

#как изобразить нормальное распределение?
plot(density(tidy_set$ap_lo, adjust = 10),col=1, lwd=2, main="нижнее давление")

#два графика на одном
plot(density(tidy_set$ap_lo, adjust = 10),col=1, lwd=2, main="нижнее давление")
lines(density(tidy_set$ap_hi,adjust = 10),col=3, lwd=2, lty=2)
legend("topright",c("ap_lo","ap_hi"), col=c(1,3), lty = c(1,2))

#скаттерплот
plot(tidy_set$ap_hi,tidy_set$ap_lo)

#покажем разделение на группы
plot(tidy_set$ap_hi,tidy_set$ap_lo, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "верхнее давление", ylab= "нижнее давление")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.5)
# фомулируем вывод

plot(tidy_set$weight,tidy_set$height, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "вес", ylab= "рост")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.5)
# фомулируем вывод

miniset<-tidy_set[,3:5]
head(miniset)

plot(miniset,pch=21, bg=miniset$gender)

mini_set1<-tidy_set[,c(3,6,7)]
head(mini_set1)
plot(mini_set1,pch=21, bg=miniset$gender)

########
plot(miniset$gender,miniset$height,pch=21,bg=miniset$gender)





