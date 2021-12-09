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
box_lo<-boxplot(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
title("Нижнее давление")
box_hi<-boxplot(dat$ap_hi[dat$ap_hi<300 & dat$ap_hi>40])
title("верхнее давление")


min(dat$ap_hi[dat$ap_hi<300&dat$ap_hi>40]) #для верхнего давления границы опасные д/жихни 70 до 140
min(dat$ap_hi)
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
sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[c(34497,34498)]

sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34498]

head(sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34497], 20)

#Воспользуемся функцией  filter(), %>%  из пакета dplyr ,чтобы подготовить датасэт без грубейших ошибок ввода

tidy_set <-dat %>% filter((ap_lo<200&ap_lo>20) & (ap_hi<300&ap_hi>40))

head(tidy_set)
#сравним число значений переменных в 1-м и новом датасэте
dim(dat)
dim(tidy_set)
nrow(dat)
ncol(dat)
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

hist(tidy_set$ap_hi, main="верхнее давление.new", xlab = "aphi_tidy", ylab="частота")
hist(dat$ap_hi, main="верхнее давление", xlab = "aphi_dat", ylab="частота")


# QQ-график позволяет проверить данные на нормальность
#в основе лежит идея сравнить теоретические квантили с квантилями случайной величины
mypar(1,2)
qqnorm(tidy_set$ap_lo, main="нижнее давление.tidy")
qqline(tidy_set$ap_lo, col="red", lwd=2)

qqnorm(tidy_set$ap_hi, main="верхнее давление.tidy")
qqline(tidy_set$ap_hi, col="red", lwd=4)
abline(h=160, col="green")
#вывод: по нижнему давлению :верхние и нижние значения лежат слишком 
# высоко и низко соответственно,чем предполагалось нормальным распределением
#по верхнему давлению

#Сравним верхнее и нижнее давление по полу
mypar(1,2)
head(tidy_set)
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
plot(density(tidy_set$ap_lo), col="1", lwd=2, main="нижнее давление")
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
legend("bottomright",c("ap_lo","ap_hi"), col=c(1,3), lty = c(1,2))
#скаттерплот
plot(tidy_set$ap_hi,tidy_set$ap_lo)

#покажем разделение на группы

plot(tidy_set$ap_hi,tidy_set$ap_lo, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "верхнее давление", ylab= "нижнее давление")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.1)
# фомулируем вывод

plot(tidy_set$weight,tidy_set$height, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "вес", ylab= "рост")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.1)
cor()

# фомулируем вывод
head(tidy_set)
miniset<-tidy_set[,3:5]
head(miniset)
nrow(miniset)
plot(miniset,pch=21, bg=miniset$gender)
plot(miniset$gender,miniset$weight, pch=21, bg=miniset$gender)

colnames(tidy_set)
mini_set1<-tidy_set[,c(3,6,7)]
head(mini_set1)
plot(mini_set1,pch=21, bg=miniset$gender)

########
plot(miniset$gender,miniset$height,pch=21,bg=miniset$gender)
######The End


