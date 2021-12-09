#options(download.file.method="libcurl")

#install.packages("cluster")
library(cluster)
library(dplyr)


data()
?votes.repub
head(votes.repub)
str(votes.repub)


dim(votes.repub)

votes.repub[1:6,1:10]

colMeans(votes.repub)

colMeans(votes.repub, na.rm=TRUE)

# в 30-м столбце нет пропущенных значений

votes.repub[,30]

if(mean(votes.repub[,30])>60){
  print("республиканцы набрали высокий процент голосов")
}else{
  print("Республиканцы набрали менее 60% голосов")
}


# в 7-м столбце есть пропущенные значения

if(mean(votes.repub[,7], na.rm = TRUE)>60){
  print("республиканцы набрали высокий поцент голосов")
} else{
  print("Республиканцы набрали менее 60% голосов")
}

?ifelse()

x<-c(3,0,0,0,1,0)
x
ifelse(x!=0,"Yes","No")

#В том случае ,если среднее значение доли голосов, отданных за республиканцев,
#по штатам более 60, будет выводиться сообщение : "Республиканцы набрали менее 60% голосов",
#в противном случае "Республиканцы набрали менее 60% голосов"

ifelse(colMeans(votes.repub,na.rm = TRUE)>60, 
       "республиканцы набрали высокий процент голосов","Республиканцы набрали менее 60% голосов")

# Поставим перед собой задачу написать функцию,которая бы считала, на какую сумму продано мячей за определенный день
#Здесь же пзнакомимся с пакетом lubridate, предназначенного для работы с датами
# Воспользуемся набором «dat» ,содержащего данные о продажах в первые дни 2018 года 
# НЕ ЗАБУДЬТЕ УСТАНОВИТЬ РАБОЧУЮ ДИРЕКТОРИЮ,ГДЕ ЛЕЖИТ НУЖНЫЙ ФАЙЛ sESSION->SET WORKING DIRECTORY->CHOOSE DIRECTORY

getwd()

dat<-read.csv("datn.csv")
#X -  номер покупки
# d.date – год-месяц-день
#ball – теннисные мячи продавались по одному, а также распакованными по 3 и 5 мячей
#price – цены
head(dat)
dim(dat)
str(dat)
dat$d.date
levels(dat$d.date)
unique(dat$ball)
unique(dat$price)
dim(dat)
datn<-dat[,-1]
datn
head(datn,10)
str(datn)
unique(datn$ball)
unique(dat$price)

#install.packages("lubridate")
library(lubridate)
?lubridate
class(datn$d.date)
datn$d.date

dayn<-ymd(datn$d.date)
class(dayn)
dayn
year(dayn)
month(dayn)
day(dayn)

datn$price
datn$price[day(ymd(datn$d.date))==1]
sum(datn$price[day(ymd(datn$d.date))==1])

f.1<-function(d){
  sum(datn$price[day(ymd(datn$d.date))==d])
}

f.1(3)

f.1<-function(d){
  t<-sum(datn$price[day(ymd(datn$d.date))==d]) #здесь необходимо добавить return(t)
  return(t)  
  }
  
f.1(3)

sum(f.1(1),f.1(2),f.1(3))
sum(datn$price)

#напишем функцию, которая бы суммировала продажи, но тут будут еще указан месяц

f.2<-function(m,d){sum(datn$price[month(ymd(datn$d.date))==m & day(ymd(datn$d.date))==d])}
f.2(1,1)
f.2(1,3)
#второго месяца в наборе данных нет
f.2(2,1)

# Посчитаем прибыль магазина по дням. Для этого понадобится цикл for
f.1(1)
f.1(2)

for(i in 1:3){
  print(f.1(i))
}
# рассмотрим функции lapply() , sapply()

head(votes.repub)
?apply
class(votes.repub)

(apply(votes.repub,1,mean))

?lapply
lapply(votes.repub,sum)

class(unlist(lapply(votes.repub,sum)))
as.numeric(unlist(lapply(votes.repub,sum)))

sapply(votes.repub,sum)

class(sapply(votes.repub,sum))


## функция tapply() пименяет нужную функцию к группам
#Как она pаботает будет показано на примере дата сэта "Orange"
data()
head(Orange,12) # окружность измерялась в одинаковом возрасте
?Orange
#Набор содержит три переменные:
# - номер дерева;
# - возраст дерева с 31.12.1968, в днях;
# - длина окружности ствола.

#Напишем функцию, которая будет считать на сколько изменилась окужность дерева с 
# первой даты замера(118-ый день) до последней даты (1582 день)
# прежде, введем функции range(), diff()
a<- c(1,2,3,5,8)

diff(a)

range(a)
min(a)
max(a)
diff(a)
diff(range(a))
l<-function(x){diff(range(x))} 
l(a)
?tapply

tapply(Orange$circumference,Orange$Tree,l) # 1,2 аргументы функции должны быть одинаковой длины,
                                           #1-й -это то,что разбиваем по подгруппа,2-й- фактор
###
search()
install.packages("effectsize")



z<-c(1,2,3,4)
v<-c(0,82,2,8)
z%in%v

#Сохраняем набор данных в формате csv

a<-c(1,2,3)
b<-c(0,0,0)
nm<-paste0(seq(1,3),"_","row")
nm
df.1<-data.frame(a,b, row.names = nm)
df.1
write.csv(df.1,file = "november.csv")
getwd()
df<-read.csv2("november.csv")
class(df)
read.csv("cardio_train.csv")
head(read.csv("cardio_train.csv"))
head(read.csv("cardio_train.csv",sep=";"))
read.csv("cardio_train.csv",sep=";",nrow=5,row.names = paste0("row",seq(1,5))) #сколько строк хотим прочитатьб задать имя строки
read.csv("cardio_train.csv",sep=";",nrow=5,row.names = paste0("row",seq(1,5)))# в качестве имен строк можно и столбец вязть
read.csv("cardio_train.csv",sep=";",nrow=5,row.names = paste0("row",seq(1,5)), header= FALSE)
# Загрузим пакет для чтения данных в фомате excel

install.packages("readxls")
library(readxl)
read_xls("book_1.xls")

library(rio)
?rio
#Сравним базовые функции и функции пакета "рио"
head(import("cardio_train.csv"))
import("book_1.xls")
read.csv("book_1.xls") # ошибка!  эта функция не прочитает другой формат
