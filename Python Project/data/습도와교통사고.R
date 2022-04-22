
getwd()

setwd("C:/github/1suk4zo/Python Project/data")
getwd()
data <- read.csv("과제.csv")
data1 <- read.csv("도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
data2 <- read.csv("서울시 시간대별 교통사고 2017")
dat <- read.csv("서울시 시간대별 교통사고 2017")
str(data1)

data1
data1 <- data1[-c(2:12)]

data2 <- read.csv("17-19 일별 서울 습도,일조,일사량.csv")

data3 <- data2[-c(1,2)]

data3 <- data3[-c(3,4)]


data1

data4 <- table (data1$발생일)

data6<-cbind(data3,data4)
data6 <- data6[-c(3)]
data5<-merge(data4,data3)
View(data5)
data5 <- data5[-c(1)]
View(data5)
data5<-data5[c(2,3,1)]
View(data5)
install.packages("ggplot")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
data6
df<-data6
plot(df$Freq ~ df$평균.상대습도...)
plot(df$Freq ~ df$평균.상대습도...)
plot(df2)
df2 <- df[df$평균.상대습도...>20&df$평균.상대습도...<=30,]
View(df2)
df3 <- df[df$평균.상대습도...>30&df$평균.상대습도...<=40,]
View(df3)
df4 <- df[df$평균.상대습도...>40&df$평균.상대습도...<=50,]
View(df4)
df4 <- df[df$평균.상대습도...>50&df$평균.상대습도...<=60,]
View(df5)
df5 <- df[df$평균.상대습도...>60&df$평균.상대습도...<=70,]
View(df6)
df6 <- df[df$평균.상대습도...>70&df$평균.상대습도...<=80,]
View(df7)
df7 <- df[df$평균.상대습도...>80&df$평균.상대습도...<=90,]
View(df8)
df8 <- df[df$평균.상대습도...>=90&df$평균.상대습도...<=100,]
ac2 <- df2[-c(1,2)]
ac3 <- df3[-c(1,2)]
ac4 <- df4[-c(1,2)]
ac5 <- df5[-c(1,2)]
ac6 <- df6[-c(1,2)]
ac7 <- df7[-c(1,2)]
ac8 <- df8[-c(1,2)]
b2<-mean(ac2$Freq)
b3<-mean(ac3$Freq)
b4<-mean(ac4$Freq)
b5<-mean(ac5$Freq)
b6<-mean(ac6$Freq)
b7<-mean(ac7$Freq)
b8<-mean(ac8$Freq)
Humidity<- c(20~30,30~40,40~50,50~60,60~70,70~80,80~90)
View(Humidity2)
Freq<- c(96,103,105,103,104,104,110)
Humidity<-c(20~30,30~40,40~50,50~60,60~70,70~80,80~90)
Freq<-c(96,103,105,103,104,104,110)
Freq
dfc <- data.frame(Humidity = c(20~30,30~40,40~50,50~60,60~70,70~80,80~90),
                  Freq = c(96,103,105,103,104,104,110))



View(data)
ggplot(data,aes(x=Humidity,y=Freq))+
  geom_bar(stat="identity")

plot(x)


Humidity <-c(20,30,40,50,60,70,80)
Freq <- c(96,103,105,103,104,104,110)
plot(Humidity,                                   #x data
      Freq,                                       #y data
     main="습도와 사건빈도",                  #제목
     type="l",                                 #그래프의 종류 선택(알파벳) Line
     lty=1,                                     #선의 종류(Line Type) 선택
     lwd=1,                                    #선의 굵기 선택
     xlab="Humidity",                          #x축 레이블
     ylab="Freq"                         #y축 레이블
)


