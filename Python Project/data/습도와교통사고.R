setwd("C:/github/1suk4zo/Python Project/data")
data <- read.csv("과제.csv")
data1 <- read.csv("도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv") #자료 입력
data2 <- read.csv("서울시 시간대별 교통사고 2017") #자료입력
data1 <- data1[-c(2:12)] #자료가공
data2 <- read.csv("17-19 일별 서울 습도,일조,일사량.csv") #자료 입력
data3 <- data2[-c(1,2)] #자료가공
data3 <- data3[-c(3,4)]#자료가공
data4 <- table (data1$발생일) #데이터입력
data6<-cbind(data3,data4) #2개의 데이터 교집
data6 <- data6[-c(3)] #데이터 가공
install.packages("ggplot") # 패키지설치
install.packages("ggplot2") # 패키지설치
install.packages("dplyr") # 패키지설치
library(dplyr) 
library(ggplot2)

df<-data6 #데이터 이름 변경
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
df8 <- df[df$평균.상대습도...>=90&df$평균.상대습도...<=100,]# 18~33줄 평균 상대습도에따른 각각의 평균 빈도수 구하기
ac2 <- df2[-c(1,2)] 
ac3 <- df3[-c(1,2)]
ac4 <- df4[-c(1,2)]
ac5 <- df5[-c(1,2)]
ac6 <- df6[-c(1,2)]
ac7 <- df7[-c(1,2)]
ac8 <- df8[-c(1,2)] #34줄~40줄 가공된데이터들중 필요없는 열지우기
b2<-mean(ac2$Freq)
b3<-mean(ac3$Freq)
b4<-mean(ac4$Freq)
b5<-mean(ac5$Freq)
b6<-mean(ac6$Freq)
b7<-mean(ac7$Freq)
b8<-mean(ac8$Freq)  # 41줄~47줄 가공된데이터 mean 적용 전체숫자에서 평균값으로
Humidity<- c(20~30,30~40,40~50,50~60,60~70,70~80,80~90) #시각화할데이터의 x축 값 데이터프레임만들기
Freq<- c(96,103,105,103,104,104,110) # y축값 데이터프레임
dfc <- data.frame(Humidity = c(20~30,30~40,40~50,50~60,60~70,70~80,80~90),
                  Freq = c(96,103,105,103,104,104,110))  # x축 y축 값 합치기
ggplot(data,aes(x=Humidity,y=Freq))+ # 막대그래프 버젼으로 시각화 
  geom_bar(stat="identity")
Humidity <-c(20,30,40,50,60,70,80)  # 선그래프 버젼 x축 20~30 은 데이터값이 달라져서 안넣어져서 새로 만듬
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
# 선그래프 시각화

