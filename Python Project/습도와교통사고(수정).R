
#파일경로 설정
setwd("C:/github/1suk4zo/Python Project/data") 

#필요 패키지들 설치밎 적용

install.packages("ggplot") # 패키지설치 그래프 그리기
install.packages("ggplot2") # 패키지설치 그래프 그리기 
install.packages("dplyr")  # 패키지 설치 조건 행 추출, 함수연결등을 할수 있다.
library(dplyr)#조건 행 추출, 함수연결등을 할수 있다.
library(ggplot2) # 그래프 그리기 


#데이터 불러오기 
data1 <- read.csv("도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv") 
data1 <- data1[-c(2:12)] #자료가공
data2 <- read.csv("17-19 일별 서울 습도,일조,일사량.csv") 
#불러온 데이터 가공 (필요없는 속성 삭제)
data3 <- data2[-c(1,2)] #자료가공
data3 <- data3[-c(3,4)]#자료가공
data4 <- table (data1$발생일) #데이터입력
data6<-cbind(data3,data4) #2개의 데이터 교집
data6 <- data6[-c(3)] #데이터 가공


df<-data6 #데이터 이름 변경
#시각화 하기전 평균.상대습도 범위에 따른 총 사건수 를 구하기 위한 조건문
# 20~90 까지 10단위를 기준으로 한 열을 메인으로 데이터를 나누었다. (시각화를 할시 x축값에 넣을 변수작성을 같이하기위함)
# 습도량 나누기
df2 <- df[df$평균.상대습도...>20&df$평균.상대습도...<=30,] #20이상 30이하 
View(df2)
df3 <- df[df$평균.상대습도...>30&df$평균.상대습도...<=40,] #30이상 40이하
View(df3)
df4 <- df[df$평균.상대습도...>40&df$평균.상대습도...<=50, ]#40이상 50이하
View(df4) 
df4 <- df[df$평균.상대습도...>50&df$평균.상대습도...<=60,] #50이상 60이하
View(df5)
df5 <- df[df$평균.상대습도...>60&df$평균.상대습도...<=70,] #60이상 70이하
View(df6)
df6 <- df[df$평균.상대습도...>70&df$평균.상대습도...<=80,] #70이상 80이하
View(df7)
df7 <- df[df$평균.상대습도...>80&df$평균.상대습도...<=90,] #80이상 90이하

df8 <- df[df$평균.상대습도...>=90&df$평균.상대습도...<=100,]# 32~46줄 평균 상대습도에따른 각각의 평균 빈도수 구하기
View(df6)
#위 조건문으로 예시(습도20이상30이하의 총건수가 열로 나왔기에 구할려는 열뺴고 나머지삭제)
# 일시하고 평균 습도 삭제 평균습도는 위 df2~df8 시리즈에서 값을 구해서 시각화떄 저거 쓰면 됩니다.
#일시, 평균습도 삭제
# (시각화 데이터만들기전 필요한 데이터를제외 전부다 삭제하여 이데이터를 그대로 가져다가 시각화에 쓸 예정)
ac2 <- df2[-c(1,2)] #필요없는 속성 삭제 52~ 58줄
ac3 <- df3[-c(1,2)]
ac4 <- df4[-c(1,2)]
ac5 <- df5[-c(1,2)]
ac6 <- df6[-c(1,2)] 
ac7 <- df7[-c(1,2)]
ac8 <- df8[-c(1,2)] 

View(ac8)
# 1열로 쭉 건수만 있는상태 그걸 다 더하고 평균빈도를 나타내는 mean 함수적용(sum으로하면 숫자가 너무커서 그래프가 별의미 없어보인다.)
#모든사건사고수 mean으로 평균구하기
# df들은 각각 열로 모든 사건수이기에 그걸 평균을 구하는 mean함수로 데이터를 더정교하게 바꿈
b2<-sum(ac2$Freq)  #62~68줄 d
b3<-sum(ac3$Freq)
b4<-sum(ac4$Freq)
b5<-sum(ac5$Freq)
b6<-sum(ac6$Freq)
b7<-sum(ac7$Freq)
b8<-sum(ac8$Freq)   
View(b2)

# 위 데이터 가공한 속성들로 시각화할 데이터 프레임 만들기
Humidity<- c(20~30,30~40,40~50,50~60,60~70,70~80,80~90) #시각화할데이터의 x축 값 데이터프레임만들기
Freq<- c(96,103,105,103,104,104,110) # y축값 데이터프레임
dfc <- data.frame(Humidity = c(20~30,30~40,40~50,50~60,60~70,70~80,80~90), #x축,y축 통합 
                  Freq = c(96,103,105,103,104,104,110))  

# 막대 그래프그리기
ggplot(data,aes(x=Humidity,y=Freq))+ # 막대그래프 버젼으로 시각화 
  geom_bar(stat="identity")
# 다른 시각화 자료에 맞춰서 선그래프로 변경 

Humidity <-c(20,30,40,50,60,70,80)  # 선그래프 버젼 x축 20~30 은 데이터값이 달라져서 안넣어져서 새로 만듬
#선그래프 그리기 
plot(Humidity,                                   #x data
     Freq,                                       #y data
     main="습도와 사건빈도",                  #제목
     type="l",                                 #그래프의 종류 선택(알파벳) Line
     lty=1,                                     #선의 종류(Line Type) 선택
     lwd=1,                                    #선의 굵기 선택
     xlab="Humidity",                          #x축 레이블
     ylab="Freq"                         #y축 레이블
)

# 막대그래프 가 필요 없을시 66줄,68~69줄 72~73줄 삭제 69줄은 선그래프 적용함수라 삭제X 








#월별 봄,여름,가을,겨울 별 그래프 만들기

#일단 사건빈도부터 건들이고 Humidity 쪽에 봄 여름 가을 겨울 넣고 넣고 
#data4가 1월1일 자료부터 12월31일까지 일별로 총사건수가 있음


#날짜 월단위를 추가해야 알수있기에 날짜 열 추가
colnames(data4) = c("date","Freq") #월단위로 바꾸기위해서 date 열이 필요하기에 열이름변경
data4<- rename(data4,"날짜"="Var1") #Var1 이름을 명확하게 알수있게 날짜로 이름변경
#날짜 넣기위한 패키지설치
install.packages('lubridate') #(날짜 시간 데이터 처리하기위한 용도)
library(lubridate)

install.packages("reshape2") # 이 패키지는 옆으로 컬럼이 많은 형태 ( Wide )를 세로로 긴 (Long) 형탤 변경해주고 세로로 긴형태를 옆으로 넣ㅁ게바꿔줌
library(reshape2)

#총사건수,평균습도,날짜정해진데이터에month열 만들고 월단위로 분류하기위한준비
dw <- cbind(data6, month=month(data6$일시)) #month 월단위 1월 2월 3월 열 추가 및 분류


#month 월단위 기준으로 Freq값을 다더해서 월단위 그래프 값준비완료
dw2<-dcast(dw, month ~ . , value.var="Freq", sum) # month 값을 기준으로 Freq 총 사건수양을 다더하기


#그래프에 이름을 넣기위해 열이름 x축값 y축값으로 미리 이름변경
names(dw2)=c("월","총사건수") #이름 변경

# 월단위 사건수량 
ggplot(dw2,aes(x=월,y=총사건수))+  #x축y축 데이터넣기
  geom_bar(stat="identity",fill="gold",colour="black")+ #막대그래프형태,노랑색넣기,겉태두리 검정넣기
  scale_x_continuous(breaks=seq(1,12,1)) #x축 눈금 1~12 넣고 간격 1로 조절
theme(axis.text.x=element_text(colour="blue", size=12,hjust=0,vjust=1))+ #아래 x축 이름크기및 간격조절
  theme(axis.text.y=element_text(size=rel(2),colour="red", #y축 크기조절및 간격조절
 vjust=1))
  
  



#아래는 잘못만든 그래프및 일단 필요없는 변수들










 # y1<-cbind(df2,month=month(x$일시)
 #  
 #                                           b8<-mean(ac8$Freq)  
 #                                           
 #                                           dw2<-mean(dw2$.)
 #                                           View(dw2)
 #   
 #                                           dW3 <- dw2[dw2$month>2&dw2$month<=5,]
 #                                           dw4 <-dw2[dw2$month>5&dw2$month<=8,]
 #                                           dw5 <-dw2[dw2$month>8&dw2$month<=11,]
 #                                           dw6 <-dw2[dw2$month>=1&dw2$month<=2,]
 #                                           dw7 <-dw2[dw2$month>=12,]
 #                                           data6<-cbind(data3,data4) #2개의 데이터 교집
 #                                           dw6[3,] <-c(12,9463)
 #                                           
 #                                           View(dw7) #6하고 6합치기 
 #                                           View(dW3)#봄
 #                                           View(dw4) #여름
 #                                           View(dw5)#가을
 #                                           View(dw6) #겨울
 #                                           colSums(dW3)
 #                                           colSums(dw4)
 #                                           colSums(dw5)
 #                                           colSums(dw6)
 #                                           
 #                                     
 #                                           azx<-c(1,2,3,4,5,6,7,8,9,10,11,12)
 #                                           asz<-c(9165,8053,9104,9503,10048,9608,9707,9747,9757,10215,10073,9463)
 #                                           
 #                                           
 #                                           plot(azx,                                #x data
 #                                                asz,                                    #y data
 #                                                main="월별 사건수",      #제목
 #                                                type="l",                                 #그래프의 종류 선택(알파벳) Line
 #                                                lty=1,                                     #선의 종류(Line Type) 선택
 #                                                lwd=1,                                    #선의 굵기 선택
 #                                                xlab="계절",                       #x축 레이블
 #                                                ylab="총사건양"                 #y축 레이블
 #                                           )
 #                              
 #                                           ggplot(dw2,aes(x=month,y=.,fill=
 #                                                            
 #                                                            
 #                                                            
 #                                                            
 #                                                            
 #                                                            da <- cbind(df2, month=month(df2$일시))
 #                                                          da2<- cbind(df3, month=month(df3$일시))
 #                                                          da3<- cbind(df4, month=month(df4$일시))
 #                                                          da4<- cbind(df5, month=month(df5$일시))
 #                                                          da5<- cbind(df6, month=month(df6$일시))
 #                                                          da6<- cbind(df7, month=month(df7$일시))
 #                                                          da7<- cbind(df8, month=month(df8$일시))
 #                                                          
 #                                                          
 #                                                          dz <-da[da$month>=1&da$month<2,]
 #                                                          dz2<-da[da$month>=2&da$month<3,]
 #                                                          dz3<-da[da$month>=3&da$month<4,]
 #                                                          dz4<-da[da$month>=4&da$month<5,]
 #                                                          dz5<-da[da$month>=5&da$month<6,]
 #                                                          dz6<-da[da$month>=6&da$month<7,]
 #                                                          dz7<-da[da$month>=7&da$month<8,]
 #                                                          dz8<-da[da$month>=8&da$month<9,]
 #                                                          dz9<-da[da$month>=9&da$month<10,]
 #                                                          dz10<-da[da$month>=10&da$month<11,]
 #                                                          dz11<-da[da$month>=11&da$month<12,]
 #                                                          dz12<-da[da$month>=12&da$month<13,]
 #                                                          
 #                                                          c<-sum(dz$Freq) 
 #                                                          c2<-sum(dz2$Freq)
 #                                                          c3<-sum(dz3$Freq)
 #                                                          c4<-sum(dz4$Freq)
 #                                                          c5<-sum(dz5$Freq)
 #                                                          c6<-sum(dz6$Freq)
 #                                                          c7<-sum(dz7$Freq)
 #                                                          c8<-sum(dz8$Freq)
 #                                                          c9<-sum(dz9$Freq)
 #                                                          c10<-sum(dz10$Freq)
 #                                                          c11<-sum(dz11$Freq)
 #                                                          c12<-sum(dz12$Freq)
 #                                                          
 #                                                          
 #                                                          
 #                                                          dx<-da2[da2$month>=1&da2$month<2,]
 #                                                          dx2<-da2[da2$month>=2&da2$month<3,]
 #                                                          dx3<-da2[da2$month>=3&da2$month<4,]
 #                                                          dx4<-da2[da2$month>=4&da2$month<5,]
 #                                                          dx5<-da2[da2$month>=5&da2$month<6,]
 #                                                          dx6<-da2[da2$month>=6&da2$month<7,]
 #                                                          dx7<-da2[da2$month>=7&da2$month<8,]
 #                                                          dx8<-da2[da2$month>=8&da2$month<9,]
 #                                                          dx9<-da2[da2$month>=9&da2$month<10,]
 #                                                          dx10<-da2[da2$month>=10&da2$month<11,]
 #                                                          dx11<-da2[da2$month>=11&da2$month<12,]
 #                                                          dx12<-da2[da2$month>=12&da2$month<13,]
 #                                                          
 #                                                          
 #                                                          dc<-da3[da3$month>=1&da3$month<2,]
 #                                                          dc2<-da3[da3$month>=2&da3$month<3,]
 #                                                          dc3<-da3[da3$month>=3&da3$month<4,]
 #                                                          dc4<-da3[da3$month>=4&da3$month<5,]
 #                                                          dc5<-da3[da3$month>=5&da3$month<6,]
 #                                                          dc6<-da3[da3$month>=6&da3$month<7,]
 #                                                          dc7<-da3[da3$month>=7&da3$month<8,]
 #                                                          dc8<-da3[da3$month>=8&da3$month<9,]
 #                                                          dc9<-da3[da3$month>=9&da3$month<10,]
 #                                                          dc10<-da3[da3$month>=10&da3$month<11,]
 #                                                          dc11<-da3[da3$month>=11&da3$month<12,]
 #                                                          dc12<-da3[da3$month>=12&da2$month<13,]   
 #                                                          
 #                                                          
 #                                                          
 #                                                          dv<-da4[da4$month>=1&da4$month<2,]
 #                                                          dv2<-da4[da4$month>=2&da4$month<3,]
 #                                                          dv3<-da4[da4$month>=3&da4$month<4,]
 #                                                          dv4<-da4[da4$month>=4&da4$month<5,]
 #                                                          dv5<-da4[da4$month>=5&da4$month<6,]
 #                                                          dv6<-da4[da4$month>=6&da4$month<7,]
 #                                                          dv7<-da4[da4$month>=7&da4$month<8,]
 #                                                          dv8<-da4[da4$month>=8&da4$month<9,]
 #                                                          dv9<-da4[da4$month>=9&da4$month<10,]
 #                                                          dv10<-da4[da4$month>=10&da4$month<11,]
 #                                                          dv11<-da4[da4$month>=11&da4$month<12,]
 #                                                          dv12<-da4[da4$month>=12&da4$month<13,]
 #                                                          
 #                                                          
 #                                                          
 #                                                          
 #                                                          dv<-da5[da5$month>=1&da5$month<2,]
 #                                                          dv2<-da5[da5$month>=2&da5$month<3,]
 #                                                          dv3<-da5[da5$month>=3&da5$month<4,]
 #                                                          dv4<-da5[da5$month>=4&da5$month<5,]
 #                                                          dv5<-da5[da5$month>=5&da5$month<6,]
 #                                                          dv6<-da5[da5$month>=6&da5$month<7,]
 #                                                          dv7<-da5[da5$month>=7&da5$month<8,]
 #                                                          dv8<-da5[da5$month>=8&da5$month<9,]
 #                                                          dv9<-da5[da5$month>=9&da5$month<10,]
 #                                                          dv10<-da5[da5$month>=10&da5$month<11,]
 #                                                          dv11<-da5[da5$month>=11&da5$month<12,]
 #                                                          dv12<-da5[da5$month>=12&da5$month<13,]
 #                                                          
 #                                                          
 #                                                          
 #                                                          db<-da6[da6$month>=1&da6$month<2,]
 #                                                          db2<-da6[da6$month>=2&da6$month<3,]
 #                                                          db3<-da6[da6$month>=3&da6$month<4,]
 #                                                          db4<-da6[da6$month>=4&da6$month<5,]
 #                                                          db5<-da6[da6$month>=5&da6$month<6,]
 #                                                          db6<-da6[da6$month>=6&da6$month<7,]
 #                                                          db7<-da6[da6$month>=7&da6$month<8,]
 #                                                          db8<-da6[da6$month>=8&da6$month<9,]
 #                                                          db9<-da6[da6$month>=9&da6$month<10,]
 #                                                          db10<-da6[da6$month>=10&da6$month<11,]
 #                                                          db11<-da6[da6$month>=11&da6$month<12,]
 #                                                          db12<-da6[da6$month>=12&da6$month<13,]
 #                                                          
 #                                                          
 #                                                          dn<-da7[da7$month>=1&da7$month<2,]
 #                                                          dn2<-da7[da7$month>=2&da7$month<3,]
 #                                                          dn3<-da7[da7$month>=3&da7$month<4,]
 #                                                          dn4<-da7[da7$month>=4&da7$month<5,]
 #                                                          dn5<-da7[da7$month>=5&da7$month<6,]
 #                                                          dn6<-da7[da7$month>=6&da7$month<7,]
 #                                                          dn7<-da7[da7$month>=7&da7$month<8,]
 #                                                          dn8<-da7[da7$month>=8&da7$month<9,]
 #                                                          dn9<-da7[da7$month>=9&da7$month<10,]
 #                                                          dn10<-da7[da7$month>=10&da7$month<11,]
 #                                                          dn11<-da7[da7$month>=11&da7$month<12,]
 #                                                          dn12<-da7[da7$month>=12&da7$month<13,]