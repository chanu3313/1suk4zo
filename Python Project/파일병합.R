getwd()

setwd("D:/project1/1suk4zo/Python Project")
getwd()

crash <- read.csv("data/도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
str(crash)
View(crash)
crash

weather <- read.csv("data/2017-2019날씨데이터_1.csv")
humidity <- read.csv("data/17-19 일별 서울 습도,일조,일사량.csv")
View(humidity) 
humidity <- humidity[-c(1,2)]
View(humidity)

str(weather)
View(weather)
weather

library(dplyr)
library(readxl)


weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시)
View(weather)
humidity <- rename(humidity, 발생일=일시)

crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일'))

View(crash_weather)

crash_weather_humidity <- left_join(crash_weather,humidity,by='발생일')

# 복사본 생성(원본 유지)
c_w_h <- crash_weather_humidity

# 일별 사고건수_강수량 으로 합치기
acci_sum <- c_w_h %>%
  group_by(발생일)%>%
  summarise(사고건수 =sum(사고건수))
acci_sum

rain <- weather %>%
  group_by(발생일)%>%
  summarise(강수량 =max(일강수량.mm.))
View(rain)

acci_rain <- left_join(acci_sum,rain,by='발생일')

acci_rain
View(acci_rain)

# 결측치 제거
is.na(acci_rain)

table(is.na(acci_rain))

acci_rain <- na.omit(acci_rain)

#####
# 단계 구분도 code 합치기
seoul_map <- read_excel('data/서울_map.xlsx')
seoul1 <- read_excel('data/서울.xlsx') # 서울시 지역구별 코드
View(seoul1)

seoul1 <- rename(seoul1,발생지_시군구=행정구역별_읍면동)
View(seoul1)
seoul_code <- select(seoul1,"발생지_시군구","code")
View(seoul_code)


acci_gu_code <- left_join(acci_gu,seoul_code,by='발생지_시군구')
View(acci_gu_code)










