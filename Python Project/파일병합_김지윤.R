getwd()

setwd("C:/project/1suk4zo/Python Project")
getwd()

crash <- read.csv("도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
str(crash)
View(crash)
crash

weather <- read.csv("2017-2019날씨데이터_1.csv")
str(weather)
View(weather)
weather

library(dplyr)


weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시)
View(weather)

crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일'))

View(crash_weather)

# 복사본 생성(원본 유지)
c_w <- crash_weather
dim(c_w)
str(c_w)
View(c_w)
summary(c_w)



















