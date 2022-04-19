getwd()

setwd("C:/project/1suk4zo/Python Project")
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


weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시)
View(weather)
humidity <- rename(humidity, 발생일=일시)

crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일'))

View(crash_weather)

crash_weather_humidity <- left_join(crash_weather,humidity,by='발생일')

# 복사본 생성(원본 유지)
c_w_h <- crash_weather_humidity
View(c_w_h)
dim(c_w)
str(c_w)
View(c_w)
summary(c_w)

# 평균기온 결측치 제거
c_w$평균기온..C.
c_w <- c_w[!is.na(c_w$평균기온..C.),]
View(c_w)
dim(c_w)
# [1] 112192     15

plot(c_w$평균기온..C., c_w$사고건수)
barplot(c_w$평균기온..C., c_w$사고건수)

str(c_w$발생일)

factor <- factor(c_w$발생일)
c_w$발생일 <- as.Date(c_w$발생일, format = "%Y-%m-%d")
View(c_w)





# # install.packages('tidyverse')
# library('tidyverse')
# 
# # install.packages('lubridate')
# library('lubridate')
# month(c_w$발생일)==1
# 
# winter <- c_w %>% filter(month(발생일)==1 | month(발생일)==2 | month(발생일)==12) %>%
#   group_by(year(발생일))
#   # summarise(total = sum(사고건수))
#   
# View(winter)
# 
# 
# barplot(c_w$평균기온..C., c_w$사고건수)






