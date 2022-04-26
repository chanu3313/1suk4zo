getwd()

setwd("D:/project1/1suk4zo/Python Project")
getwd()

# 파일 불러오기
crash <- read.csv("data/도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")

weather <- read.csv("data/2017-2019날씨데이터_1.csv")

humidity <- read.csv("data/17-19 일별 서울 습도,일조,일사량.csv")

weather <- as.data.frame(weather)

# 결측치 제거
weather <- weather[weather['지점명']!='관악(레)',]
weather <- weather[weather['지점명']!='남현',]
weather <- weather[weather['지점명']!='기상청',]
weather <- weather[weather['지점명']!='한강',]

# weather(날씨) 데이터 지점, 최저기온, 최고기온 변수 삭제

weather <- weather[-c(1,5,6)]

# humidity(습도) 데이터 지점, 지점명, 일사량 변수 삭제
humidity <- humidity[-c(1,2,6)]


# 날씨데이터, 습도&일사량 데이터 변수명 변경
library(dplyr)

weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시, 평균기온=평균기온..C., 일강수량=일강수량.mm.)

humidity <- rename(humidity, 발생일=일시, 평균상대습도=평균.상대습도..., 합계일조시간=합계.일조시간.hr.)

# 날씨 데이터, 습도 데이터 병합(일자 기준)
weather_humidity <- left_join(weather, humidity, by=c('발생일'))

# 교통사고 데이터

# 발생일, 발생지_시군구, 사고건수 데이터 추출
crash <- crash %>% select(발생일, 발생지_시군구, 사고건수) %>%
  group_by(발생일, 발생지_시군구) %>%
  summarise(total사고건수 = sum(사고건수))

# 날씨&습도 데이터, 교통사고 데이터 병합(구 기준)
crash_weather_humidity <- left_join(crash,weather_humidity,by=c('발생지_시군구','발생일'))
head(crash_weather_humidity)

# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거

write.csv(crash_weather_humidity, file='data/교통사고_날씨_습도.csv')

