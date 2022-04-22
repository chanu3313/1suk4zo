getwd()

setwd("C:/project/1suk4zo/Python Project")
getwd()

# 파일 불러오기
crash <- read.csv("data/도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
str(crash)
View(crash)
crash

weather <- read.csv("data/2017-2019날씨데이터_1.csv")
humidity <- read.csv("data/17-19 일별 서울 습도,일조,일사량.csv")
View(humidity)

# 지점, 지점병 열 제거한 변수 생성
humidity <- humidity[-c(1,2)]
View(humidity)

str(weather)
View(weather)
weather

# 데이터셋 변수명 변경
library(dplyr)

weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시)
View(weather)
humidity <- rename(humidity, 발생일=일시)

# 교통사고 데이터, 날씨 데이터 병합
crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일'))
View(crash_weather)

# 교통사고&날씨 데이터, 습도, 일조시간 병합
crash_weather_humidity <- left_join(crash_weather,humidity,by='발생일')

# 복사본 생성(원본 유지)
c_w_h <- crash_weather_humidity
View(c_w_h)
dim(c_w_h)
str(c_w_h)
View(c_w_h)
summary(c_w_h)

# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거
c_w_h_every <- c_w_h[!is.na(c_w_h$평균기온..C.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$일강수량.mm.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일조시간.hr.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일사량.MJ.m2.),]

View(c_w_h_every)
dim(c_w_h_every)
# [1] 110706     18
summary(c_w_h_every)

# 발생일별 사고건수 합계, 평균 기온, 일강수량 합계, 평균상대습도, 일조시간 합계 계산
every <- c_w_h_every %>% group_by(발생일) %>%
  summarise(total사고건수 = sum(사고건수),
            mean평균기온 = mean(평균기온..C.),
            total일강수량 = sum(일강수량.mm.),
            mean평균상대습도 = mean(평균.상대습도...),
            total일조시간 = sum(합계.일조시간.hr.),
            total일사량 = sum(합계.일사량.MJ.m2.))
View(every)
str(every)
dim(every)

# 평균기온 범위
range(every$mean평균기온)

# 발생일 변수(문자열) -> 날짜형 변수로 변경
library('tidyverse')
library('lubridate')
every$발생일 <- as.Date(every$발생일)
month(every$발생일)

# 계절 그룹 생성
every$계절 <- ifelse(month(every$발생일) >=4 & month(every$발생일) <6,
                   "봄",
                   ifelse(month(every$발생일) >=6 & month(every$발생일) < 9, '여름',
                          ifelse(month(every$발생일) >=10 & month(every$발생일) < 11, '가을', '겨울')))

View(every)

# 계절별 사고건수 평균 dataframe 생성
seasons <- every %>% group_by(계절) %>%
  summarise(계절_사고건수평균 = mean(total사고건수)) %>%
  arrange(desc(계절_사고건수평균))

View(seasons)

# 계절 변수를 factor형 변수로 변경
seasons$계절 <- factor(seasons$계절, level=c("겨울","여름","봄","가을"))

library(ggplot2)
library(scales)
# dev.off() # 아래 오류메시지 발생 시 plot 초기화시켜주는 명령어
# Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
#   invalid graphics state

# 계절별 사고건수 합계 시각화
ggplot(seasons, aes(x=계절, y=계절_사고건수합, fill = factor(계절))) + geom_bar(stat='identity') +
  scale_fill_brewer(palette="BuPu")

# 계절별 사고건수 평균 시각화
ggplot(seasons, aes(x=계절, y=계절_사고건수평균, fill=계절_사고건수평균)) + ylim(0,120) +
  geom_bar(stat='identity')

# 평균기온 범위 확인
range(every$mean평균기온)
# [1] -13.81089  33.38017

# 5도씩 기온 분할 후 그룹 생성
every$기온그룹5 <- ifelse(every$mean평균기온 > 30 & every$mean평균기온 <= 35,
                        "30~35도",
                        ifelse(every$mean평균기온 > 25 &every$mean평균기온 <= 30, '25~30도',
                               ifelse(every$mean평균기온 > 20 &every$mean평균기온 <= 25, '20~25도',
                                      ifelse(every$mean평균기온 > 15 &every$mean평균기온 <= 20, '15~20도',
                                             ifelse(every$mean평균기온 > 10 &every$mean평균기온 <= 15, '10~15도',
                                                    ifelse(every$mean평균기온 > 5 &every$mean평균기온 <= 10, '5~10도',
                                                           ifelse(every$mean평균기온 > 0 &every$mean평균기온 <= 5, '0~5도',
                                                                  ifelse(every$mean평균기온 > -5 &every$mean평균기온 <= 0, '-5~0도', ifelse(every$mean평균기온 > -10 &every$mean평균기온 <= -5, '-10~-5도','-15~-10도')))))))))

View(every)

# every$기온그룹 <- as.factor(every$기온그룹)
# every$기온그룹 <- as.factor(every$기온그룹, levels=c('영하 10도 이하','영하 10도 이하','0~10도','10~20도',"20도 이상"))

summary(every)

# 발생일, 사고건수, 기온 그룹 변수 추출
every_temp5 <- every[c(1,2,9)]
View(every_temp5)

# 기온그룹에 따른 평균사고건수 계산
temp_crash5 <- every_temp5 %>% group_by(기온그룹5) %>%
  summarise(사고건수평균 = mean(total사고건수))
View(temp_crash5)

# 기온그룹5 시각화
temp_crash5$기온그룹5 <- factor(temp_crash5$기온그룹5, levels=c("-15~-10도","-10~-5도", "-5~0도","0~5도","5~10도","10~15도","15~20도","20~25도","25~30도","30~35도"))

ggplot(temp_crash5, aes(기온그룹5, 사고건수평균,group=1)) +
  geom_line()


####### 회귀분석
View(every)

# 사고건수, 평균기온, 일강수량 합계, 상대습도, 일조시간 추출
reg <- every[c(2:6)]
reg
summary(reg)

# 산점도 행렬
pairs(reg)

# 회귀분석 ( 종속변수 : 사고건수, 독립변수 : 평균기온, 일강수량, 상대습도, 일조시간)
model <- lm(total사고건수 ~ mean평균기온 + total일강수량 + mean평균상대습도 + total일조시간, data=reg)

par(mfrow = c(2,2)) # 2행, 2열로 그래프 표현
# 한번에 그래프 4개를 볼 수 있음
plot(model)
# 잔차산점도, 정규분포, 등분산성, 이상치 확인

summary(model)
# Call:
#   lm(formula = total사고건수 ~ mean평균기온 + total일강수량 + mean평균상대습도 + 
#        total일조시간, data = reg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -58.009 -11.397  -0.365  10.344  73.596 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)      58.1979461  3.2659854  17.819  < 2e-16
# mean평균기온     -0.2065272  0.0575248  -3.590 0.000345
# total일강수량     0.0027130  0.0004737   5.727 1.32e-08
# mean평균상대습도  0.4611138  0.0505789   9.117  < 2e-16
# total일조시간     0.0292278  0.0014476  20.191  < 2e-16
# 
# (Intercept)      ***
#   mean평균기온     ***
#   total일강수량    ***
#   mean평균상대습도 ***
#   total일조시간    ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.8 on 1076 degrees of freedom
# Multiple R-squared:  0.2901,	Adjusted R-squared:  0.2875 
# F-statistic: 109.9 on 4 and 1076 DF,  p-value: < 2.2e-16

# --> mean평균기온, total일강수량, mean평균상대습도, total일조시간 각각의 변수의 p-value값이 0.05이하이다.
# 따라서 유의수준 5%에서 각각의 변수가 사고건수에 영향을 미치는 유의한 변수라고 볼 수 있다.
# p-value < 0.05로 유의수준 5%에서 적절한 회귀 모델이라고 판단된다.
