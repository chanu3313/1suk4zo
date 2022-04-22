# 220418_1석4조_빅데이터 프로젝트.R
#######################################
# 서은성: 일조시간과 사고건수(전처리,시각화)
#######################################

# library(ggmap)
# library(raster)
# library(rgeos)
# library(maptools)
# library(rgdal)
# library(foreign)
library(ggplot2)
library(dplyr)
library(descr)
# .libPaths() 
# library(googleVis)
# library(reshape2)
# library(ggplot2)
# library(MASS)
# install.packages('tidyverse')
# library('tidyverse')
# install.packages('lubridate')
# library('lubridate')
# library(readxl)
# library(psych)
# library(readxl)
# library(scales)
# library(ggiraphExtra) 

# 현재 디렉터리 조회
getwd() #"C:/PhythonStudy/1suk4zo/Python Project/data"
# 작업 디렉터리를 data 폴더로 설정
setwd("C:/PhythonStudy/1suk4zo/Python Project") 
# 'C:/PhythonStudy/1suk4zo/Python Project'
# 디렉토리 변경 : setwd("c:\\temp")

### 데이터 불러오기
crash <- read.csv("data/도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
weather <- read.csv("data/2017-2019날씨데이터_1.csv")
humidity <- read.csv("data/17-19 일별 서울 습도,일조,일사량.csv")
humidity <- humidity[-c(1,2)] # 지점,지점명 제거(불필요)
View(humidity)
seoul_map <- read_excel('data/서울_map.xlsx')
seoul1 <- read_excel('data/서울.xlsx') # 서울시 지역구별 코드
shine <- read_csv('data/sunshine.csv',locale = locale("ko", encoding = "euc-kr")) # 일조시간

## 데이터 전처리
weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시) # 날씨 데이터 변수 맞추기

humidity <- rename(humidity, 발생일=일시) # 습도 데이터 변수 맞추기

crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일')) # 사고와 날씨 데이터 by로 병합

crash_weather_humidity <- left_join(crash_weather,humidity,by='발생일') # 사고,날씨,습도,일사 데이터 by로 병합

# 복사본 생성(원본 유지)
c_w_h <- crash_weather_humidity
View(c_w_h)
# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거
c_w_h_every <- c_w_h[!is.na(c_w_h$평균기온..C.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$일강수량.mm.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일조시간.hr.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일사량.MJ.m2.),]

# 발생일별 사고건수 합계, 평균 기온, 일강수량 합계, 평균상대습도, 일조시간 최대 계산
every <- c_w_h_every %>% group_by(발생일) %>%
  summarise(total사고건수 = sum(사고건수),
            mean평균기온 = mean(평균기온..C.),
            total일강수량 = max(일강수량.mm.),
            mean평균상대습도 = mean(평균.상대습도...),
            max일조시간 = max(합계.일조시간.hr.),
            total일사량 = mean(합계.일사량.MJ.m2.))

View(every)
ls(every)

# 일조시간 요약 읽기
summary(every$max일조시간)

# "발생일","발생시간", "사고건수","합계.일조시간.hr." 선택 후 재저장
shine <- select(every,"발생일","total사고건수","max일조시간")
head(shine)
View(shine)

# 검증 : 서울시의 날짜별 일조시간(x)이  교통사고 건수(y)와 상관있는지 확인
# 일조시간이 교통사고와 관련 있다는 가설을 검증
# 파생변수 만들기 - 일조시간별 그룹 생성
shine$group <- ifelse(shine$max일조시간 <= 1,'1',
                           ifelse(shine$max일조시간 <= 2,'2',
                                  ifelse(shine$max일조시간 <= 3,'3', 
                                         ifelse(shine$max일조시간 <= 4,'4',
                                                ifelse(shine$max일조시간 <= 5,'5',
                                                       ifelse(shine$max일조시간 <= 6,'6',
                                                              ifelse(shine$max일조시간 <= 7,'7', 
                                                                     ifelse(shine$max일조시간 <= 8,'8',
                                                                            ifelse(shine$max일조시간 <= 9,'9',
                                                                                   ifelse(shine$max일조시간 <= 10,'10',
                                                                                          ifelse(shine$max일조시간 <= 11,'11', 
                                                                                                 ifelse(shine$max일조시간 <= 12,'12',
                                                                                                        ifelse(shine$max일조시간 <= 13,'13','14이상')))))))))))))

# 사고건수_평균, 그룹화 저장
dura_sunshine_group <- shine %>%
  group_by(group) %>%
  summarise(total사고건수 = sum(total사고건수))
View(dura_sunshine_group)
summary(dura_sunshine_group)

# 파생변수 만들기: x를 순서대로 정렬
dura_sunshine_group$group <- factor(dura_sunshine_group$group,levels=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14이상'))
View(dura_sunshine_group)
class(dura_sunshine_group)

# 일조시간 별_사고건수_합 그래프: 선그래프
ggplot(dura_sunshine_group, aes(x=group, y=total사고건수,group=1
))+ 
  geom_line(aes(color=group))+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "bottom")+
  geom_text(aes(label=group),pisition=position_stack(vjust=0.5))

#######################################
# (계절별 시간별 연도별) 구별 사고건수 
#   x축 연도, y축 : 사고건수
######################################
# 필요한 컬럼만 추출("발생일","발생지_시군구", "사고건수")
ls(crash)
data_gu <- select(crash,"발생일","발생지_시군구", "사고건수")
head(data_gu)
View(data_gu)
# write.csv(data_gu,file="data_gu.csv")  

# 구별로 그룹
data_gu_group <- data_gu %>%
  group_by(발생지_시군구) %>%
  summarise(시군구별_사고건수_합 = sum(사고건수))
View(data_gu_group)

# 시군구별_사고건수_합 그래프
ggplot(data_gu_group,aes(x=시군구별_사고건수_합,y=발생지_시군구,fill=시군구별_사고건수_합)) + geom_col()
