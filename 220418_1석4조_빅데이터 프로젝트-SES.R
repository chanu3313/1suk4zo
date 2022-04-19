# 220418_1석4조_빅데이터 프로젝트.R
# 서은성: 지역구별 교통사고 발생 현황(전처리,시각화)
# 강수량에 따른 구별 교통사고 건수 예측 (모델링)

# 1. 파일을 원본 그대로 읽어오기
getwd() # 현재 디렉터리 조회: [1] "C:/PhythonStudy/1suk4zo/Python Project/data"
setwd('Python Project/data') # 작업 디렉터리를 data 폴더로 설정
# C:/PhythonStudy/1suk4zo/Python Project/data

# 4. 사용자로부터 국어 점수를 입력 받아 벡터로 저장
traffic_info_17_19 <- read.csv("도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv") 
traffic_info_17_19
View(traffic_info_17_19)
ls(traffic_info_17_19)
sum(traffic_info_17_19["사망자수"]) # [1] 897
traffic_info_17_19_c <- ls(traffic_info_17_19) 
#ls():변수목록반환
# [1] "경상자수"      "발생시간"      "발생일"        "발생지_시군구"
# [5] "발생지_시도"   "법정동명"      "부상신고자수"  "사고건수"     
# [9] "사망자수"      "중상자수"  
typeof(traffic_info_17_19)

apply(traffic_info_17_19_c[-1],2,sum) # 첫번째 열을 제외하고 나머지 열의 합계를 계산해달라

library(readxl)
traffic_info_2017 <- read_xls("서울시 시간대별 교통사고 2017.xls") 
traffic_info_2017
View(traffic_info_2017)

traffic_info_2018 <- read_xls("서울시 시간대별 교통사고 2018.xls") 
traffic_info_2018

traffic_info_2019 <- read_xls("서울시 시간대별 교통사고 2019.xls") 
traffic_info_2019

rain_info_17_19 <- read.csv("20172019기온및강수량(기상청).csv") 
rain_info_17_19

#install.packages('dplyr')
library(ggplot2)
#install.packages('dplyr')
library(dplyr) # 데이터 처리를 위한 R 패키지로 데이터추출, 변수 선택, 변수 추가, 정렬, 결과 요약과 같은 데이터 분석을 위한 사전처리 작업을 보다 쉽게 할 수 있는 기능 제공
# install.packages("descr")
library(descr)
.libPaths()
library(googleVis)
library(reshape2)
library(ggplot2)
library(MASS)

# 열이름은 name, tel, address로 변경해서 읽어 오기
# traffic_info_17_19_list <- read.table('mem.txt', 
#                   header=T, 
#                   col.names = c("name","tel","address"))

# ggplot: 그래프의 배경을 만들어 주는 함수
# ggplot(traffic_info_17_19,aes("발생지_시군구",value,color="사망자수")) + geom_line()
# ggplot(traffic_info_17_19,aes(x="발생지_시군구", y="사망자수")) + geom_point()
ggplot(traffic_info_17_19,aes("발생지_시군구", "사망자수")) + geom_point(color='blue', shape=2, size=2)

ggplot(data=traffic_info_17_19,aes(x="발생지_시군구", y="사망자수")) + 
  geom_point(color='red',shape=3) +
  xlim(0,20) +
  ylim(0,20)

ggplot(traffic_info_17_19,aes("발생지_시군구", "사망자수")) + geom_col(aes(fill="사망자수))

df <- traffic_info_17_19 %>% 
  filter(class=="사망자수") %>% 
  group_by("발생지_시군구") %>% 
  summarise(mean_traffic_info_17_19 = mean(traffic_info_17_19)) %>% 

# df_1 %>% 
#   filter(class %in% c("발생지_시군구","사망자수")) %>% 
#   ggplot(aes(x=class, y="부상신고자수")) + geom_boxplot()  
