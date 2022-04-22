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
# library(ggplot2)
# library(dplyr) 
# library(descr)
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
getwd() : [1] "C:/PhythonStudy/1suk4zo/Python Project/data"
# 작업 디렉터리를 data 폴더로 설정
setwd() 
# C:/PhythonStudy/1suk4zo/Python Project/data
# 디렉토리 변경 : setwd("c:\\temp")

# 전처리한 파일 읽기
shine <- read_csv('sunshine.csv',locale = locale("ko", encoding = "euc-kr"))
head(shine)
View(shine)
ls(shine)


# 일조시간 요약 읽기
summary(shine$합계.일조시간.hr.) # 요약,평균,중간값
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   3.600   8.500   7.222  10.100  13.700    1074 

# "발생일","발생시간", "사고건수","합계.일조시간.hr." 선택 후 재저장
shinedata <- select(shine,"발생일","발생시간", "사고건수","합계.일조시간.hr.")
head(shinedata)
View(shinedata)

# 변수 속성 확인
str(shinedata)

# 검증 : 서울시의 날짜별 일조시간(x)이  교통사고 건수(y)와 상관있는지 확인
# 일조시간이 교통사고와 관련 있다는 가설을 검증

# 일조시간 합계  
shine <- shinedata %>%
  group_by(발생일) %>%
  summarise(일조시간 = SUM(합계.일조시간.hr.))
View(shine)

# 사고건수 합계
acci_sum <- shinedata %>%
  group_by(발생일) %>%
  summarise(사고건수 = sum(사고건수))
View(acci_sum)

# 사고건수, 일조시간 병합
data_shine <- merge(shine,acci_sum, by="발생일", all=T)
View(data_shine)
is.na(data_shine)

# 사고건수, 일조시간 병합 파일 저장 
write.csv(data_shine,file="data_shine.csv")

# 사고건수_평균, 그룹화 저장
dura_sunshine_group <- data_shine %>% 
  group_by(data_shine) %>% 
  summarise(사고건수_평균 = mean(사고건수))
View(dura_sunshine_group)
summary(data_shine)

# 결측치 제거 
data_shine <- na.omit(data_shine)
View(data_shine)

# 결측치 제거 후 재확인
summary(data_shine)

# 파생변수 만들기 - 일조시간별 그룹 생성
data_shine$group <- ifelse(data_shine$일조시간 <= 1,'1',
                           ifelse(data_shine$일조시간 <= 2,'2',
                                  ifelse(data_shine$일조시간 <= 3,'3', 
                                         ifelse(data_shine$일조시간 <= 4,'4',
                                                ifelse(data_shine$일조시간 <= 5,'5',
                                                       ifelse(data_shine$일조시간 <= 6,'6',
                                                              ifelse(data_shine$일조시간 <= 7,'7', 
                                                                     ifelse(data_shine$일조시간 <= 8,'8',
                                                                            ifelse(data_shine$일조시간 <= 9,'9',
                                                                                   ifelse(data_shine$일조시간 <= 10,'10',
                                                                                          ifelse(data_shine$일조시간 <= 11,'11', 
                                                                                                 ifelse(data_shine$일조시간 <= 12,'12',
                                                                                                        ifelse(data_shine$일조시간 <= 13,'13','14이상')))))))))))))


summary(dura_sunshine_group)

# 파생변수 만들기: x를 순서대로 정렬
dura_sunshine_group$group <- factor(dura_sunshine_group$group,levels=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14이상'))
View(dura_sunshine_group)
class(dura_sunshine_group)

# 일조시간 별_사고건수_합 그래프: 선그래프
ggplot(dura_sunshine_group, aes(x=group, y=사고건수_평균,group=1
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
