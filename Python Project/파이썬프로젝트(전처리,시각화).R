################# 파이썬 프로젝트 전처리 시각화 ###################

# 파일 경로 설정
setwd("D:/project1/1suk4zo/Python Project")

# 필요 패키지 부착
library(descr) # 변수명 변경,데이터 추출, 정렬,요약,그룹,결합,제거등을 할수 있다.
library(dplyr) # 조건 행 추출, 함수연결등을 할수 있다.
library(ggplot2) # 그래프 그리기
library(kormaps2014) # 한국의 지역별 인구통계 데이터와 지역별 지도 데이터가 있음
library(ggiraph) # ggplot2 등의 클릭시 툴팁등이 작용
library(ggiraphExtra) # 단계구분도를 만들기위한 패키지
library(readxl) # xl을 불러오는 패키지
library('tidyverse') # dplyr, tidyr, ggplot2 등 tidy 패키지 생태계에 속하는 핵심 패키지들을 한번에 설치 및 관리 (lubridate를 사용하기위한 선행 패키지)
library('lubridate') # 날짜와 시간을 다루는 패키지
library(scales) # 축과 범례의 파손 및 레이블을 자동으로 결정하는 방법을 제공하는 시각화를 위한 척도 함수

### 데이터 불러오기
crash <- read.csv("data/도로교통공단_서울시 일별 시간별 교통사고 현황_20191231.csv")
weather <- read.csv("data/2017-2019날씨데이터_1.csv")
humidity <- read.csv("data/17-19 일별 서울 습도,일조,일사량.csv")
humidity <- humidity[-c(1,2)] # 지점,지점명 제거(불필요)
seoul_map <- read_excel('data/서울_map.xlsx')
seoul1 <- read_excel('data/서울.xlsx') # 서울시 지역구별 코드
shine <- read_csv('data/sunshine.csv',locale = locale("ko", encoding = "euc-kr")) # 일조시간

## 데이터 전처리
weather <- rename(weather, 발생지_시군구=지점명, 발생일=일시) # 날씨 데이터 변수 맞추기

humidity <- rename(humidity, 발생일=일시) # 습도 데이터 변수 맞추기

crash_weather <- left_join(crash,weather,by=c('발생지_시군구','발생일')) # 사고와 날씨 데이터 by로 병합

crash_weather_humidity <- left_join(crash_weather,humidity,by='발생일') # 사고,날씨,습도,일사 데이터 by로 병합
View()
# 복사본 생성(원본 유지)
c_w_h <- crash_weather_humidity
View(c_w_h)
# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거
c_w_h_every <- c_w_h[!is.na(c_w_h$평균기온..C.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$일강수량.mm.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일조시간.hr.),]
c_w_h_every <- c_w_h_every[!is.na(c_w_h_every$합계.일사량.MJ.m2.),]

# 발생일별 사고건수 합계, 평균 기온, 일강수량 합계, 평균상대습도, 일조시간 합계 계산
every <- c_w_h_every %>% group_by(발생일) %>%
  summarise(total사고건수 = sum(사고건수),
            mean평균기온 = mean(평균기온..C.),
            일강수량 = max(일강수량.mm.),
            평균상대습도 = mean(평균.상대습도...),
            일조시간 = max(합계.일조시간.hr.),
            일사량 = mean(합계.일사량.MJ.m2.))

##################################### 평균기온 사고 ##############################
# 평균기온 범위
range(every$mean평균기온)

# 발생일 변수(문자열) -> 날짜형 변수로 변경
every$발생일 <- as.Date(every$발생일)
month(every$발생일)

# 계절 그룹 생성
every$계절 <- ifelse(month(every$발생일) >=4 & month(every$발생일) <6,
                   "봄",
                   ifelse(month(every$발생일) >=6 & month(every$발생일) < 9, '여름',
                          ifelse(month(every$발생일) >=10 & month(every$발생일) < 11, '가을', '겨울')))

# 계절별 사고건수 평균 dataframe 생성
seasons <- every %>% group_by(계절) %>%
  summarise(계절_사고건수평균 = mean(total사고건수)) %>%
  arrange(desc(계절_사고건수평균))

# 계절 변수를 factor형 변수로 변경
seasons$계절 <- factor(seasons$계절, level=c("겨울","여름","봄","가을"))

# dev.off() # 아래 오류메시지 발생 시 plot 초기화시켜주는 명령어
# Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
#   invalid graphics state

# 계절별 사고건수 평균 시각화
ggplot(seasons, aes(x=계절, y=계절_사고건수평균, fill=계절_사고건수평균)) + ylim(0,120) +
  geom_bar(stat='identity')

# 계절별 통행량 기사 참조하기

# 년도 그룹 생성
every$년도 <- ifelse(year(every$발생일) == 2017,
                   "2017년",
                   ifelse(year(every$발생일) == 2018, '2018년', '2019년'))

# 년도별 사고건수 평균 dataframe 생성
year <- every %>% group_by(년도) %>%
  summarise(계절_사고건수평균 = mean(total사고건수)) %>%
  arrange(desc(계절_사고건수평균))

# 년도별 사고건수 평균 시각화
ggplot(year, aes(x=년도, y=계절_사고건수평균, fill=계절_사고건수평균)) +
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

# every$기온그룹 <- as.factor(every$기온그룹)
# every$기온그룹 <- as.factor(every$기온그룹, levels=c('영하 10도 이하','영하 10도 이하','0~10도','10~20도',"20도 이상"))
View(every)
# 발생일, 사고건수, 기온 그룹 변수 추출
every_temp5 <- every[c(1,2,8)]
View(every_temp5)
# 기온그룹에 따른 평균사고건수 계산
temp_crash5 <- every_temp5 %>% group_by(기온그룹5) %>%
  summarise(사고건수평균 = mean(total사고건수))

# 기온그룹5 시각화
temp_crash5$기온그룹5 <- factor(temp_crash5$기온그룹5, levels=c("-15~-10도","-10~-5도", "-5~0도","0~5도","5~10도","10~15도","15~20도","20~25도","25~30도","30~35도"))

ggplot(temp_crash5, aes(기온그룹5, 사고건수평균,group=1)) +
  geom_line()

################# 일별 사고건수_ 강수량 ###################
# 일별 사고건수_강수량 으로 합치기
#acci_sum <- c_w_h_every %>%
#  group_by(발생일)%>%
#  summarise(사고건수 =sum(사고건수))

# rain <- c_w_h_every %>%
#   group_by(발생일)%>%
#   summarise(강수량 =max(일강수량.mm.))

# acci_rain <- left_join(acci_sum,rain,by='발생일')

#### 시각화
# 년도별로 행 나누기
a_c_17 <- every %>% slice(1:357) # 17년도 
a_c_18 <- every %>% slice(358:718) # 18년도
a_c_19 <- every %>% slice(719:1081) # 19년도
par(mfrow=c(1,2))
# 히스토그램
hist(every$total사고건수,plot=T,main='사고건수') # 일별 사고건수
hist(every$일강수량,plot=T,main='강수량') # 강수량 일수

ggplot(every, aes(x=total사고건수)) + geom_bar() # 사고건수량

ggplot(every, aes(x=일강수량,y=total사고건수)) + geom_line(aes(group='발생일')) # 시계열

# 발생일에 따른 강수량,사고건수 이중그래프 그려보기 (년도,년도총합)
# 오른쪽에 강수량축이 있어야한다(축다르게)(표준화해서 그리기)
# 17년도
ggplot(a_c_17, aes(x=발생일)) +
  geom_col(aes(y=일강수량),group=1)+
  geom_line(aes(y=total사고건수),group=1,col=c("blue"))
# 18 년도
ggplot(a_c_18, aes(x=발생일)) +
  geom_col(aes(y=일강수량),group=1)+
  geom_line(aes(y=total사고건수),group=1,col=c("blue"))
# 19년도
ggplot(a_c_19, aes(x=발생일)) +
  geom_col(aes(y=일강수량),group=1)+
  geom_line(aes(y=total사고건수),group=1,col=c("blue"))
# 17,18,19년도 총합
ggplot(every, aes(x=발생일)) +
  geom_col(aes(y=일강수량),group=1)+
  geom_line(aes(y=total사고건수),group=1,col=c("blue"))

## 강수량 구간별 사고건수 평균 구해서 비교하기
# 강수량에 따른 사고건수 평균 구하기
# 강수량 나누기
every$강수량_그룹 <- ifelse(every$일강수량 ==0 , '0' ,
                           ifelse(every$일강수량>0 &every$일강수량 <=10, '0~10' ,
                                  ifelse(every$일강수량>10 &every$일강수량 <=20, '10~20',
                                         ifelse(every$일강수량>20 &every$일강수량 <=30, '20~30',
                                                ifelse(every$일강수량>30 &every$일강수량 <=40, '30~40',
                                                       ifelse(every$일강수량>40 &every$일강수량 <=50, '40~50',
                                                              ifelse(every$일강수량>50 &every$일강수량 <=65, '50~65',
                                                                     ifelse(every$일강수량>65 &every$일강수량 <=80, '65~80',
                                                                            ifelse(every$일강수량>80 &every$일강수량 <=90, '80~90',
                                                                                   ifelse(every$일강수량>90 &every$일강수량 <=100, '90~100',
                                                                                          ifelse(every$일강수량>100 &every$일강수량 <=150, '100~150','150이상')))))))))))

View(every)

# 강수량에따른 그룹의 평균 구하기
acci_rain_group <- every %>% 
  group_by(강수량_그룹) %>% 
  summarise(사고건수_평균 = mean(total사고건수))

# 강수량 그룹의 순서 정하기

acci_rain_group$강수량_그룹 <- factor(acci_rain_group$강수량_그룹, levels=c("0","0~10","10~20","20~30","30~40","40~50","50~65","65~80","80~90","90~100","100~150","150이상"))

class(acci_rain_group$강수량_그룹)
levels(acci_rain_group$강수량_그룹)

ggplot(acci_rain_group, aes(x=강수량_그룹,y=사고건수_평균 ,group=1))+ ylim(90,125) + geom_line() + ggtitle("17~19년도 강수량에따른 사고건수평균")

# 강수량에 따른 사고 평균을 보면 0부터 100까지 우상향인걸로 보아 강수량이 높아질수록 사고가 좀더 많이난다는걸 예측해볼 수 있다 하지만 100이후에는 사고발생수가 떨어지는데 이는 강수량이 많아짐에따라 외출, 대중교통이용에 따른 자가용 사용 등이 줄어 사고건수가 낮아지는것으로 생각할수 있었다.

#####################################
### 시간별 사고건수 분석
View(c_w_h)

acci_t <- c_w_h_every %>%
  group_by(발생시간) %>% 
  summarise(시간별_사고건수_합 = sum(사고건수))

ggplot(acci_t,aes(x=발생시간,y=시간별_사고건수_합)) + geom_line()

# 오전 3~4시가 일별중 가장 사고가 적고 (유동인구가 적은시간이기 때문), 이후 5시부터 출근하기 시작하며 오전중 8시에 가장 사고건수가 많았다가 이후 오후에 유동인구가 점점많아지면서 사고건수가 오르고, 퇴근시간인 6시가 가장 교통사고가 높이 나타났다가 이후 점차 줄어들어드는것으로 분석할 수 있다.

### 구별 사고건수 분석
# 구별 사고건수 합
acci_gu <- c_w_h %>% 
  group_by(발생지_시군구) %>% 
  summarise(시군구별_사고건수_합 = sum(사고건수))

ggplot(acci_gu,aes(x=시군구별_사고건수_합,y=발생지_시군구,fill=시군구별_사고건수_합)) + geom_col()

# 구별 사고건수 단계구분도

# 단계구분도 code 부착 
# 오류뜨면 다시 불러오기할것 (변수수정하면 그변수가 사라지기때문)
# seoul1 <- read_excel('data/서울.xlsx') # 서울시 지역구별 코드
# 계절별로 해볼것(봄,여름,가을,겨울)

seoul1 <- rename(seoul1,발생지_시군구=행정구역별_읍면동) # seoul1 변수 수정

seoul_code <- select(seoul1,"발생지_시군구","code") # seoul1에서 시군구,code만 추출

acci_gu_code <- left_join(acci_gu,seoul_code,by='발생지_시군구') # 추출한 seoul코드와 전처리한 코드 합치기

ggChoropleth(data=acci_gu_code,
             aes(fill=시군구별_사고건수_합,
                 map_id=code,
                 tooltip=발생지_시군구),
             map=seoul_map,
             interactive=T)

# 강남구 , 송파구 , 영등포구 , 서초구 순으로 사고건수가 가장 많다.

#################################

# 일조시간 요약 읽기
summary(every$일조시간)

# "발생일","발생시간", "사고건수","합계.일조시간.hr." 선택 후 재저장
shine <- select(every,"발생일","total사고건수","일조시간")
head(shine)
View(shine)

# 검증 : 서울시의 날짜별 일조시간(x)이  교통사고 건수(y)와 상관있는지 확인
# 일조시간이 교통사고와 관련 있다는 가설을 검증
# 파생변수 만들기 - 일조시간별 그룹 생성
shine$group <- ifelse(shine$일조시간 <= 1,'1',
                      ifelse(shine$일조시간 <= 2,'2',
                             ifelse(shine$일조시간 <= 3,'3', 
                                    ifelse(shine$일조시간 <= 4,'4',
                                           ifelse(shine$일조시간 <= 5,'5',
                                                  ifelse(shine$일조시간 <= 6,'6',
                                                         ifelse(shine$일조시간 <= 7,'7', 
                                                                ifelse(shine$일조시간 <= 8,'8',
                                                                       ifelse(shine$일조시간 <= 9,'9',
                                                                              ifelse(shine$일조시간 <= 10,'10',
                                                                                     ifelse(shine$일조시간 <= 11,'11', 
                                                                                            ifelse(shine$일조시간 <= 12,'12',
                                                                                                   ifelse(shine$일조시간 <= 13,'13','14이상')))))))))))))

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








































####### 회귀분석
# 사고건수, 평균기온, 일강수량 합계, 상대습도, 일조시간 추출
reg <- every[c(2:6)]
reg

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
View(every)
# --> mean평균기온, total일강수량, mean평균상대습도, total일조시간 각각의 변수의 p-value값이 0.05이하이다.
# 따라서 유의수준 5%에서 각각의 변수가 사고건수에 영향을 미치는 유의한 변수라고 볼 수 있다.
# p-value < 0.05로 유의수준 5%에서 적절한 회귀 모델이라고 판단된다.










