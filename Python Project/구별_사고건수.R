new_cwh <- read.csv('data/교통사고_날씨_습도.csv')

# 구별 강수량 사고건수
head(new_cwh)

# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거
new_cwh <- new_cwh[!is.na(new_cwh$평균기온),]
new_cwh <- new_cwh[!is.na(new_cwh$일강수량),]
new_cwh <- new_cwh[!is.na(new_cwh$평균상대습도),]
new_cwh <- new_cwh[!is.na(new_cwh$합계일조시간),]

# 구별 강수량 사고건수
local_rain <- new_cwh %>% select(발생지_시군구, total사고건수,일강수량) %>%
  group_by(발생지_시군구) %>%
  summarise(지역_사고건수평균=mean(total사고건수), 지역_강수량=mean(일강수량))

# 시각화 
ggplot(local_rain,aes(x=reorder(발생지_시군구,지역_사고건수평균)))+
  geom_col(aes(y=지역_강수량*5),group=1, fill=c('ivory')) + 
  xlab("서울시_지역구")+
  ylab("지역_사고건수합(검정)")+
  geom_col(aes(y=지역_사고건수평균),group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "평균강수량(흰색)"))

# 구별 평균기온 사고건수
local_temp <- new_cwh %>% select(발생지_시군구, total사고건수,평균기온) %>%
  group_by(발생지_시군구) %>%
  summarise(지역_사고건수평균=mean(total사고건수), 지역_평균기온=mean(평균기온))
View(local_temp)
# 구별 평균기온 시각화
ggplot(local_temp,aes(x=reorder(발생지_시군구, 지역_사고건수평균)))+
  geom_col(aes(y=지역_평균기온),group=1, fill=c('ivory')) +
  xlab("서울시_지역구")+
  ylab("지역_사고건수합(검정)") +
  geom_col(aes(y=지역_사고건수평균),group=1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "평균기온(흰색)"))

local_rain_temp <- left_join(local_rain,local_temp,by=c('발생지_시군구','지역_사고건수평균'))

head(local_rain_temp)

# 계절 그룹 생성 
# 발생일 변수(문자열) -> 날짜형 변수로 변경
every$발생일 <- as.Date(every$발생일)
month(every$발생일)

# 구별 계절별 사고건수
new_cwh$발생일 <- as.Date(new_cwh$발생일)
month(new_cwh$발생일)

# 날짜생성
new_cwh$발생일 <- as.Date(new_cwh$발생일)
month(new_cwh$발생일)

# 계절 그룹 생성
new_cwh$계절 <- ifelse(month(new_cwh$발생일) >=3 & month(new_cwh$발생일) <6,
                     "봄",
                     ifelse(month(new_cwh$발생일) >=6 & month(new_cwh$발생일) < 9, '여름',
                            ifelse(month(new_cwh$발생일) >=9 & month(new_cwh$발생일) < 12, '가을', '겨울')))

# 계절 변수에서 봄/여름/가을/겨울추출
new_cwh1 <- new_cwh %>% filter(계절 %in% c('봄','여름','가을','겨울'))

# 발생지 시군구별 total사고건수 시각화
ggplot(new_cwh1, aes(x=발생지_시군구, y=total사고건수, fill=계절))+geom_bar(stat='identity')



# 상관분석

cor(new_cwh[,4:8])
#               total사고건수   평균기온    일강수량 평균상대습도 합계일조시간
#total사고건수   1.000000000   0.03978881  0.01949351  0.009984152   0.02824370

cor.test(new_cwh$total사고건수, new_cwh$평균기온 ,method='pearson')

# Pearson's product-moment correlation

# data:  new_cwh$total사고건수 and new_cwh$평균기온
# t = 6.361, df = 25518, p-value = 2.038e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.02753318 0.05203247
# sample estimates:
#        cor 
# 0.03978881 

cor.test(new_cwh$total사고건수, new_cwh$일강수량 ,method='pearson')

# Pearson's product-moment correlation
# 
# data:  new_cwh$total사고건수 and new_cwh$일강수량
# t = 3.1146, df = 25518, p-value = 0.001844
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.007226169 0.031754978
# sample estimates:
#        cor 
# 0.01949351

cor.test(local_rain_temp$지역_평균기온,local_rain_temp$지역_사고건수평균)
# Pearson's product-moment correlation
# 
# data:  local_rain_temp$지역_평균기온 and local_rain_temp$지역_사고건수평균
# t = 1.7831, df = 23, p-value = 0.08777
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.05408185  0.65362226
# sample estimates:
#       cor 
# 0.3484962 

cor.test(local_rain_temp$지역_강수량,local_rain_temp$지역_사고건수평균)

# Pearson's product-moment correlation
# 
# data:  local_rain_temp$지역_강수량 and local_rain_temp$지역_사고건수평균
# t = -0.07683, df = 23, p-value = 0.9394
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.4085632  0.3815275
# sample estimates:
#         cor 
# -0.01601816 

# 강수량과 습도의 상관관계
head(new_cwh)

n_r_s <- new_cwh[,6:7]

cor(n_r_s)

#             일강수량 평균상대습도
# 일강수량     1.0000000    0.4687223
corrplot(cor(new_cwh[,4:8]))
corrplot(cor(new_cwh[,4:8]),method='num',title='구별')
