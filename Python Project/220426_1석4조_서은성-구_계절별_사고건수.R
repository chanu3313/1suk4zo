#######################################
# 구별 계절별(봄여름가을겨울) 사고건수 -> 서은성 
#   x축 구, y축 : 사고건수
######################################

new_cwh <- read.csv("교통사고_날씨_습도.csv") 
head(new_cwh)

# 평균기온, 일강수량, 상대습도, 일조시간, 일사량 결측치 제거
new_cwh <- new_cwh[!is.na(new_cwh$평균기온),]
new_cwh <- new_cwh[!is.na(new_cwh$일강수량),]
new_cwh <- new_cwh[!is.na(new_cwh$평균상대습도),]
new_cwh <- new_cwh[!is.na(new_cwh$합계일조시간),]

summary(new_cwh) # 결측치 제거 확인
View(new_cwh)

str(new_cwh) 

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
