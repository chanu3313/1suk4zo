install.packages('lubridate')
library('lubridate')

getwd()
setwd()
new_cwh <- read.csv("C:/project/1suk4zo/Python Project/data/교통사고_날씨_습도.csv")

head(new_cwh)

# 발생일(문자열)을 날짜형 데이터로 변환
new_cwh[,2] <- as.Date(new_cwh[,2])
class(new_cwh[,2])

# 요일데이터 생성
new_cwh$발생일 <- as.POSIXct(new_cwh$발생일)
new_cwh$weekday <- weekdays(new_cwh$발생일)

head(new_cwh)
View(new_cwh)

new_cwh[,2] <- as.Date(new_cwh[,2])

# 요일을 평일, 주말로 분할한 '날짜' 변수 생성
new_cwh$날짜 <- ifelse(new_cwh$weekday == "토요일" | new_cwh$weekday == "일요일", "주말", "평일")

head(new_cwh)


##### '날짜' 변수에 공휴일 지정

#### 2018년

# 2017년 1월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 1 & (day(new_cwh$발생일) == 1 | day(new_cwh$발생일) == 27 | day(new_cwh$발생일) == 28 | day(new_cwh$발생일) == 29 | day(new_cwh$발생일) == 30), "공휴일", new_cwh$날짜)

# 2017년 3월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 3 & day(new_cwh$발생일) == 1, "공휴일", new_cwh$날짜)

# 2017년 5월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 5 & (day(new_cwh$발생일) == 3 | day(new_cwh$발생일) == 5), "공휴일", new_cwh$날짜)

# 2017년 6월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 6 & (day(new_cwh$발생일) == 6), "공휴일", new_cwh$날짜)

# 2017년 8월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 8 & day(new_cwh$발생일) == 15, "공휴일", new_cwh$날짜)

# 2017년 10월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 10 & (day(new_cwh$발생일) == 1 | day(new_cwh$발생일) == 3 | day(new_cwh$발생일) == 4 | day(new_cwh$발생일) == 5 | day(new_cwh$발생일) == 6 | day(new_cwh$발생일) == 9), "공휴일", new_cwh$날짜)

# 2017년 12월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2017 & month(new_cwh$발생일) == 12 & (day(new_cwh$발생일) == 20 | day(new_cwh$발생일) == 25), "공휴일", new_cwh$날짜)

#### 2018년 

# 2018년 1월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 1 & day(new_cwh$발생일) == 1, "공휴일", new_cwh$날짜)

# 2018년 2월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 2 & (day(new_cwh$발생일) == 15 | day(new_cwh$발생일) == 16 | day(new_cwh$발생일) == 17), "공휴일", new_cwh$날짜)

View(new_cwh)

# 2018년 3월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 3 & day(new_cwh$발생일) == 1, "공휴일", new_cwh$날짜)

# 2018년 5월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 5 & (day(new_cwh$발생일) == 5 | day(new_cwh$발생일) == 7 | day(new_cwh$발생일) == 22), "공휴일", new_cwh$날짜)

# 2018년 6월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 6 & (day(new_cwh$발생일) == 6 | day(new_cwh$발생일) == 13), "공휴일", new_cwh$날짜)

# 2018년 8월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 8 & day(new_cwh$발생일) == 15, "공휴일", new_cwh$날짜)

# 2018년 9월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 9 & (day(new_cwh$발생일) == 23 | day(new_cwh$발생일) == 24 | day(new_cwh$발생일) == 25 | day(new_cwh$발생일) == 26), "공휴일", new_cwh$날짜)

# 2018년 10월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 10 & (day(new_cwh$발생일) == 3 | day(new_cwh$발생일) == 9), "공휴일", new_cwh$날짜)

# 2018년 12월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2018 & month(new_cwh$발생일) == 12 & day(new_cwh$발생일) == 25, "공휴일", new_cwh$날짜)

#### 2019년 

# 2019년 1월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 1 & day(new_cwh$발생일) == 1, "공휴일", new_cwh$날짜)

# 2019년 2월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 2 & (day(new_cwh$발생일) == 4 | day(new_cwh$발생일) == 5 | day(new_cwh$발생일) == 6), "공휴일", new_cwh$날짜)

# 2019년 3월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 3 & day(new_cwh$발생일) == 1, "공휴일", new_cwh$날짜)

# 2019년 5월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 5 & (day(new_cwh$발생일) == 5 | day(new_cwh$발생일) == 6 | day(new_cwh$발생일) == 12), "공휴일", new_cwh$날짜)

# 2019년 6월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 6 & (day(new_cwh$발생일) == 6), "공휴일", new_cwh$날짜)

# 2019년 8월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 8 & day(new_cwh$발생일) == 15, "공휴일", new_cwh$날짜)

# 2019년 9월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 9 & (day(new_cwh$발생일) == 12 | day(new_cwh$발생일) == 13 | day(new_cwh$발생일) == 14), "공휴일", new_cwh$날짜)

# 2019년 10월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 10 & (day(new_cwh$발생일) == 3 | day(new_cwh$발생일) == 9), "공휴일", new_cwh$날짜)

# 2019년 12월
new_cwh$날짜 <- ifelse(year(new_cwh$발생일) == 2019 & month(new_cwh$발생일) == 12 & day(new_cwh$발생일) == 25, "공휴일", new_cwh$날짜)

View(new_cwh)

# csv파일로 생성
write.csv(new_cwh, file="C:/project/1suk4zo/Python Project/data/교통사고_날씨_습도_요일.csv", row.names=FALSE)
