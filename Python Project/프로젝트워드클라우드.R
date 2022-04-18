install.packages("rJava") #r에서 Java를 사용한 패키지 사용
install.packages("memoise") #koNLP패키지가 사용하는 패키지
install.packages("KoNLP") #한국어 자연어 처리 
install.packages("stringr") #문자열을 처리하는 여러가지 함수를가지고 있는 패키지
install.packages("wordcloud")
install.packages("wordcloud2")
#워드클라우드를 그려주는 패키지 #패키지 라이브러리에 올리기
library(KoNLP)
library(stringr) 
library(wordcloud) 
library(wordcloud2) 
search()
setwd('data')
setwd('D:/project1/1suk4zo/Python Project/data')

Sys.getlocale()
Sys.setlocale("LC_ALL","C")

#한국어 분석에 사용할 사전을 로드
useSejongDic()#텍스트 파일(*.txt)에서 한줄씩 문자열들을 읽음
a <-readLines("acci.txt",encoding="UTF-8") 
str(a)
head(a)

b<-extractNoun(a)
bb <- unlist(b)
bb <- gsub("qwertyuiopasdfghjklzxcvbnm","",bb)
bb <- gsub("http","",bb)
bb <- gsub("href","",bb)
bb <- gsub("https","",bb)
bb <- gsub("span","",bb)
bb <- gsub("ne","",bb)
bb <- gsub("ad","",bb)
bb <- gsub("gif","",bb)
bb <- gsub("br","",bb)
bb <- gsub("php","",bb)
bb <- gsub("class","",bb)
bb <- gsub("more","",bb)
bb <- gsub("txt","",bb)
bb <- gsub("img","",bb)
bb <- gsub("view","",bb)
bb <- gsub("title","",bb)
bb <- gsub("ss","",bb)
bb <- gsub("pt","",bb)
bb <- gsub("ECONOMY","",bb)
bb <- gsub("style","",bb)
bb <- gsub("ba","",bb)
bb <- gsub("px","",bb)
bb <- gsub("com","",bb)
bb <- gsub("size","",bb)
bb <- gsub("mail","",bb)
bb <- gsub("html","",bb)
bb <- gsub("text","",bb)
bb <- gsub("auto","",bb)
bb <- gsub("font","",bb)
bb <- gsub("www","",bb)
bb <- gsub("li","",bb)
bb <- gsub("ws","",bb)
bb <- gsub("jpg","",bb)
bb <- gsub("div","",bb)
bb <- gsub("kr","",bb)
bb <- gsub("kih","",bb)
bb <- gsub("key","",bb)
bb <- gsub("ray","",bb)
bb <- gsub("All","",bb)
bb <- gsub("rs","",bb)
bb <- gsub("mainlistmain","",bb)
bb <- gsub("mt","",bb)
bb <- gsub("ID","",bb)
bb <- gsub("Copyright","",bb)
bb <- gsub("to","",bb)
bb <- gsub("ffd","",bb)
bb <- gsub("idb","",bb)
bb <- gsub("nk","",bb)
bb <- gsub("sa","",bb)
bb <- gsub("si","",bb)
bb <- gsub("id","",bb)
bb <- gsub("js","",bb)
bb <- gsub("alt","",bb)
bb <- gsub("st","",bb)
bb <- gsub("sm","",bb)
bb <- gsub("Ltd","",bb)
bb <- gsub("art","",bb)
bb <- gsub("st","",bb)
bb <- gsub("ma","",bb)
bb <- gsub("BK","",bb)
bb <- gsub("by","",bb)
bb <- gsub("fo","",bb)
bb <- gsub("gta","",bb)
bb <- gsub("rio","",bb)
bb <- gsub("p또","",bb)
bb <- gsub("PM","",bb)
bb <- gsub("no","",bb)
bb <- gsub("PC","",bb)
bb <- gsub("rr","",bb)
bb <- gsub("BY","",bb)
bb <- gsub("ins","",bb)
bb <- gsub("viva","",bb)
bb <- gsub("mainmain","",bb)
bb <- gsub("mb","",bb)
bb <- gsub("agn","",bb)
bb <- gsub("wth","",bb)
bb <- gsub("cj","",bb)
bb <- gsub("SM","",bb)
bb <- gsub("Btv","",bb)
bb <- gsub("sub","",bb)
bb <- gsub("in","",bb)
bb <- gsub("src","",bb)
bb <- gsub("sry","",bb)
bb <- gsub("tit","",bb)
bb <- gsub("type","",bb)
bb <- gsub("em","",bb)
bb <- gsub('\\d+',"",bb)
bb <- gsub("[~!@#$%^&*(){}_+:\"<>?,.;\'-=]","",bb)
bb <- gsub("\\[","",bb)
bb <- gsub("\\]","",bb)
bb <- gsub("\\''","",bb)
bb <- gsub(" ","",bb)
bb <- gsub("→","",bb)
bb <- gsub("/","",bb)
bb <- gsub("↑","",bb)
bb <- gsub("\\/","",bb)
bb <- gsub("\\","",bb)

head(bb)
table(bb)

write(bb,"bb.txt")
c <- read.table("bb.txt")
# View(c)
cc <- table(c)
(sort(cc,decreasing = T))


nchar(bb) >=2
bb <- Filter(function(x){nchar(x) >= 2},bb)
bb

cc <- table(bb)
# View(cc)


write(bb,"cc.txt")
d <- read.table("cc.txt")

dd <- table(d)
# View(sort(dd,decreasing = T))

palete <- brewer.pal(9,"Set3")
wordcloud(names(dd), 
          freq=dd,
          scale=c(5,1),     
          rot.per=0.25,     
          min.freq = 1,     
          random.order = F, 
          random.color = T, 
          colors = palete)

# 별모양 워드 크라우드
wordcloud2(dd, size = 0.7,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           rotateRatio = 0.4, shape = 'star', ellipticity = 0.65,)


