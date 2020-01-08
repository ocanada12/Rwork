#################################
## <제13장 연습문제>
################################# 

# 01. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 
#  기존 구매비율  보다 15% 향상되었는지를 각 단계별로 분석을 수행하여 검정하시오.
#귀무가설(H0) : 기존 구매비율과 차이가 없다.
#연구가설(H1) : 기존 구매비율과 차이가 있다.

#조건) 구매여부 변수 : buy (1: 구매하지 않음, 2: 구매)

# 단계1 : 데이터셋 가져오기
hdtv <- read.csv("hdtv.csv")

# 단계2 :  빈도수와 비율 계산
summary(hdtv) 
length(hdtv$buy) # 50개

table(hdtv$buy)
table(hdtv$buy, useNA="ifany") # NA 빈도수 표시


# 단계3 : 가설검정
binom.test(10,50,p=0.15)

# 단계4 : 검정결과 해설 
# p-value = 0.321 > 0.05 귀무가설 채택 : 유의성 없다. => 기존 구매비율과 차이가 없다.


# 02. 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm로 알려져 있는 상태에서  
# A중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정하여 표본평균신장을 
# 계산하고 모집단의 평균과 차이가 있는지를 각 단계별로 분석을 수행하여 검정하시오.

# 단계1 : 데이터셋 가져오기
stheight<- read.csv("student_height.csv")
stheight
height <- stheight$height
head(height)

# 단계2 : 기술통계량/결측치 확인
length(height) #50
summary(height) # 149.4 

x1 <- na.omit(height)
x1 # 정제 데이터 
mean(x1) # 149.4 -> 평균신장

# 단계3 : 정규성 검정
shapiro.test(x1)
# p-value = 0.0001853 < 0.05 귀무가설 기각 정규분포가 아니다.

# 단계4 : 가설검정 - 양측검정
t.test(x,mu) # 정규분포
wilcox.test(x,mu) # 비정규분포
wilcox.test(x1,148.5)
# W = 32, p-value = 0.6555 > 0.05 귀무가설 채택 => A중학교와 2학년 여학생과 우리나라 전체 중학교 2학년 여학생의 키 차이는 없다.


# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서
# 만족도에 차이가 있는가를 검정하시오.

# 힌트) 두 집단 비율 차이 검정
#  조건) 파일명 : two_sample.csv, 변수명 : gender(1,2), survey(0,1)
# gender : 남학생(1), 여학생(2)
# survey : 불만(0), 만족(1)
# prop.test('성공횟수', '시행횟수')

# 단계1 : 실습데이터 가져오기
getwd()
data <- read.csv("two_sample.csv")
data
head(data) # 변수명 확인

# 단계2 : 두 집단 subset 작성
data$gender
data$survey

x<-data$gender
y<-data$survey

table(x)
# x(성별)
# 1   2 
# 174 126 
table(y)
# y(만족도)
# 0   1 
# 55 245 

table(x,y)
#       y
# x   0   1
# 1  36 138
# 2  19 107

# 단계3 : 두집단 비율차이검증 : prop.test()
prop.test(c(138,107),c(174,126))
# X-squared = 1.1845, df = 1, p-value = 0.2765 
# p-value = 0.2765 > 0.05 귀무가설 채택 : 유의성 없음. 만족도 차이 없음.







# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.

#힌트) 두 집단 평균 차이 검정
#조건1) 파일 : twomethod.csv
#조건2) 변수 : method : 교육방법, score : 시험성적
#조건3) 모델 : 교육방법(명목)  ->  시험성적(비율)
#조건4) 전처리 : 결측치 제거 : 평균으로 대체 

#단계1. 실습파일 가져오기
Data <- read.csv("twomethod.csv", header=TRUE)
head(Data) #3개 변수 확인 -> id method score

#단계2. 두 집단 subset 작성
# subset 생성 
Data <- Data[c('method', 'score')] 
# score 결측치 처리 
Data$score <- ifelse(is.na(Data$score), mean(Data$score, na.rm=T), Data$score)
summary(Data)







#단계3. 데이터 분리

# 1) 집단(교육방법)으로 분리
result <- subset(Data, !is.na(score), c(method,score))
result

a <- subset(result, method == 1)
b <- subset(result, method == 2)

# 2) 교육방법에서 시험성적 추출
a1 <- a$score
b1 <- b$score

length(a1)
length(b1)


#단계4 : 분포모양 검정
var.test(a1,b1)
# p-value = 0.7302 > 0.05 귀무가설 채택 : 유의성 없음 => 동질적이다.

#단계5: 가설검정
t.test(a1,b1)
# t = -5.4487, df = 46.604, p-value = 1.859e-06 < 0.05 귀무가설 기각, 연구가설 대립가설 채택 : 유의성 있음.

# 대립가설 : a1 > b1
t.test(a1,b1,alternative = "greater",conf.int=TRUE, conf.level=0.95)
# p-value = 1 귀무가설 채택 : 유의성 없음

# 대립가설 : a1 < b1
t.test(a1,b1,alternative = "less",conf.int=TRUE, conf.level=0.95)
# p-value = 9.295e-07 귀무가설 기각 : 연구가설 채택 : 유의성 있음. 
# 즉 a1 < b1 이다.
mean(a1)
mean(b1)


# 05. airqualiity 대상으로 월별로 오존량에 차이가 있는지 검정하시오

airquality
str(airquality)
# Ozone -> y : 연속형 변수
# Month -> x : 집단 변수
table(airquality$Month)

# 단계 1 : 전처리 (결측치 제거)
summary(airquality)
dataset <- na.omit(airquality)

# 단계 2 : 동질성 검정
bartlett.test(Ozone ~ Month, data = dataset )
# p-value =  0.007395 < 0.05 유의성 있음. 귀무가설 기각 , 동질성 없음.

# 단계 3 : 분산분석 (모수 vs 비모수)
kruskal.test(Ozone ~ Month, data = dataset )
# p-value= 2.742e-05 < 0.05 유의성 있음. 적어도 하나 이상 차이 보인다.

# 단계 4 : 사후 검정
dataset %>% group_by(Month) %>% summarise(med = median(Ozone))


