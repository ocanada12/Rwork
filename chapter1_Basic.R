# chapter1_Basic

# 수업내용 

# 1. session 정보
# 2. R실행방법
# 3. 패키지와 데이터셋
# 4. 변수와 자료형
# 5. 기본함수 사용 및 작업 공간


# 1. session 정보
# session : R의 실행환경
sessionInfo() 
# R ver, 다국어(locale)정보, base package


# 2. R 실행방법

# 주요 단축키
# script 실행 : ctrl + Enter or ctrl + R
# 자동완성 : ctrl + space
# 저장 : ctrl + s
# 여러줄 주석 : ctrl + shift + c


# 
# a <- 10
# b <- 20
# c <- a + b
# print(c) # 30
# 

# 1) 줄단위 실행
a <-rnorm(n=20) # <- or = 무작위로 수만들기
a 
hist(a) #히스토그램 만들기
mean(a) # 평균
# 2) 블럭단위 실행
getwd()#현재 잡업경로 볼 수 있음
pdf("test.pdf") # open
hist(a) # 히스토그램
dev.off() # close



# 3. 패키지와 데이터 셋

# 1) 패키지 = function + [dataset]

# 사용가능한 패키지 
dim(available.packages()) 
# 15328(row) 17(col)


#패키지 설치/사용

install.packages("stringr") #문자열처리

# 패키지 in memory 
library(stringr)


#사용가능한 패키지 확인
search()

#설치 위치
.libPaths()

#패키지 활용
str <- "서봉석29이순신55유관순25알랙산더255"
str

# 이름 추출
str_extract_all(str,"[가-힣]{3}")

# 나이 추출
str_extract_all(str, "[0-9]{2}")
str_extract_all(str, "[0-9]{3}")

#패키지 삭제
remove.packages("stringr")

# 2) 데이터셋
data()
data("Nile") # in memory
Nile
length(Nile) #[1] 100  -> 1차원, 데이터 100개
mode(Nile) # "numeric"
plot(Nile)
mean(Nile) # 919.35


# 4. 변수와 자료형

# 1) 변수(variable) : 메모리 주소를 저장
# - R의 모든 변수는 객체(참조변수)
# - 변수 선언 시 type 없음
a <- c(1:10000)
a


# 2) 변수 작성 규칙
# - 첫자는 영문자 또는 특수문자(__)
# - 점(.)을 사용(lr.model)
# - 예약어 사용 불가
# - eothanswk rnqns : num or NUM

var1 <- 0 # var1 = 0
#java) int var1 = 0
var1 <- 1
var1

var2 <- 20
var3 <- 30
var2; var3 # 두개 동시에 출력

# 객체. 멤버
member.id <- "hong"
member.name <-"홍길동"
member.pwd <-"1234"

num <- 10
NUM <- 100
num; NUM

# scala(1) vs vector(n)
name <- c("홍길동","이순신","유관순")
name #[1] "홍길동" "이순신" "유관순"

name[2] # "이순신"

# tensor : scala(0), vector(1), matrix(2)

# 변수 목록
ls()

# 3) 자료형
# - 숫자형, 문자형, 논리형

int <- 100 # 숫자형(연산, 차트)
string <- "대한민국" # ='대한민국' 
boolean <- TRUE # T, FALSE(F)

mode(int) # "numeric"
mode(string) # "character"
mode(boolean) # "logical"

# is.xxx()
is.numeric(int) # TRUE
is.character(string) # TRUE
is.numeric(boolean) # FALSE

X <- C(100,90,NA,65,78) # NA : 결측치(9999)
is.na(x)

# 4) 자료형 변환(캐스팅)
num <- c(10,20,30,"40")
mode(num) # numeric
num <- as.numeric(num)
mean(num)

plot(num)

#2) 요인형(Factor)
# - 동일한 값을 범주로 갖는 집단변수 생성
# ex) 성별) 남(0), 여(1) -> 더미변수

gender <- c("M","F","M","F","M")
mode(gender)
plot(gender)


# 요인형 변환 : 문자열 -> 요인형
fgender <- as.factor(gender)
mode(fgender)
fgender
# [1] M F M F M
# Levels: F M // 영문자의 알파벳순서로 자동 결정 (변경가능) 
str(fgender)
# Factor w/ 2 levels "F"(1),"M"(2): 2 1 2 1 2
plot(fgender)

x <- c('M','F')
fgender2 <- factor(gender, levels = x)
str(fgender2)
# Factor w/ 2 levels "M","F": 1 2 1 2 1


# mode vs class
# mode() : 자료형 반환
# class() : 자료구조 반환
mode(fgender) # "numeric"
class(fgender) # "factor"

# Factor 형 고려사항
num <- c(4,2,4,2)
mode(num)


# 숫자형 -> 요인형
fnum <- as.factor(num)
fnum
# [1] 4(2) 2(1) 4 2
# Levels: 2 4
str(fnum)

# 요인형 -> 숫자형
num2 <- as.numeric(fnum)
num2 # 2 1 2 1
# 요인형 -> 문자형 -> 숫자형 // 요인형에서 바로 숫자형으로 변경하면 2121이 반환되기 때문에 문자형을 거쳐야한다.
snum <- as.character(fnum)
snum
num2 <- as.numeric(snum)
num2 

# 5. 기본함수 사용 및 작업공간


# 1) 함수 도움말
mean(10,20,30,NA) # 평균 - 10
x <- c(10, 20, 30, NA)
mean(x, na.rm = TRUE)


help(mean)
?mean

sum(x,na.rm=TRUE)

# 2)작업공간
getwd()
setwd("C:/Rwork/data")
getwd()

test <- read.csv("test.csv")
test

str(test)
# 'data.frame':	402 obs. of  5 variables:
# obs : 관측치 402 행
# variable : 변수, 변인 5(열)
# 관측치의 변수가 402개 402 바이 5차원의 csv 파일
