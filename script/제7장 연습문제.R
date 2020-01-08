#################################
## <제7장 연습문제>
################################# 

# 01. 본문에서 생성된 dataset2의 resident 칼럼을 대상으로 NA 값을 제거하시오.(힌트 : subset()함수 이용)
dataset2 <- subset(dataset,!is.na(resident))
dataset2
summary(dataset2)

# 02. dataset2의 gender 칼럼을 대상으로 1->"남자", 2->"여자" 형태로 코딩 변경하여 
# gender2 칼럼에 추가하고, 파이 차트로 결과를 확인하시오.
dataset2 
gender2 <- ifelse(dataset2$gender == 1, "남자", "여자")
gender2
dataset2$gender2 <- gender2
dataset2
pie(table(gender2))

# 03. 나이를 30세 이하 -> 1, 31~55 -> 2, 56이상 -> 3 으로 코딩변경하여 age3 칼럼에 추가한 후 
# age, age2 칼럼만 확인하시오.
summary(dataset2)

dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30] <- 2
dataset2$age3[dataset2$age > 55] <- 3

dataset2$age3 <- age3
dataset2$age3

# 04. ggplot2 패키지에서 제공하는 mpg 데이터셋의 hwy 변수를 대상으로 이상치를 발견하고, 제거하시오.
install.packages("ggplot2")
library(ggplot2)
mpg <- as.data.frame(mpg)

summary(mpg)
boxplot(mpg$hwy)$stats # 12 37
mpg2 <- subset(mpg,hwy >= 12 & hwy <=37)
boxplot(mpg2$hwy) # 12 37

# 단계1) 상자그래프와 통계량 

# 단계2) 이상치 제거 


# 05. iris 데이터셋을 대상으로 8:2 비율로 sampling 하여 train과 test셋으로 나눠보세요
dim(iris) # 150 5
nrow(iris)
train1 <- sample(x=nrow(iris),size = nrow(iris)*0.8)
train1

test1 <- sample(x=nrow(iris), size = nrow(iris)*0.2)
test1

train <-df[train1,]
test <-df[test1,]
dim(train)
dim(test)

str(train1)
str(train)

