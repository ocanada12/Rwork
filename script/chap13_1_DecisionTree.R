# chap13_1_DecisionTree

# 관련 패키지 설치
install.packages("rpart")
library(rpart)

# tree 시각화 패키지 
install.packages("rpart.plot")
library(rpart.plot)

# 1. dataset(train/test) : iris
idx <- sample(nrow(iris),nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]
names(iris)
# 2. 분류모델
model <- rpart(Species ~ ., data = train)
model

# 분류모델 시각화
rpart.plot(model)
# [중요변수] 가장 주요변수 : "Petal.Length"

# * 표시는 종단노드를 가리킨다.
# * 표시가 없으면 내부노드임
# 4개의 변수를 모두 사용하지 않고 페탈렝쓰 하나만으로도 분류가 가능
# 가장 영향력있는 변수가 가장 상위로 오게됨
# 영향력 선정원리 : 엔트로피의 값에 의해 선정


# 3. 모델 평가
y_pred <- predict(model,test) # 비율 예측치
y_pred

y_pred <- predict(model,test,type="class")
y_pred

y_true <- test$Species

# 교차분할표(confusion table)
table(y_true, y_pred)

acc <- (12+16+15)/nrow(test)
acc # 0.9555556

##########################
# Titanic 분류분석
##########################
setwd("c:/Rwork/data")
titanic3 <- read.csv("titanic3.csv")
str(titanic3)

# int -> Factor(범주형)
titanic3$survived <- factor(titanic3$survived,levels = c(0,1))
table(titanic3$survived)

# 0   1 
# 809 500 
809 / 1309 # 0.618029

# subset 생성 : 불필요한 칼럼 제외
titanic <- titanic3[-c(3,8,10,12:14)]
str(titanic)
# 'data.frame':	1309 obs. of  8 variables:
#  $ survived: Factor w/ 2

# train/test set 
idx <- sample(nrow(titanic),nrow(titanic)*0.8)
train <- titanic[idx,]
test <- titanic[-idx,]

model <- rpart(survived ~.,data = train)

rpart.plot(model)

y_pred <- predict(model,test,type="class")
y_true <- test$survived

table(y_true,y_pred)
#          y_pred
# y_true   0    1
#      0  165  11
#      1  29   57

acc <- (165+57)/nrow(test)
acc # 0.8473282

table(test$survived)
#   0   1 
# 176  86 

# 재현율  yes yes
recall <- (57) / (29+57)


# 정확률 ues ues
precision <- 57 / (11+57)
  
  
# F1 score
f1_score = 2*((precision * recall )/(precision + recall))
f1_score

N