# chap13_2_RandomForest

# 패키지 설치
install.packages("randomForest")
library(randomForest)

names(iris)

##########################
## 분류트리(y변수 : 범주형)
##########################

# 150개의 관측치를 500개의 데이터셋으로 
# 랜덤하게 샘플링 500개의 트리생성

# 1. model
model <- randomForest(Species ~ ., data = iris)
# 500 dataset 생성 -> 500 tree(model) -> 예측치

model
# Number of trees : 500 -> 트리 수
# No. of variables tried at each split: 2 -> 노드 분류에 사용한 변수 갯수

# OOB estimate of  error rate : 4%
# 분류정확도 : 96%
# Confusion matrix :
# 

(50+47+47)/150
?randomForest
model2 <- randomForest(Species ~ ., data = iris,
                       ntree = 400, mtry =2,
                       imporance = TRUE,
                       na.action = na.omit)
  
model2 

importance(model2) 
# MeanDecreaseGini : 노드 불순도(불확실성) 개선에 기여하는 변수 (크면클수록 y에 지대한 영향을 미친다.)
# Sepal.Length         9.251432
# Sepal.Width          2.245477
# Petal.Length        44.394871
# Petal.Width         43.270620
# y의 가장 중요한 변수가 3번쟤 4번쨰

varImpPlot(model2)

##########################
## 회귀트리 (y변수 : 연속형)
##########################
library(MASS)
data("Boston")#crim : 도시 1인당 범죄율 

str(Boston)

#zn : 25,000 평방피트를 초과하는 거주지역 비율
#indus : 비상업지역이 점유하고 있는 토지 비율  
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
#nox : 10ppm 당 농축 일산화질소 
#rm : 주택 1가구당 평균 방의 개수 
#age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv(y) : 소유 주택가격 중앙값 (단위 : $1,000)

# y = medv
# x = 13칼럼

# 하나의 노드를 가지고 분류조건에의해서 노드가 분류되는데 x변수가 
# 13개 있는데 다쓸것이냐? 아니면 상위 몇개만 쓸것이냐(mtry)

p = 14
(1/3) * p # 4.6666

mtry = round((1/3)*p) # 반올림
mtry = floor((1/3)*p) # 절삭
mtry # 4 

model3 <- randomForest(medv ~ ., data = Boston,
                       ntree = 500, mtry = mtry,
                       imporance = TRUE,
                       na.action = na.omit)
model3

# No. of variables tried at each split: 4
# Mean of squared residuals: 10.16085 ----> mse 오차
# % Var explained: 87.96

# 중요변수 시각화
varImpPlot(model3)



