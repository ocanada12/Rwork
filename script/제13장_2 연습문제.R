##########################
## 제13-2장 RM 연습문제 
##########################

# 01. weatherAUS 데이터셋을 대상으로 100개의 Tree와 2개의 분류변수를 파라미터로 지정하여 
# 모델을 생성하고, 분류정확도를 구하시오.
#  조건> subset 생성 : 1,2,22,23 칼럼 제외 

setwd("c:/Rwork/data")
weatherAUS = read.csv("weatherAUS.csv") 
str(weatherAUS)
weatherAUS1 <- weatherAUS[-c(1,2,22,23)]
str(weatherAUS1)

model3 <- randomForest(RainTomorrow ~ ., data = weatherAUS1,
                       ntree = 100, mtry =2,
                       imporance = TRUE,
                       na.action = na.omit)

model3

#        No  Yes  class.error
# No  12604  822  0.06122449
# Yes  1648 2304  0.41700405
# err : 14.43%

(12604 + 2304)/nrow(weatherAUS1) 0.

# 02. 변수의 중요도 평가를 통해서 가장 중요한 변수를 확인하고, 시각화 하시오. 
varImpPlot(model3)
# Humudufy3pm

