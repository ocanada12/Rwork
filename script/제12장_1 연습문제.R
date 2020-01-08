#################################
## <제12장 연습문제>
################################# 

# 01. mpg의 엔진크기(displ)가 고속도록주행마일(hwy)에 어떤 영향을 미치는가?    

library(ggplot2)
data(mpg)
str(mpg)


# <조건1> 단순선형회귀모델 생성
x <- mpg$displ
y <- mpg$hwy
df <- data.frame(x,y)
df
model <- lm(y~x,data=df)
model


# <조건2> 회귀선 시각화  
plot(df$x,df$y)
#  text 추가
text(df$x,df$y,labels=df$y,cex=0.7,pos=1,col="blue")

abline(model,col = "red")


# <조건3> 회귀분석 결과 해석 : 모델 유의성검정, 설명력, x변수 유의성 검정  
summary(model) # 음의 상관관계
# 유의성 검정 0.05보다 작음 유의성 있음
# 설명력 0.585
# x변수 유의성 : t= -18, p = 2e-16 0.05보다 작음 유의성 있음
# 엔진크기가 클수록 주행거리가 줄어든다? 유의미한 차이를 보인다.







# 02. product 데이터셋을 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
setwd("c:/Rwork/data")
product <- read.csv("product.csv", header=TRUE)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
a <- sample(x=nrow(product), size = nrow(product)*0.7,replace = FALSE)
a

train <- product[a,]
test <- product[-a,]

dim(product)
dim(train)
dim(test)



#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도

model <- lm("제품_만족도" ~ ., data=train)


#  단계3 : 검정데이터 이용 모델 예측치 생성 

y_pred <- predict(model,test)
y_pred
y_true <- test$"제품_만족도"
y_true

#  단계4 : 모델 평가 : cor()함수 이용  
mse = mean((y_true - y_pred)^2)
mse # 0.309906
cor(y_true,y_pred) # 0.6987649 

# y real value vs y prediction
plot(y_true, col = "blue", type ="o", pch = 18)
points(y_pred, col = "red", type = "o", pch = 19)
title("real value vs predict value")
legend("topleft",legend=c("real","pred"),col=c("blue","red"),pch = c(18,19))
# 03. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
# carrat
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)

formula = price ~ carat + table + depth 
model <- lm(formula = formula,  data=diamonds)
summary(model) # 모델 평가

