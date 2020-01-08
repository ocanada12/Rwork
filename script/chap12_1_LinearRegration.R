# chap12_1_LinearRegration

###################################
# 1. 단순 선형 회귀 분석
###################################
# - 독립변수(1) -> 종속변수(1) 미치는 영향 분석

# 독립변수 두개이상은 다중회귀분석

setwd("c:/Rwork/data")
product <- read.csv("product.csv")
str(product)
# 'data.frame' : 264 obs. of 3 variables :

# 1) x,y 변수 선택
x <- product$"제품_적절성" # 독립변수
y <- product$"제품_만족도" # 종속변수

df <- data.frame(x,y)
df

# 2) 회귀모델 생성
model <- lm(y ~ x, data = df)
model

# Coefficients : 회귀계수 (기울기와 절편)
# (Intercept)           x  
# 0.7789(절편)       0.7393(기울기)

# 회귀방정식(y) = 0.7393 * x + 0.7789
head(df)
# x = 4, y = 3
x = 4
Y = 3
y = 0.7393*x +0.7789 # 예측치
y # 3.7361

# 오차 = 관측치(정담) - 예측치
err <- Y-y
err # -0.7361
abs(err)
mse = mean(err^2) # 평균제곱오차
mse # 0.5418432 # 제곱 (부호 #, 패널티)

names(model) # 12칼럼 제공
# "coefficients" : 계수
# "residuals" : 오차(잔차)
# "effects" : 적합치(예측치)

model$coefficients
model$residuals[1] # 오차
model$fitted.values[1] # 예측치 3.735963 


# 3) 회귀모델 분석
summary(model)

# <회귀모델 분석 순서> summary에서 확인..
# 1. F검정 통계량 : 모델의 유의성 검정 /// 374 on 1 and 262 DF,  p-value : 2.2e-16 < 0.05 => 유의하다. => 0에 가깝다
# 2. 모델의 설명력 : Adjusted R-squared (0.5865) /// 상관계수에다 제곱을 가한 값 Adjusted R-squared 1에 가까울수록 예측력이 높다. 
# 3. x변수 유의성 검정 : 영향력 판단 (p < 0.05)

# R-squared = R^2
R <- sqrt(0.5865)
R # 0.7658 높은 상관성

# 4) 회귀선 : 회귀방정식에 의해서 구ㅐ진 직선(예측치)

# x, y 산점도 
plot(df$x,df$y)

# 회귀선(직선)
abline(model, col ="red") # 직전에 만든 그림에 함수추가하는 것



###################################
# 2. 다중 선형 회귀 분석
###################################
# - 독립변수(n) -> 종속변수(1) 미치는 영향 분석

install.packages("car")
library(car)

head(Prestige)
str(Prestige)
# 102개의 직업군 대상(6개변수 조사) : 교육수준, 수입, 여성 비율, 평판, 인구수(직원수), 유형(x)
# 회귀분석 대상에서 타입은 쓸수없음. 범주형은 사용할수없음 타입을 제외한 나머지 다섯개 변수
row.names(Prestige)


# 1) subset
newdata <- Prestige[c(1:4)]
str(newdata)

# 2) 상관분석
cor(newdata)
# 교육수준하고 평판이 가장 상관성이 높다.(0.8)
#         education(x1)   income    women(x2)   prestige(x3)
# education 1.00000000  0.5775802  0.06185286  0.8501769
# income(y) 0.57758023  1.0000000 -0.44105927  0.7149057

# 3) 회귀모델
model <- lm(income ~ education + women + prestige, data = newdata)
model

income <- 12351 # Y(정답)
education <- 13.11 # x1
women <- 11.16 # x2
prestige <- 68.8 # x3

head(newdata) 

# 예측치 : 회귀방정식
y_pred <- 177.2 * education + -50.9 * women + 141.4 * prestige + (-253.8)
y_pred # 11229.57(예측치)

err <- income - y_pred
err # 1121.431


# 4) 회귀모델분석
summary(model)
# 모델의 유의성 검정 :p-value: < 2.2e-16 유의하다
# 모델의 설명력 : Adjusted R-squared:  0.6323 중간이상의 설명력
# x변수의 유의성 검정 : 
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347(영향없음)    
# women        -50.896      8.556  -5.948    4.19e-08(-영향있음) 
# prestige     141.435     29.910   4.729    7.58e-06(+영향있음)
# 즉, 여성의 비율이 낮을수록, 평판이 좋을수록 소득이 높으며, 교육수준과는 관계없다.

# 현재 x변수 3개중에서 1개를 빼고 (유의한변수)2개를 만드는것이 효과적



###################################
# 3. 변수 선택법
###################################

# - 최적 모델을 위한 x변수 선택

newdata2 <- Prestige[c(1:5)]

dim(newdata2)

library(MASS)
model2 <- lm(income ~ .,data = newdata2)


step <- stepAIC(model2, direction = "both") # 최적의 변수 자동 제공 aic값 가장 적은것?
step


model3 <- lm(formula = income ~ women + prestige, data = newdata2)
model3

summary(model3)
# F검정 통계량 : p-value: < 2.2e-16
# Adjusted R-squared : 0.6327
# women        -5.953  4.02e-08 
# prestige     11.067  < 2e-16 

0.6327 - 0.6323 
# 4e-04
# 변수 선택의 결과 : 미묘한 설명력의 변화 , 조금 좋은 예측력을 갖게 된다.



###################################
# 4. 기계학습
###################################

iris_data <- iris[-5]
str(iris_data)
# 'data.frame':	150 obs. of  4 variables:

# 1) train / test set(70 vs 30)
idx <- sample(x = nrow(iris_data), size = nrow(iris_data)*0.7, replace=FALSE)
idx

train <- iris_data[idx,]
test <- iris_data[-idx,]

dim(train) # 105 4(y+x)
dim(test) # 45 4(y+x)

# 홀드아웃방식
# 올해부터 데이터분석에 대한 기사 시험도 나온다고 함?

# 2) model(train)
model <- lm(Sepal.Length ~ ., data = train)

# 3) model 평가(test)
y_pred <- predict(model,test) # y의 예측치 test안에는 y를 예측할수있는 x가 있어야함
y_pred
y_true <- test$Sepal.Length # y의 정답치
y_true

# 평가 : mse(평균제곱오차)
mse <- mean((y_true - y_pred)^2)
mse # 오차의 값이 0에수렴할수록 예측력이 좋은것

cor(y_true, y_pred) # 높은 상관계수

plot(y_true, col = "blue", type ="o", pch = 18)
points(y_pred, col = "red", type = "o", pch = 19)
title("real value vs predict value")
legend("topleft",legend=c("real","pred"),col=c("blue","red"),pch = c(18,19))

##########################################
##  5. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링  
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 잔차의 등분산성 검정
#   2) 잔차의 정규성 검정 
#   3) 잔차의 독립성(자기상관) 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris) # 아이리스에 소속된 변수 lm이라는 함수로 회귀 모델 만듦
model
names(model)


# 3. 모형의 잔차검정
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
plot(model, which =  1) 
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
res <- model$residuals
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qqnorm(res)

# (3) 잔차의 독립성(자기상관 검정 : Durbin-Watson) 잔차와 잔차간의 영향을 미치는가 를 검증하는것 잔차간에는 관련성이 없는것이 좋다.
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값  일반적으로 2~ 4 사이 의 경우 귀무가설 채택
# DW = 2.0604, p-value = 0.6013 > 0.05 

# 4. 다중공선성 검사 
# - 독립 x변수가 두개이상일때 발생 독립변수간에 강한 상관관계로 인해서 발생하는 문제
# - ex) 생년월일, 생일 

library(car) # 패키지안의 함수로 확인할수있음. 
sqrt(vif(model)) > 2 # TRUE 

# 5. 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가


# 아이리스는 모두 통과했음


###############################################
# 15_2. 로지스틱 회귀분석(Logistic Regression) 
###############################################

# 목적 : 일반 회귀분석과 동일하게 종속변수와 독립변수 간의 관계를 나타내어 
# 향후 예측 모델을 생성하는데 있다.

# 차이점 : 종속변수가 범주형 데이터를 대상으로 하며 입력 데이터가 주어졌을 때
# 해당 데이터의결과가 특정 분류로 나눠지기 때문에 분류분석 방법으로 분류된다.
# 유형 : 이항형(종속변수가 2개 범주-Yes/No), 다항형(종속변수가 3개 이상 범주-iris 꽃 종류)
# 다항형 로지스틱 회귀분석 : nnet, rpart 패키지 이용 
# a : 0.6,  b:0.3,  c:0.1 -> a 분류 

# 분야 : 의료, 통신, 기타 데이터마이닝

# 선형회귀분석 vs 로지스틱 회귀분석 
# 1. 로지스틱 회귀분석 결과는 0과 1로 나타난다.(이항형)
# 2. 정규분포 대신에 이항분포를 따른다.
# 3. 로직스틱 모형 적용 : 변수[-무한대, +무한대] -> 변수[0,1]사이에 있도록 하는 모형 
#    -> 로짓변환 : 출력범위를 [0,1]로 조정
# 4. 종속변수가 2개 이상인 경우 더미변수(dummy variable)로 변환하여 0과 1를 갖도록한다.
#    예) 혈액형 AB인 경우 -> [1,0,0,0] AB(1) -> A,B,O(0)



# 로짓변환 vs sigmoid function




# 1) 로짓변환 : 오즈비에 log(자연로그)함수 적용
p = 0.5 # 성공확률
odds_ratio <- p / (1-p)
logit1 <- log(odds_ratio) # 0

p = 1 # 성공확률
odds_ratio <- p / (1-p)
logit2 <- log(odds_ratio) # Inf

p = 0 # 성공확률
odds_ratio <- p / (1-p)
logit3 <- log(odds_ratio) # -Inf
# [정리] p = 0.5 : 0, p > 0.5 : Inf , p < 0.5 : -Inf





# 2) sigmoid function
sig1 <- 1 / (1 + exp(-logit1))
sig1 # 0.5

sig2 <- 1 / (1 + exp(-logit2))
sig2 # 1

sig3 <- 1 / (1 + exp(-logit3))
sig3 # 0
# [정리] logit = 0 :0.5 , logit = Inf : 1 , -Inf = 0
# 값의 범위 : 0 ~ 1 확률값(cut off = 0.5) -> 이항분류에 적합





# 단계1. 데이터 가져오기
weather = read.csv("C:/Rwork/data/weather.csv", stringsAsFactors = F) 
dim(weather)  # 366  15
head(weather)
str(weather) 





# chr 칼럼, Date, RainToday 칼럼 제거 
weather_df <- weather[, c(-1, -6, -8, -14)]
str(weather_df)







# RainTomorrow 칼럼 -> 로지스틱 회귀분석 결과(0,1)에 맞게 더미변수 생성      
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)










#  단계2.  데이터 셈플링 (7:3 비율) 
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]


#  단계3.  로지스틱  회귀모델 생성 : 학습데이터 
weater_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
weater_model  # binomial 이항이라는 뜻

summary(weater_model) 


# 단계4. 로지스틱  회귀모델 예측치 생성 : 검정데이터 
# newdata=test : 새로운 데이터 셋, type="response" : 0~1 확률값으로 예측 
pred <- predict(weater_model, newdata=test, type="response")  
pred 
range(pred, na.rm = T) # 0.001142558 0.989754258 0과 1사이의 값
summary(pred) 
str(pred) # Named num [1:110] => 벡터구조

# cut off = 0.5 적용 
cpred <- ifelse(pred >= 0.5, 1, 0) # 예측치(0,1)
y_true <- test$RainTomorrow # 정답(0,1)


# 교차분할표(confusion matrix)
t <- table(y_true,cpred)
#           cpred
# y_true    0   1
# 0         93  4 = 97
# 1         5   7 = 12

# model 평가 : 분류정확도
acc <- (93+7) / sum(t)
cat('accracy =',acc) # accracy = 0.9174312

(4+5)/sum(t) # 0.08256881

# 특이도 : NO -> NO
acc_no <- 93 /97 # 0.9587629

# 재현율(민감도) : YES -> YES
recall <- 7/12 # 0.5833333
recall

# 정확률 : model(yes) -> yes
precision <- 7/11

# F1 Score : no != yes(불균형) 
f1_score = 2*((precision *recall)/(precision +recall))
f1_score # 0.6086957
### ROC Curve를 이용한 모형평가(분류정확도)  ####
# Receiver Operating Characteristic

install.packages("ROCR")
library(ROCR)

# ROCR 패키지 제공 함수 : prediction() -> performance
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)






################################################
#### 다항형 로지스틱 회귀 분석
################################################

install.packages("nnet") # 신경망을 기반으로 한 라이브러리
library(nnet)

# 이항분류 vs 다항분류
# sigmoid : 이항분류(0 ~ 1 확률, cutoff = 0.5) 
# softmax : 다항분류(0 ~ 1 확률, 확률합 = 1) 
# ex) Class1 = 0.9, Class2 = 0.1, Class3 = -.1 

names(iris)
idx <- sample(nrow(iris), nrow(iris)*0.7)
train_iris <- iris[idx, ]
test_iris <- iris[-idx, ]

# 다항분류 model : train
model <- multinom(Species ~ ., data = train_iris)

# model 평가 : test
y_pred <- predict(model, test_iris, type ="probs")
# type = "probs" : 0 ~ 1 확률 (합 = 1) -> softmax

str(y_pred)
#       setosa      versicolor    virginica
# 2   9.999991e-01 + 9.045336e-07 + 2.157182e-26 = 1
# 5   1.000000e+00 4.659867e-09 1.399179e-29
# 7   9.999998e-01 1.515113e-07 8.812742e-27
# 8   9.999999e-01 8.691505e-08 9.308345e-28
# 12  9.999990e-01 1.007701e-06 2.722041e-26

y_pred <- predict(model, test_iris, type ="class")
# type = "class" : setosa, versicolor, vitginia 
range(y_pred)
# 1.203234e-35(0) 1.000000e+00(1) 

y_true <- test_iris$Species

t <- table(y_true, y_pred)

acc <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)
acc

