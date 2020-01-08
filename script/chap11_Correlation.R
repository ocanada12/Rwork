# chap11_Correlation

# dataset 가져오기
product <- read.csv("product.csv")
# 친밀도,적절성, 만족도 : 5점 (등간척도)
str(product) 

table(product)

# 1. 상관분석
# - 두 변수의 확률분포의 상관관계의 정도를 나타내는 계수(-1 ~ +1)


corr <- cor(product, method = "pearson") # 상관계수 행렬 제공
# 제품의 적절성과 제품의 만족도가 가장 높은 상관성을 보여줌 (0.76)

cor(x=product$제품_적절성, y=product$제품_만족도)

# 2. 상관분석 시각화
install.packages("corrplot")
library(corrplot)
corrplot(corr, method = "number")
corrplot(corr, method = "square")
corrplot(corr, method = "circle")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product)

# 3. 공분산
# - 두 변수의 확률분포의 상관관계의 정도를 나타내는 값
cov(product) # 공분산 행렬
# 대각선제외하고 가장 큰 값

# 상관관계 vs 공분산
# 상관관계 : 크기, 방향(-, +) 
# 공분산 : 크기

#####################
#### IRIS 적용
#####################
cor(iris[-5])

# 양의 상관관계
plot(iris$Petal.Length, iris$Petal.Width)

# 음의 상관관계
plot(iris$Sepal.Width, iris$Petal.Length)

