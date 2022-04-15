#Advanced Analytics 
#mid exam project

# tomato-------------------------------------------------------------

TomatoFirst = read.csv("TomatoFirst.csv", header = TRUE, sep = ",")
head(TomatoFirst)




# wine---------------------------------------------------------------


wine = read.csv("wine.csv", header = TRUE, sep = ",")
head(wine)




# iris--迴歸分析---------------------------------------------------------
#迴歸分析

head(iris)
str(iris) # 查看 iris 內部結構
iris[!complete.cases(iris),]  # 檢查是否有 NA 的資料

summary(iris) # 查看基本統計量
library(ggplot2)
library(GGally)
ggpairs(iris)

# 畫出 XY 散佈圖
require(ggplot2)
qplot(x = Petal.Length,
      y = Petal.Width,
      data = iris)

# 畫出 XY 散佈圖，依據 Species 上色
require(ggplot2)
qplot(x = Petal.Length,
      y = Petal.Width,
      data = iris,
      color = Species)

# 建立迴歸模型
iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
              data = iris)
# 查看模型配適結果
summary(iris.lm)

library(ggfortify)

# 畫出模型診斷用的圖
autoplot(iris.lm)

# 常態性檢定
shapiro.test(iris.lm$residual)

# 殘差獨立性檢定
library(car)
require(car)
durbinWatsonTest(iris.lm)

# 殘差變異數同質性檢定
require(car)
ncvTest(iris.lm)

# 新觀測值
new.iris <- data.frame(Sepal.Width=3.1, Petal.Length=1.6, Petal.Width=0.3)
new.iris

# 預測資料
predict(iris.lm, new.iris)
