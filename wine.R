#Advanced Analytics 
#mid exam project

# wine
wine = read.csv("wine.csv", header = TRUE, sep = ",")
head(wine)
str(wine) # 查看 wine 內部結構
wine[!complete.cases(wine),]  # 檢查是否有 NA 的資料

# wine--迴歸分析---------------------------------------------------------
#迴歸分析

summary(wine) # 查看基本統計量
library(ggplot2)
library(GGally)
ggpairs(wine)


# 畫出 XY 散佈圖，依據 Species 上色
require(ggplot2)
qplot(x = Petal.Length,
      y = Petal.Width,
      data = wine,
      color = Species)

# 建立迴歸模型
wine.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
              data = wine)
# 查看模型配適結果
summary(wine.lm)

library(ggfortify)

# 畫出模型診斷用的圖
autoplot(wine.lm)

# 常態性檢定
shapiro.test(wine.lm$residual)

# 殘差獨立性檢定
library(car)
require(car)
durbinWatsonTest(wine.lm)

# 殘差變異數同質性檢定
require(car)
ncvTest(wine.lm)

# 新觀測值
new.wine <- data.frame(Sepal.Width=3.1, Petal.Length=1.6, Petal.Width=0.3)
new.wine

# 預測資料
predict(wine.lm, new.wine)
