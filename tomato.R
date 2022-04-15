#Advanced Analytics 
#mid exam project

# tomato
tomato = read.csv("TomatoFirst.csv", header = TRUE, sep = ",")
head(tomato)
str(tomato) # 查看 tomato 內部結構
tomato[!complete.cases(tomato),]  # 檢查是否有 NA 的資料

# tomato--迴歸分析---------------------------------------------------------
#迴歸分析

summary(tomato) # 查看基本統計量
library(ggplot2)
library(GGally)
ggpairs(tomato)


# 畫出 XY 散佈圖，依據 Species 上色
require(ggplot2)
qplot(x = Petal.Length,
      y = Petal.Width,
      data = tomato,
      color = Species)


#ERROR---------------------------------
# 建立迴歸模型
tomato.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
              data = tomato)
# 查看模型配適結果
summary(tomato.lm)

library(ggfortify)

# 畫出模型診斷用的圖
autoplot(tomato.lm)

# 常態性檢定
shapiro.test(tomato.lm$residual)

# 殘差獨立性檢定
library(car)
require(car)
durbinWatsonTest(tomato.lm)

# 殘差變異數同質性檢定
require(car)
ncvTest(tomato.lm)

# 新觀測值
new.tomato <- data.frame(Sepal.Width=3.1, Petal.Length=1.6, Petal.Width=0.3)
new.tomato

# 預測資料
predict(tomato.lm, new.tomato)
