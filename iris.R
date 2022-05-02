# iris 鳶尾花
head(iris)
str(iris) 
iris[!complete.cases(iris),]  


## 迴歸分析 Regression Analysis----
summary(iris) 
library(ggplot2)
library(GGally)
ggpairs(iris)

require(ggplot2)
qplot(x=Petal.Length,      
      y=Petal.Width, 
      data=iris, 
      geom=c("smooth","point"),    
      color=Species)
 

iris.lm <- lm(Petal.Length ~Sepal.Length + Sepal.Width + Petal.Width,data = iris)
summary(iris.lm)

names(iris.lm)

library(ggfortify)

# 模型診斷圖
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

## 變異數分析 anova
a.lm <- lm(Sepal.Length~Species, data=iris)
anova(a.lm)

b.lm <- lm(Sepal.Width~Species, data=iris)
anova(b.lm)

## 預測 
# set test
new.iris <- data.frame(Sepal.Width=3.1, Sepal.Width=5, Petal.Width=0.3)
new.iris

# 預測資料
predict(iris.lm, new.iris)

## iris 類神經網路 neural net ----

library(neuralnet)
library(nnet)
library(caret)

data <- iris

# 將Species轉換成三個output nodes
head(class.ind(data$Species))

# 資料合併，cbind即column-bind
data <- cbind(data, class.ind(data$Species))
head(data)

# 建立神經網路
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
bpn <- neuralnet(formula = formula.bpn, 
                 data = data,
                 hidden = c(2,4,2),       
                 learningrate = 0.01, 
                 threshold = 0.01,    
                 )

# bpn模型
plot(bpn)

# 80%資料作為訓練用，20%為預測用
smp.size <- floor(0.8*nrow(data)) 
set.seed(131)                     
train.ind <- sample(seq_len(nrow(data)), smp.size)
train <- data[train.ind, ]
test <- data[-train.ind, ]


# tune parameters
model <- train(form=formula.bpn,     
               data=train,           
               method="neuralnet",   

               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0:4)),               
               learningrate = 0.01,  
               threshold = 0.01,     
               stepmax = 5e5         
               )

# 顯示最佳解
model

# 把參數組合和RMSE畫成圖
plot(model)

# 重新建立model
bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(1,2,3),     
                 learningrate = 0.01, 
                 threshold = 0.01,    
                 stepmax = 5e5        
                 )

# 匯出新模型
plot(bpn)

# 輸入的test資料只能包含input node的值，取前四個欄位，丟入模型進行預測
pred <- compute(bpn, test[, 1:4])  

# 預測結果
pred$net.result

# 四捨五入後，變成0/1的狀態
pred.result <- round(pred$net.result)
pred.result

# 把結果轉成data frame的型態
pred.result <- as.data.frame(pred.result)

# 建立一個新欄位，叫做Species
pred.result$Species <- ""

# 把預測結果轉回Species的型態
for(i in 1:nrow(pred.result)){
  if(pred.result[i, 1]==1){ pred.result[i, "Species"] <- "setosa"}
  if(pred.result[i, 2]==1){ pred.result[i, "Species"] <- "versicolor"}
  if(pred.result[i, 3]==1){ pred.result[i, "Species"] <- "virginica"}
}

pred.result

# 混淆矩陣 
table(real    = test$Species, 
      predict = pred.result$Species)


