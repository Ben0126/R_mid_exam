#Advanced Analytics 
#mid exam project

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
 

iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
              data = iris)
summary(iris.lm)

names(iris.lm)

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

## 變異數分析 anova
a.lm <- lm(Petal.Width~Species, data=iris)
anova(a.lm)

b.lm <- lm(Petal.Length~Species, data=iris)
anova(b.lm)

## 預測 


# 新觀測值
new.iris <- data.frame(Sepal.Width=3.1, Petal.Length=1.6, Petal.Width=0.3)
new.iris

# 預測資料
predict(iris.lm, new.iris)

## ANN---- 
require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters

data <- wine
head(data)

formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
bpn <- neuralnet(formula = formula.bpn, 
                 data = data,
                 hidden = c(4,2,4),       # 一個隱藏層：2個node
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5        # 最大的iteration數 = 500000(5*10^5)
                 
)

# bpn模型會長得像這樣
plot(bpn)


## Tuning Parameters
# nrow()是用來擷取資料筆數，乘上0.8後，表示我們的train set裡面要有多少筆資料(data size)
smp.size <- floor(0.8*nrow(data)) 

# 因為是抽樣，有可能每次抽樣結果都不一樣，因此這裡規定好亂數表，讓每次抽樣的結果一樣
set.seed(131)         

# 從原始資料裡面，抽出train set所需要的資料筆數(data size)
train.ind <- sample(seq_len(nrow(data)), smp.size)

# 分成train/test
train <- data[train.ind, ]
test <- data[-train.ind, ]

# tune parameters
model <- train(form=formula.bpn,     # formula
               data=train,           # 資料
               method="neuralnet",   # 類神經網路(bpn)
               
               # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
               # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:6), .layer3=c(0:6)),               
               
               # 以下的參數設定，和上面的neuralnet內一樣
               learningrate = 0.01,  
               threshold = 0.01
)

# 顯示最佳解
model

##  使用新參數重新訓練
# 將參數組合及RMSE畫成圖
plot(model)

bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(4,5,6),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
)

# 新的bpn模型會長得像這樣
plot(bpn)

pred <- compute(bpn, test[, 1:4])  

# 預測結果
pred$net.result
pred.result <- round(pred$net.result)
pred.result

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

# 混淆矩陣 (預測率有96.67%)
table(real = test$Alcohol, predict = pred$Alcohol)
table(real=test$Alcohol, predict=pred)

prunetree_confus.matrix <- table(real=test$Alcohol, predict=pred$Alcohol)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) # 對角線的數量/總數量