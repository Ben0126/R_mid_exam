# wine
wine = read.csv("wine.csv", header = TRUE, sep = ",")
head(wine)
str(wine) 
wine[!complete.cases(wine),]  


## 決策樹 Decision Tree ----
require(rpart)

#  train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(wine), size=ceiling(0.8*nrow(wine) ))
train <- wine[train.index, ]
test <- wine[-train.index, ]

# CART的模型：酒精濃度(Alcohol)的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(Alcohol ~. , 
                   data=train)

# 輸出各節點的細部資訊
cart.model

require(rpart.plot) 
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=1)  

require(partykit)   
rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊


plot(rparty.tree) 
pred <- predict(cart.model, test)

# 用table看預測的情況
table(real=test$Alcohol, predict=pred)

# 計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=test$Alcohol, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量

printcp(cart.model) # 先觀察未修剪的樹，CP欄位代表樹的成本複雜度參數
plotcp(cart.model) # 畫圖觀察未修剪的樹
prunetree_cart.model <- prune(cart.model, 
                              cp = cart.model$cptable[which.min(cart.model$cptable[,"xerror"]),
                                                      "CP"]) # 利用能使決策樹具有最小誤差的CP來修剪樹

prunetree_pred <- predict(prunetree_cart.model,test)

# 用table看預測的情況
table(real=test$Alcohol, predict=prunetree_pred)

prunetree_confus.matrix <- table(real=test$Alcohol, predict=prunetree_pred)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) # 對角線的數量/總數量

require(caret)
require(e1071)
train_control <- trainControl(method="cv", number=10)
train_control.model <- train(Alcohol~., data=train, method="rpart", trControl=train_control)
train_control.model


## 類神經網路 ANN----
require(neuralnet) 
require(nnet)      
require(caret)     

data <- wine
head(data)

# 建立神經網路
formula.bpn <- Alcohol ~  Cultivar+ Malic.acid + Ash + Alcalinity.of.ash  +
                Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols +
                Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline  
bpn <- neuralnet(formula = formula.bpn, data = data, hidden = c(2,4,2), learningrate = 0.01)    
                 
# bpn模型
plot(bpn)

# 80%資料作為train
smp.size <- floor(0.8*nrow(data)) 
set.seed(131)         

# 從原始資料裡面，抽出train set所需要的資料筆數(data size)
train.ind <- sample(seq_len(nrow(data)), smp.size)

# 分成train/test
train <- data[train.ind, ]
test <- data[-train.ind, ]

# tune parameters
model <- train(form=formula.bpn,     
               data=train,           
               method="neuralnet",   # 類神經網路(bpn)
               tuneGrid = expand.grid(.layer1=c(1:6), .layer2=c(0:6), .layer3=c(0:6)),               
               learningrate = 0.01,
               )

# 顯示最佳解
model

# 將參數組合及RMSE畫成圖
plot(model)

# 重新建立model
bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(6,6,6),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 )

# 匯出新模型
plot(bpn)

pred <- compute(bpn,test[,-2])  
pred.result <- round(pred$net.result,1)
pred$net.result

error = pred.result - test$Alcohol
error

err = (error / test$Alcohol)*100
err = abs(err)
err = mean(err)
err = round(err,2)
err

pred.result <- as.data.frame(pred.result)

# 建立一個新欄位，叫做Alcohol
pred.result$Alcohol <- ""

error = pred.result - test$Alcohol
error

# 混淆矩陣 (預測率有96.67%)
table(real = test$Alcohol, predict = pred.result$Alcohol)


prunetree_confus.matrix <- table(real=test$Alcohol, predict=pred$Alcohol)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) # 對角線的數量/總數量
