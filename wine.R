# wine
wine = read.csv("wine.csv", header = TRUE, sep = ",")
head(wine)
str(wine) # 查看 wine 內部結構
wine[!complete.cases(wine),]  # 檢查是否有 NA 的資料


## 決策樹 Decision Tree ----
require(rpart)

# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(wine), size=ceiling(0.8*nrow(wine) ))
train <- wine[train.index, ]
test <- wine[-train.index, ]

# CART的模型：酒精濃度(Alcohol)的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(Alcohol ~. , 
                   data=train)

# 輸出各節點的細部資訊(呈現在console視窗)
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
require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters

data <- wine



# 原始資料就會變成像這樣
head(data)

formula.bpn <- Alcohol ~  Cultivar+ Malic.acid + Ash + Alcalinity.of.ash  +
                Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols +
                Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline  

bpn <- neuralnet(formula = formula.bpn, data = data, hidden = c(2,4,2), learningrate = 0.01)    
                 
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
               tuneGrid = expand.grid(.layer1=c(1:6), .layer2=c(0:6), .layer3=c(0:6)),               
               
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
                 hidden = c(6,6,6),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 )

# 新的bpn模型會長得像這樣
plot(bpn)

pred <- compute(bpn,test)  
pred$Alcohol

pred

# 建立一個新欄位，叫做Alcohol
pred$Alcohol <- ""

# 把結果轉成data frame的型態
pred <- as.data.frame(pred)
pred[i, "Alcohol"] = pred[i, 1]
pred

# 混淆矩陣 (預測率有96.67%)
table(real = test$Alcohol, predict = pred$Alcohol)
table(real=test$Alcohol, predict=pred$Alcohol)

prunetree_confus.matrix <- table(real=test$Alcohol, predict=pred$Alcohol)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) # 對角線的數量/總數量
