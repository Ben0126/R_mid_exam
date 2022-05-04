# tomato
tomato = read.csv("TomatoFirst.csv", header = TRUE, sep = ",")
head(tomato)

summary(tomato) 
library(ggplot2)
library(GGally)

## 資料預處理 ----
str(tomato) # 查看 tomato 內部結構
tomato[!complete.cases(tomato),]  # 檢查是否有 NA 的資料

# test
test = tomato[10,]
test = test[,-2]  #刪除Tomato
test = test[,-3]  #刪除Source
test = test[,-1]  #刪除round

# train
tomato = tomato[-10,]  #刪除第10列
tomato = tomato[,-2]  #刪除Tomato
tomato = tomato[,-1]  #刪除round
tomato = tomato[,-2]  #刪除Source
tomato[!complete.cases(tomato),]  # 檢查是否有 NA 的資料
str(tomato)


## 回歸分析 ----
require(ggplot2)
library(ggfortify)
tomato.lm <- lm(Price ~Sweet + Acid + Color + Texture + Overall + Avg.of.Totals + Total.of.Avg,data = tomato)
summary(tomato.lm)
names(tomato.lm)

# 模型診斷圖
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

## 變異數分析 anova
Sweet.lm <- lm(Sweet~Price, data=tomato)
anova(Sweet.lm) # Sweet~Price

Acid.lm <- lm(Acid~Price, data=tomato)
anova(Acid.lm)  # Acid~Price

Color.lm <- lm(Color~Price, data=tomato)
anova(Color.lm)  # Color~Price

Texture.lm <- lm(Texture~Price, data=tomato)
anova(Texture.lm)  # Texture~Price

Overall.lm <- lm(Overall~Price, data=tomato)
anova(Overall.lm)  # Overall~Price

Avg.of.Totals.lm <- lm(Avg.of.Totals~Price, data=tomato)
anova(Avg.of.Totals.lm)  # Avg.of.Totals~Price

Total.of.Avg.lm <- lm(Total.of.Avg~Price, data=tomato)
anova(Total.of.Avg.lm)  # Total.of.Avg~Price

## 預測 
# set test

pred.lm <- predict(tomato.lm,test)
pred.lm


# 相關係數
cor.test(tomato$Price,tomato$Sweet) # -0.2298
cor.test(tomato$Price,tomato$Acid) # 0.6036
cor.test(tomato$Price,tomato$Color) # 0.1809
cor.test(tomato$Price,tomato$Texture) # -0.3098
cor.test(tomato$Price,tomato$Overall) #-0.4436
cor.test(tomato$Price,tomato$Avg.of.Totals) # -0.172355
cor.test(tomato$Price,tomato$Total.of.Avg) #-0.1821





## 建立關聯規則----
summary(tomato) 
require(arules)

rule <- apriori(tomato, parameter=list(minlen=2, supp=0.1, conf=0.1))
inspect(rule)

sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

## 冗於規則判斷與去除
# 根據support大小排序規則
sort.rule <- sort(rule, by="support")
subset.matrix <- is.subset(x=sort.rule, y=sort.rule)
subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))

# 將矩陣的下三角去除，只留上三角的資訊
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

# 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# 移除多餘的規則
sort.rule <- sort.rule[!redundant]

# 顯示整理後的結果
inspect(sort.rule)

# 資料視覺化
require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph")
plot(sort.rule, method="grouped")




## 決策樹 Decision Tree ----
require(rpart)

set.seed(16)
train.index <- sample(x=1:nrow(tomato), size=ceiling(1*nrow(tomato) ))
train <- tomato[train.index, ]


# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(Price~. ,  data=train)
cart.model

require(rpart.plot) 

# 畫出決策樹
prp(cart.model,         # 模型
    faclen=0,           
    extra=5)

## 整理決策樹
require(partykit)   
rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊

plot(rparty.tree) 
plotcp(cart.model) # 畫圖觀察未修剪的樹

prunetree_cart.model <- prune(cart.model, 
                              cp = cart.model$cptable[which.min(cart.model$cptable[,"xerror"]),
                                                      "CP"]) # 利用能使決策樹具有最小誤差的CP來修剪樹

prunetree_pred <- predict(cart.model,test)
prunetree_pred

require(caret)
require(e1071)
train_control <- trainControl(method="cv", number=10)
train_control.model <- train(Price~., data=train, method="rpart", trControl=train_control)
train_control.model


## 預測 ----
pred <- predict(cart.model,test)
pred



