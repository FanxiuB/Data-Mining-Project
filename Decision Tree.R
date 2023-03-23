##Import library
library(dplyr)
library(plyr)
library(splitTools)
library(corrplot)
library(ggplot2)
library(GGally)
library(psych)
library(knitr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(forcats)
library(caret)
library(tidyr)

#Import dataset and omit NA
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv', na.strings = "")
print(colSums(is.na(data)))
data <- na.omit(data)

##Data Cleaning
for(i in c(2:10,15,21)){
  data[,i] <- as.factor(data[,i])
  print(class(data[,i]))
}

data_tree <- data[,-c(5,10,13,16,20)]

set.seed(2023)
split <- partition(data_tree$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data_tree[split$train, ]
valid <- data_tree[split$valid, ]
test <- data_tree[split$test, ]

##Modelling Decision Tree
#Model1
set.seed(2023)

minsplit=100
positiveWeight = 1.0 / (nrow(subset(train, y == "yes")) / nrow(train))
negativeWeight = 1.0 / (nrow(subset(train, y!= "yes")) / nrow(train))
modelWeights <- ifelse(train$y== "yes", positiveWeight, negativeWeight)

train.Model <- rpart(y ~., data=train, method="class", weights=modelWeights,
                     parms=list(split='information'),
                     control=rpart.control(minsplit=100, minbucket=round(minsplit/3),
                                           maxdepth = 4, cp=0.02))        
rpart.plot(train.Model, type=2, extra=4)
#Variable Importance
train.Model$variable.importance

#training performance
train.pred <- predict(train.Model, newdata=train[,-16],type="class")
table(train[,16], train.pred)

#validation performance
valid.pred <- predict(train.Model, newdata=valid[,-16],type="class")
table(valid[,16], valid.pred)

#Evaluation
train.table <- table(train[,16], train.pred)
train.table[1,1]/sum(train.table[1,]) # training sensitivity
train.table[2,2]/sum(train.table[2,]) # training specificity
(train.table[1,1]+train.table[2,2])/sum(train.table) # Total accuracy

valid.table <- table(valid[,16], valid.pred)
valid.table[1,1]/sum(valid.table[1,]) # training sensitivity
valid.table[2,2]/sum(valid.table[2,]) # training specificity
(valid.table[1,1]+valid.table[2,2])/sum(valid.table) # Total accuracy

Ynew.pred <- predict(train.Model, newdata=test, type="class")
test.table <- table(test[,16], Ynew.pred)
test.table[1,1]/sum(test.table[1,]) # training sensitivity
test.table[2,2]/sum(test.table[2,]) # training specificity
(test.table[1,1]+test.table[2,2])/sum(test.table) # Total accuracy





set.seed(2023)
single_tree <- rpart(y~., data=train)
predictions <- predict(single_tree, test, "class")
overall_class_rate_tree <- mean(predictions==test$y)
print(c(overall_class_rate_bag,overall_class_rate_tree))


bag_df <- data.frame(var = rownames(randomForest::importance(bagging)),
                     MeanDecreaseGini = randomForest::importance(bagging)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
bag_ggplot <- ggplot(bag_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "Bagging", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))
bag_ggplot

#Random Forest
n<-length(names(train))     #计算数据集中自变量个数，等同n=ncol(train_data)
rate=1     #设置模型误判率向量初始值
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(y~.,data=train, mtry=i, ntree=1000)
  rate[i]<-mean(rf_train$err.rate)   #计算基于OOB数据的模型误判率均值
  print(rf_train)    
}
rate     #展示所有模型误判率的均值
plot(rate)

set.seed(2023)
rf_train<-randomForest(y~.,data=train, mtry=5, ntree=1000)
plot(rf_train)    #绘制模型误差与决策树数量关系图  

set.seed(2023)
rf <- randomForest(y~., data=train, mtry=5, ntree=200, importance=TRUE, weights=modelWeights)
rf

#validation performance
valid.pred <- predict(rf, newdata=valid[,-16],type="class")
table(valid[,16], valid.pred)

#Evaluation
train.table <- rf$confusion[,-3]
train.table[1,1]/sum(train.table[1,]) # training sensitivity
train.table[2,2]/sum(train.table[2,]) # training specificity
(train.table[1,1]+train.table[2,2])/sum(train.table) # Total accuracy

valid.table <- table(valid[,16], valid.pred)
valid.table[1,1]/sum(valid.table[1,]) # training sensitivity
valid.table[2,2]/sum(valid.table[2,]) # training specificity
(valid.table[1,1]+valid.table[2,2])/sum(valid.table) # Total accuracy

Ynew.pred <- predict(rf, newdata=test, type="class")
test.table <- table(test[,16], Ynew.pred)
test.table[1,1]/sum(test.table[1,]) # training sensitivity
test.table[2,2]/sum(test.table[2,]) # training specificity
(test.table[1,1]+test.table[2,2])/sum(test.table) # Total accuracy

randomForest::importance(rf)[, 1]
rf_df <- data_frame(var = rownames(randomForest::importance(rf)),
                     MeanDecreaseGini = randomForest::importance(rf)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
rf_ggplot <- ggplot(rf_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "randomForest", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))
rf_ggplot


