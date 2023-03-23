##Import library
#install.packages("splitTools")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("psych")
#install.packages("knitr")
#install.packages("class")
#install.packages("MASS")
#install.packages("adabag")
library(class)
library(splitTools)
library(corrplot)
library(ggplot2)
library(GGally)
library(psych)
library(knitr)
library(MASS)
library(adabag)

##Import data
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv',na.strings="")
dim(data)
class(data)
str(data)

#Convert the character variables to the factor variables 
for (i in c(2:10,15,21)) {
  data[,i] <- as.factor(data[,i])
}

#Missing Value /Unknown value
print(colSums(is.na(data)))

for (i in c(2:5,6,7)) {
  data[is.na(data[,i]),][,i] <- names(sort(-table(data[,i])))[1]
}

#Create normalize function
normalize = function(x){	
  return ((x-min(x)) / (max(x)-min(x)))	
}

##Split data
set.seed(2023)
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

#Normalize the numeric variables 
train[,c(1,11:14,16:20)]<-lapply(train[,c(1,11:14,16:20)], normalize)

valid[,c(1,11:14,16:20)]<-lapply(valid[,c(1,11:14,16:20)], normalize)

test[,c(1,11:14,16:20)]<-lapply(test[,c(1,11:14,16:20)], normalize)

#k-Nearest Neighbours
K <- 25 #Try k from 1 to 25
corr.class.rate<-c()

for(k in 1:K){
  pred.class<-knn(train[,c(1,11:14,16:20)], valid[,c(1,11:14,16:20)], train[,21], k=k)
  corr.class.rate[k]<-sum(pred.class==valid$y)/length(pred.class)
}

plot(c(1:K),corr.class.rate,type="b",
     main="Correct classification rates on the validation data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)

k.opt <- which.max(corr.class.rate)
k.opt

test.pred<-knn(train[,c(1,11:14,16:20)], test[,c(1,11:14,16:20)], train[,21], k=k.opt)
table(test$y,test.pred)
acc1 <- sum(test.pred==test$y)/length(test.pred)
acc1

#Linear Discriminant Analysis
#The ggpairs plot produced in previous section suggests that the Gaussian assumption is not satisfied
#Check if the groups have equal covariance
for (i in 1:10) {
  cat(colnames(train_num)[i],sep="\n")
  print(aggregate(train_num[,i],by=list(train$y),var)) #compute variance for each group
}
#Results suggest that considerable differences in the variances for almost all groups.
#Therefore, it may not be very appropriate to apply LDA.
#However, we want to use the Lda to see how it works.
y.lda <- lda(train$y~., data=train_num)
y.lda

y.pred.tr <- predict(y.lda)
windows()
ldahist(data = y.pred.tr$x[,1], g=train$y,cex=1.2)

y.pred <- predict(y.lda,test_num)
acc2 <- mean(test$y == y.pred$class)
acc2

#Adaptive Boosting
#AdaBoost algorithm with different numbers of classifiers
error <- as.numeric()
for(i in 1:20){
  y.adaboost <- boosting(y~., data=train[,c(1,11:14,16:21)], mfinal=i)
  predictions <- predict.boosting(y.adaboost, newdata = test)
  error[i] <- predictions$error
}

plot(c(1:20),error,type="b",
     main="Error rates on the train data for a range of classfier ",
     xlab="k",ylab="Error Rate",cex.main=0.7)

i.opt <- which.min(error)
y.adaboost <- boosting(y~., data=train[,c(1,11:14,16:21)],mfinal = i.opt)

# Make predictions on the test set
predictions <- predict(y.adaboost, newdata = test)

# Calculate the accuracy of the model and evaluate performance
predictions$confusion
acc3 <- 1-predictions$error
acc3

