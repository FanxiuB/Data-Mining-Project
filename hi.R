#install.packages(ISLR)
#library(ISLR)
install.packages('rpart')
install.packages('rpart.plot')
install.packages('randomForest')
install.packages('ROCR')
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
#
library(splitTools)
library(corrplot)
library(ggplot2)
library(GGally)
#install.packages('psych')
library(psych)
library(knitr)
install.packages("skimr")
library(skimr)
install.packages("rsample")  # install rsample package
library(rsample)            # load rsample package

##Import data
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv',na.strings = "")
data <- na.omit(data)
dim(data)
str(data)

##Data Cleaning
for(i in c(2:10,15)){
  data[,i] <- as.factor(data[,i])
  print(class(data[,i]))
}

for(i in 1:10000){
  if(data$y[i]=="no") 
    data$y[i]=0
  else data$y[i]=1
}
data$y <- as.numeric(data$y)
data$age <- as.numeric(data$age)
class(data$y)
str(data)
##Split data
set.seed(2023)
# Define the data frame
#data <- data.frame(age = c(25, 36, 47, 58, 29, 40, 51, 62, 33, 44))
#split <- initial_split(data$age, prop = c(train = 0.6, valid = 0.2, test = 0.2))
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]


##Exploratory Analysis on the training data set
dim(train)
summary(train)

train_num <- train[,c(1,11:14,16:21)]
train_cate <- train[,c(2:10,15,21)]

#eda
str(data)

skim(data)
library(ggplot2)
data$y <- factor(data$y)
ggplot(data = data, aes(x = housing, colour = y)) + 
  geom_density()
ggplot(data, aes(x = housing, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = education, colour = y)) + 
  geom_density()
ggplot(data, aes(x =education, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = duration, colour = month)) + 
  geom_density()
ggplot(data, aes(x = duration, colour = month)) +
  geom_boxplot()

ggplot(data = data, aes(x = y, colour = month)) + 
  geom_density()
ggplot(data, aes(x = y, colour = month)) +
  geom_boxplot()


ggplot(data = data, aes(x = month, colour = y)) + 
  geom_density()
ggplot(data, aes(x = month, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = duration, colour = y)) + 
  geom_density()
ggplot(data, aes(x = duration, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = emp.var.rate, colour = y)) + 
  geom_density()
ggplot(data, aes(x = emp.var.rate, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = euribor3m, colour = y)) + 
  geom_density()
ggplot(data, aes(x = euribor3m, colour = y)) +
  geom_boxplot()

ggplot(data = data, aes(x = nr.employed, colour = y)) + 
  geom_density()
ggplot(data, aes(x = nr.employed, colour = y)) +
  geom_boxplot()

ggplot(data, aes(x=marital, fill=y)) +
  geom_bar()

ggplot(data, aes(x=job, fill=y)) +
  geom_bar()

ggplot(data, aes(x=housing, fill=y)) +
  geom_bar()

ggplot(data, aes(x=loan, fill=y)) +
  geom_bar()

ggplot(data, aes(x=contact, fill=y)) +
  geom_bar()

ggplot(data, aes(x=month, fill=y)) +
  geom_bar()

ggplot(data, aes(x=month, fill=y)) +
  geom_bar()

ggplot(data, aes(x=month, fill=y)) +
  geom_bar()

#Check the multicollinearity
ggpairs(data, columns=c(11,12,16:20), ggplot2::aes(colour=y, alpha=0.2))
ggpairs(data, columns=c(1:8), ggplot2::aes(colour=y, alpha=0.2))


# Effect of different parameters in SVM
install.packages('e1071')
library(e1071)
# Remove missing values from data
data <- na.omit(data)

# Check data type of y variable
class(data$y)

# Summary of data data
summary(data)

#Build a support vector classifier, i.e. SVM with a linear kernel
data$y <- factor(data$y)
# Fit linear SVM model
Model_linear <- svm(y ~ duration + euribor3m, data = data, type = "C-classification", kernel = "linear")
Model_linear %>% 
  summary()
# Plot the model
plot(Model_linear, data, euribor3m ~ duration)


#Build an SVM with a polynomial kernel with two different degrees
Model_poly <- svm(y~duration+nr.employed, data, type="C-classification", kernel="polynomial", degree=2)
plot(Model_poly, data, nr.employed ~ duration)
Model_poly <- svm(y~duration+nr.employed, train, type="C-classification", kernel="polynomial", degree=2)
plot(Model_poly, train, nr.employed ~ duration)
Model_poly2 <- svm(y~duration+nr.employed, train, type="C-classification", kernel="polynomial", degree=4,max_iter = 10000)
plot(Model_poly2, train, nr.employed ~ duration)
#Model_poly3 <- svm(y~duration+nr.employed, data, type="C-classification", kernel="polynomial", degree=4, coef=2)
#plot(Model_poly3, data, nr.employed ~ duration)
#Model_poly3 <- svm(y~euribor3m+emp.var.rate, data, type="C-classification", kernel="polynomial", degree=4, coef=2)
#plot(Model_poly3, data, euribor3m~emp.var.rate)

#Build an SVM with a radial basis function (RBF) kernel with two different gamma values (using the argument gamma) and visualise the decision boundary.
Model_RBF <- svm(y~duration+nr.employed, data, type="C-classification", kernel="radial", gamma=0.1)
plot(Model_RBF, data,nr.employed ~ duration)
Model_RBF2 <- svm(y~duration+nr.employed, data, type="C-classification", kernel="radial", gamma=10)
plot(Model_RBF2, data, nr.employed ~ duration)

#build an SVM
#Split the dataset into 75% for training and 25% for testing
set.seed(1)
n <- nrow(data)
train.idx <- sample(n, round(n*0.75))
train <- data[train.idx,]
test <- data[-train.idx,]

numerical_vars <- sapply(data,is.numeric) #identify numerical variables
var.mean <- apply(train[,numerical_vars],2,mean) 
var.mean
var.sd   <- apply(train[,numerical_vars],2,sd)
var.sd

# standardise training and test sets
train[,numerical_vars] <-t(apply(train[,numerical_vars], 1, function(x) (x-var.mean)/var.sd))
test[,numerical_vars] <-t(apply(test[,numerical_vars], 1, function(x) (x-var.mean)/var.sd))

set.seed(1)
cost_range <- c(0.01,0.1,1,10,100)
degree_range <- 2:5
gamma_range <- c(0.001,0.01,0.1,1,10,100)
train$y <- as.factor(train$y)

#Use tune or tune.svm to select the optimal combinations 
SVM_poly <- tune.svm(y~., data=train, type="C-classification", kernel="polynomial", cost=cost_range, degree=degree_range)
summary(SVM_poly)

SVM_RBF <- tune.svm(y~., data=train, type="C-classification", kernel="radial", cost=cost_range, gamma=gamma_range)
summary(SVM_RBF)

#Build an SVM using the optimal kernel and parameters found in the above question and report the performance on test set.
gamma.opt <- SVM_RBF$best.parameters[1]
cost.opt <- SVM_RBF$best.parameters[2]
SVM_final <- svm(y~marital+housing+duration+cons.price.idx+cons.conf.idx+campaign+emp.var.rate+age+job+education+month+contact+poutcome+previous, data, type="C-classification", kernel="radial", gamma=gamma.opt, cost=cost.opt)
test.pred <- predict(SVM_final,test)
table(test$y,test.pred)







