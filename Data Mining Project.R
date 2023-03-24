##Import library
#install.packages("splitTools")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("psych")
#install.packages("knitr")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("ROCR")
#install.packages("plyr")
#install.packages("forcats")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("gridExtra")
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
library(plyr)
library(forcats)
library(caret)
library(dplyr)
library(tidyr)
library(gridExtra)

##Import data
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv', na.strings = "")
print(colSums(is.na(data)))
data <- na.omit(data)
dim(data)
str(data)

##Data Cleaning
for(i in c(2:10,15)){
  data[,i] <- as.factor(data[,i])
  print(class(data[,i]))
}

#for(i in 1:10000){
#  if(data$y[i]=="no") 
#    data$y[i]=0
#  else data$y[i]=1
#}
#data$y <- as.numeric(data$y)
class(data$y)
str(data)

##Split data
set.seed(2023)
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

##Exploratory Analysis on the training data set
dim(train)
summary(train)

train_num <- train[,c(1,11:14,16:21)]
train_cate <- train[,c(2:10,15,21)]

#Categorical Variables
barplot(table(train_cate$y))
barplot(table(train_cate$job))
pie(table(train_cate$marital))
barplot(table(train_cate$education))
pie(table(train_cate$default))
pie(table(train_cate$housing))
pie(table(train_cate$loan))
pie(table(train_cate$contact))
barplot(table(train_cate$month))
barplot(table(train_cate$day_of_week))
pie(table(train_cate$poutcome))

#ggplot2 barplots of Y and all the categorical variables
table(train_cate$y) # %>% kable() #Add this part when transferring to RMarkdown
y.freq <- table(train_cate$y)
y.freq <- as.data.frame(y.freq)
colnames(y.freq) <- c("y", "number")
p0=ggplot(data = y.freq, mapping = aes(x = y, y = number, fill=y)) +
  geom_col();p0

table(train_cate$job)
job.y <- as.data.frame(table(train_cate$y, train_cate$job))
colnames(job.y) <- c("y","job","number")
p1=ggplot(data = job.y, mapping = aes(x = job, y = number, fill=y)) +
  geom_col();p1

atable(train_cate$marital)
marital.y <- as.data.frame(table(train_cate$y, train_cate$marital))
colnames(marital.y) <- c("y","marital","number")
p2=ggplot(data = marital.y, mapping = aes(x = marital, y = number, fill=y)) +
  geom_col();p2

table(train_cate$education)
education.y <- as.data.frame(table(train_cate$y, train_cate$education))
colnames(education.y) <- c("y","education","number")
p3=ggplot(data = education.y, mapping = aes(x = education, y = number, fill=y)) +
  geom_col();p3

table(train_cate$default)
default.y <- as.data.frame(table(train_cate$y, train_cate$default))
colnames(default.y) <- c("y","default","number")
p4=ggplot(data = default.y, mapping = aes(x = default, y = number, fill=y)) +
  geom_col();p4
#delete default

table(train_cate$housing)
housing.y <- as.data.frame(table(train_cate$y, train_cate$housing))
colnames(housing.y) <- c("y","housing","number")
p5=ggplot(data = housing.y, mapping = aes(x = housing, y = number, fill=y)) +
  geom_col();p5

table(train_cate$loan)
loan.y <- as.data.frame(table(train_cate$y, train_cate$loan))
colnames(loan.y) <- c("y","loan","number")
p6=ggplot(data = loan.y, mapping = aes(x = loan, y = number, fill=y)) +
  geom_col();p6

table(train_cate$contact)
contact.y <- as.data.frame(table(train_cate$y, train_cate$contact))
colnames(contact.y) <- c("y","contact","number")
p7=ggplot(data = contact.y, mapping = aes(x = contact, y = number, fill=y)) +
  geom_col();p7

table(train_cate$month)
month.y <- as.data.frame(table(train_cate$y, train_cate$month))
colnames(month.y) <- c("y","month","number")
p8=ggplot(data = month.y, mapping = aes(x = month, y = number, fill=y)) +
  geom_col();p8

table(train_cate$day_of_week)
day_of_week.y <- as.data.frame(table(train_cate$y, train_cate$day_of_week))
colnames(day_of_week.y) <- c("y","day_of_week","number")
p9=ggplot(data = day_of_week.y, mapping = aes(x = day_of_week, y = number, fill=y)) +
  geom_col();p9

table(train_cate$poutcome)
poutcome.y <- as.data.frame(table(train_cate$y, train_cate$poutcome))
colnames(poutcome.y) <- c("y","poutcome","number")
p10=ggplot(data = poutcome.y, mapping = aes(x = poutcome, y = number, fill=y)) +
  geom_col();p10
dev.new()
grid.arrange(p0, p1, ncol=2)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol=2)

#EDA of Numerical Variables
describe(train_num[,-11])

boxplot(train_num[,-11])

ggplot(data = train_num, mapping = aes(x = age, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = age, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "age") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = duration, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = duration, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "duration") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = campaign, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = campaign, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "campaign") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = pdays, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = pdays, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "pdays") +
  theme(legend.position = "none")
#delete pdays

ggplot(data = train_num, mapping = aes(x = previous, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = previous, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "previous") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = emp.var.rate, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = emp.var.rate, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "emp.var.rate") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = cons.price.idx, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = cons.price.idx, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "cons.price.idx") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = cons.conf.idx, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = cons.conf.idx, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "cons.conf.idx") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = euribor3m, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = euribor3m, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "euribor3m") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = nr.employed, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = nr.employed, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "nr.employed") +
  theme(legend.position = "none")

#Correlation between numerical variables
corr <- cor(train_num[,-11], method="pearson")
corrplot(corr, method='ellipse',
         tl.col='black', type='upper',
         tl.cex = 0.9, tl.srt = 45, tl.pos = "lt")
corrplot(corr, method = "number", type = "lower",
         tl.col = "n", tl.cex = 0.9, tl.pos = "n",
         add = T)

ggpairs(train_num)
ggpairs(train_cate)




