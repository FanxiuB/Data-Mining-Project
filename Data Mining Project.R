##Import library
#install.packages("splitTools")
#install.packages("corrplot")
#install.packages("ggplot2")
library(splitTools)
library(corrplot)
library(ggplot2)

##Import data
data <- read.csv("DM - group_12.csv")
dim(data)

##Split data
set.seed(2023)
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

##Exploratory Analysis on the training data set
dim(train)
summary(train)

train_num <- train[,c(1,11:14,16:20)]
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

##尝试一下ggplot2
job.freq <- table(train_cate$job)
job.freq <- as.data.frame(job.freq)
colnames(job.freq) <- c("job", "number")
ggplot(data = job.freq, mapping = aes(x = job, y = number)) +
  geom_col()

#Numerical Variables
boxplot(train_num)

corr <- cor(train_num)
corrplot(corr, tl.col='black', type='upper')











