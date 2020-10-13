zoo <- read.csv("D:/Data science videos/R Codes/Assignments docs/KNN/Zoo (2).csv")
View(zoo)
attach(zoo)

# Data exploration
summary(zoo)
str(zoo)
anyNA(zoo)

# Converting interger to factor
zoo$type <- as.factor(zoo$type)
str(zoo$type)

# table of diagnosis
table(animal.name)

zoo1 <- zoo[ ,-1]
View(zoo1)
attach(zoo1)

# create training and test data
data <- sample(1:nrow(zoo1), size = nrow(zoo1)*0.7, replace = FALSE)
zoo_train <- zoo1[data, ]
zoo_test <- zoo1[-data, ]

# create labels for training and test data
zoo_train_labels <- zoo1[data,17]
zoo_test_labels <- zoo1[-data, 17]

str(zoo_train_labels)
str(zoo_test_labels)


#Training a model on the data (load the "class" library)
install.packages("class")
library(class)
library(caret)

NROW(zoo_train_labels)

# Building models for k=8
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,cl = zoo_train_labels, k=8)
zoo_test_pred

# Accuracy
Acc <- 100 * sum(zoo_test_labels == zoo_test_pred)/NROW(zoo_test_labels)
Acc

table(zoo_test_pred,zoo_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(zoo_test_pred,zoo_test_labels))


# Building models for k=9
zoo_test_pred1 <- knn(train = zoo_train, test = zoo_test,cl = zoo_train_labels, k=9)
zoo_test_pred1

# Accuracy
Acc1 <- 100 * sum(zoo_test_labels == zoo_test_pred1)/NROW(zoo_test_labels)
Acc1

table(zoo_test_pred1,zoo_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(zoo_test_pred1,zoo_test_labels))

# Creating a loop to check the accuracy for different values of "k"
i=1
k.optm=1
for (i in 1:28) {
  knn.mod <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  k.optm[i] <- 100 * sum(zoo_test_labels == knn.mod)/NROW(zoo_test_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")


# Building models for k=1
zoo_test_pred2 <- knn(train = zoo_train, test = zoo_test,cl = zoo_train_labels, k=1)
zoo_test_pred2

# Accuracy
Acc1 <- 100 * sum(zoo_test_labels == zoo_test_pred2)/NROW(zoo_test_labels)
Acc1

table(zoo_test_pred2,zoo_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(zoo_test_pred2,zoo_test_labels))
