setwd("~/Downloads/Bike-Sharing-Dataset")
bike <- read.csv("day.csv")
names(bike)

bike <- bike[,-c(1,14,15)]
names(bike)

library(caret)
train_ind <- createDataPartition(bike$cnt, p=0.75)$Resample1
train <- bike[train_ind, ]
test <- bike[-train_ind, ]
dim(train)
dim(test)


lin <- lm(cnt~., train[,-1])
prd <- predict(lin, test[,c(-1,-13)])

rmse(prd, test[,13])
mae(prd, test[,13])

range(prd)


library(randomForest)
lin <- randomForest(cnt~., train[,-1])
prd <- predict(lin, test[,c(-1,-13)])

rmse(prd, test[,13])
mae(prd, test[,13])


library(e1071)
lin <- svm(cnt~., train[,-1])
prd <- predict(lin, test[,c(-1,-13)])

rmse(prd, test[,13])
mae(prd, test[,13])


library(neuralnet)
lin <- neuralnet(cnt~season+yr+mnth+holiday+weekday+workingday +
                   weathersit+temp+atemp+hum+windspeed, data= train, hidden = 5)

prd <- predict(lin, test[,c(-1,-13)])
rmse(prd, test[,13])
mae(prd, test[,13])

