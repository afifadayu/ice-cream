#installing package
install.packages("psych")
install.packages("dplyr")
install.packages("foreign")
install.packages("ggplot2")
install.packages("MASS")
install.packages("Hmisc")
install.packages("reshape2")

#open library
library(psych)
library(dplyr)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

#import dataset
data <- read.csv(file.choose())
head(data)
summary(data)

#check validity and reliability
myData <- as.data.frame(data)
psych::alpha(myData)

#make a factor
data$Y = factor(data$Y)
data$X1 = factor(data$X1)
data$X2 = factor(data$X2)
data$X3 = factor(data$X3)

#Random sampling 
samplesize = 0.60*nrow(data)
set.seed(123123)
index = sample(seq_len(nrow(data)), size = samplesize)

#Creating training and test set 
datatrain = data[index,]
datatest = data[-index,]

dim(datatrain)
dim(datatest)

model= polr(Y ~ X1 + X2 + X3 , data = datatrain, Hess = TRUE)
summary(model)

#Compute confusion table and misclassification error
predictY = predict(model,datatest)
dim(PredictY)
table(datatest$Y, predictY)
mean(as.character(datatest$Y) != as.character(predictY))

#Code by afifadayu