#TASK-1

#Prediction using Supervised Machine Learning
#Linear Regression with two variables


library(readxl)
dataset= read_excel("C:/Users/yashi/Downloads/linear regression.xlsx")
View(dataset)
nrow(dataset)
typeof(dataset)
dataset=as.data.frame(dataset)
class(dataset)

#loading library

library(rpart)
library(dplyr)
library(caTools)

#creating train and test sets
plot(dataset)
split_values<-sample.split(dataset$Scores,SplitRatio=0.9)
train_sample<-subset(dataset,split_values==T)
nrow(train_sample)
test_sample<-subset(dataset,split_values==F)
nrow(test_sample)

#creating the appropriate linear model
plot(dataset)
b=dataset$Scores
a=dataset$Hours
model=lm(b~a,train_sample)
print(summary(model))

result=predict(object = model,newdata = test_sample)
result

c=data.frame(pred=result,actual=dataset$Scores)
c
mse=sqrt(mean((c$actual-c$pred)^2))
mse

r=data.frame(a=9.25)
final_result=predict(model,r)
final_result

