##Decision Tree Project
#EX1- Let's start by getting the data which is included in the ISLR library, the College 
#data frame.
library(ISLR)

#EX2-Call the ISLR library and check the head of College (a built-in data frame 
#with ISLR, use data() to check this.) Then reassign College to a dataframe called df
df <- data.frame(College)
head(df)
View(df)

#EX3-Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column.
library(ggplot2)
library(ggthemes)
ggplot(df, aes(x=Room.Board, y=Grad.Rate))+geom_point(aes(color=Private))

#EX4-Create a histogram of full time undergrad students, color by Private.
ggplot(df, aes(x=F.Undergrad))+geom_histogram(color="black",bins=30, aes(fill=Private))

#EX5-Create a histogram of Grad.Rate colored by Private. You should see something odd here.
ggplot(df, aes(x=Grad.Rate))+geom_histogram(color="black",bins=30, aes(fill=Private))

#EX6- What college had a Graduation Rate of above 100%?
row.names(df[df$Grad.Rate >100,])

#EX7-Change that college's grad rate to 100%
row.names(df[df$Grad.Rate >100,]) <- 100

#EX8- Split your data into training and testing sets 70/30. Use the caTools library to do this.
library(caTools)
set.seed(101)
split = sample.split(df$Grad.Rate, SplitRatio = 0.70)

split.train = subset(df, split==TRUE)
split.test  = subset(df, split==FALSE)

#EX9- Use the rpart library to build a decision tree to predict whether or not a school is 
#Private. Remember to only build your tree off the training data.
library(rpart)
library(rpart.plot)

tree <- rpart(Private ~., method='class', data = split.train)


#EX10- Use predict() to predict the Private label on the test data.
pred.tree<-predict(tree, split.test)

#EX11-Check the Head of the predicted values. You should notice that you actually 
#have two columns with the probabilities.

head(pred.tree)

#EX12- Turn these two columns into one column to match the original 
#Yes/No Label for a Private column.

join_column <-function(x){
  if (x >=0.5){
    return('Yes')
  }
  else{
    return('No')
  }
} 
library(dplyr)
pred.tree<-as.data.frame(pred.tree)
pred.tree$Private<-sapply(pred.tree$Yes, join_column)

#EX13- Now use table() to create a confusion matrix of your tree model.
table(pred.tree$Private, split.test$Private)

#EX14- Use the rpart.plot library and the prp() function to plot out your tree model.
prp(tree)

#EX15- Call the randomForest package library
library(randomForest)

#EX16- Now use randomForest() to build out a model to predict Private class. 
#Add importance=TRUE as a parameter in the model. (Use help(randomForest) to 
#find out what this does. 

model <- randomForest(Private ~ ., data=split.train, importance=TRUE)
?randomForest

#EX20- What was your model's confusion matrix on its own training set? Use model$confusion.
model$confusion

#EX21- Grab the feature importance with model$importance. Refer to the reading 
#for more info on what Gini[1] means.[2]
model$importance

#EX22- Now use your random forest model to predict on your test set!
pred.model<-predict(model, split.test)
table(pred.model, split.test$Private)
summary(tree)
