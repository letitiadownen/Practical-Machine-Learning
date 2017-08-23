# Course 8 Project
Letitia Downen  
August 22, 2017  

## Summary

Using Devices such as *Jawbone up*, *Nike Fuelband*, and *Fitbit* it is now possible to collect a large amount of data about personal activity relatively inexpensively.  These types of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in behavior, or because they are teck geeks. In this project, we will use accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

This project will attempt find a model to predict the manner in which they did the exercise (A,B,C,D,E)

## Data Analysis/Set Creation


```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.3.3
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings = c("NA","#DIV/0!",""))
                    
testing<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings = c("NA","#DIV/0!",""))
```
Remove NA columns and Non informative columns(columns 1 to 7)


```r
training<-training[,colSums(is.na(training))==0]
testing<-testing[,colSums(is.na(training))==0]

training<-training[,-c(1:7)]
testing<-testing[,-c(1:7)]
```

create training and validation sets from the full training set.

```r
set.seed(35)
inTrain<-createDataPartition(training$classe,p=.7)[[1]]
mytrain<-training[inTrain,]
myvalid<-training[-inTrain,]
```
## Model Selection
Set the control for cross validation.  We will use this for each model, so it is useful to set it ahead of time.

```r
control<-trainControl(method="cv",number=5)
```
I. Decision Trees

```r
set.seed(98)
rpartmod<-train(as.factor(classe)~.,data=mytrain,method="rpart")
```

```
## Loading required package: rpart
```

```r
rpartpred<-predict(rpartmod,newdata = myvalid)

confusionMatrix(rpartpred,myvalid$classe)$overall["Accuracy"]
```

```
##  Accuracy 
## 0.5016143
```

II. Random Forests with Preprocessing


```r
set.seed(98)
rfmod<-train(classe~.,data=mytrain,method="rf",trControl=control,preProcess="pca")
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.3.3
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range

## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid
## mtry: reset to within valid range
```

```r
rfpred<-predict(rfmod,newdata=myvalid)
confusionMatrix(rfpred,myvalid$classe)$overall["Accuracy"]
```

```
##  Accuracy 
## 0.9743415
```
II. Boosting with Trees(gbm)


```r
set.seed(98)
gbmmod<-train(classe~.,data=mytrain,method="gbm",trControl=trainControl(method="cv",number=5),verbose=FALSE)
```

```
## Loading required package: gbm
```

```
## Warning: package 'gbm' was built under R version 3.3.3
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 3.3.3
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loading required package: parallel
```

```
## Loaded gbm 2.1.3
```

```
## Loading required package: plyr
```

```r
gbmpred<-predict(gbmmod,newdata=myvalid)
confusionMatrix(gbmpred,myvalid$classe)$overall["Accuracy"]
```

```
##  Accuracy 
## 0.9614274
```

The method of using Random Forests has a high level of accuracy and therefore, I will not combine predictors/models.  

## Predictions

We will use the random forest model to predict the manner in which 20 participants exercised.


```r
predict(rfmod,newdat=testing)
```

```
##  [1] B A B A A B D B A A B C B A E E A B B B
## Levels: A B C D E
```

