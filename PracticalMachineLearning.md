Practical Machine Learning - Predicting Exercise Manner
========================================================

## Synopsis

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

## Data Processing

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv




```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(ggplot2)
library(corrplot)
library(lattice)
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(e1071)
library(drc)
```

```
## Loading required package: car
## Loading required package: gtools
## 
## Attaching package: 'gtools'
## 
## The following object is masked from 'package:car':
## 
##     logit
## 
## The following object is masked from 'package:e1071':
## 
##     permutations
## 
## Loading required package: MASS
## Loading required package: magic
## Loading required package: abind
## Loading required package: plotrix
## 
## 'drc' has been loaded.
## 
## Please cite R and 'drc' if used for a publication,
## for references type 'citation()' and 'citation('drc')'.
## 
## 
## Attaching package: 'drc'
## 
## The following objects are masked from 'package:stats':
## 
##     gaussian, getInitial
```

```r
##Download the data and read into file

if (!file.exists("pml-Training.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
        destfile = "pml-Training.csv")
}
if (!file.exists("pml-Testing.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
        destfile = "pml-Testing.csv")
}
```


```r
## Save files and format NAs

Training <- read.csv("pml-Training.csv", header = TRUE, na.strings = c("NA",""))
Testing <- read.csv("pml-Testing.csv", header = TRUE, na.strings = c("NA",""))
```

Retrieve the dimension of Training object.


```r
dim(Training)
```

```
## [1] 19622   160
```

```r
dim(Testing)
```

```
## [1]  20 160
```

The training data set has 19,622 observations of 160 unique variables. Now, we need to format the missing variables in Training data set and aid in false predictions.


```r
TrainingFormat <- Training[,(colSums(is.na(Training)) == 0)]
```

Then again with the Testing Data set. 


```r
TestingFormat <- Testing[,(colSums(is.na(Testing)) == 0)]
```

Now, we need to remove any additional columns from the Training and Testing data.


```r
RemoveColumn <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window")

TrainingFormat <- TrainingFormat[,!(names(TrainingFormat) %in% RemoveColumn)]
TestingFormat <- TestingFormat[,!(names(TestingFormat) %in% RemoveColumn)]
```

The practice around prediction is splitting up the training data set into 70% for training and to work with, and the rest of the 30% in valiation data. This needs to occur due to cross validation and to insure our models are accurate when predicting. We will also be using the variable "classe" in order to predict the manner in which the users did the exercise. 


```r
Train = createDataPartition(y = TrainingFormat$classe, p = 0.7, list = FALSE)
TrainingSub <- TrainingFormat[Train,]
ValidSub <- TrainingFormat[-Train,]
```

Now, let's look at the columns and see if they correlate with each other. 


```r
Matrix<- cor(TrainingSub[, -54])
corrplot(Matrix, order = "FPC", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

To make the model less biased, we can use component analysis which allows us to produce a set of linear noncorrelated varibles to use.

## Analysis
Let's preprocess the data using component analysis. Then we will use the predict function to the preprocessing to both data sets.


```r
PreprocessData <- preProcess(TrainingSub[, -54], method = "pca", thresh = 0.99)
TrainPreprocessData <- predict(PreprocessData, TrainingSub[, -54])
DataPreprocess <- predict(PreprocessData, ValidSub[, -54])
```

Now, test the model utilizing 'modelFit'.


Fit <- train(TrainingSub$classe ~ ., method = "rf", data = TrainPreprocessData)


## Model

predValid <- predict(Fit, DataPreprocess)
confusion <- confusionMatrix(ValidSub$classe, predValid)
confusion$table



Accuracy <- postResample(ValidSub$classe, predValid)
modelAccuracy <- accuracy[[1]]
modelAccuracy



SampleError <- 1 - modelAccuracy
SampleError


## Results

Now, apply the preprocessing to the original Testing data. Run the model against the testing data and display the results. 


test <- predict(preProc, pmlTesting_filter_col[, -54])
predict_final <- predict(modFit, test)
predict_final




