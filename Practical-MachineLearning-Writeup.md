Practical Machine Learning Course Project
========================================================
 
 
# Introduction
The purpose of this project is to describe the approach and result for building a model to predict the classification of the correctness of an individual performing a Unilateral Dumbbell Biceps Curl using 3 dimensional accelerometer, gyroscope, and magnetometer data taken from the arms, forearms, belt, and dumbbells of the participants. Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions:
* Class A: Exactly according to the specification
* Class B: Throwing the elbows to the front
* Class C: Lifting the dumbbell only halfway
* Class D: Lowering the dumbbell only halfway
* Class E: Throwing the hips to the front

Training Data has been provided to reflect the resulting ‘Class’ of each observation from the above exercise performed by the six participants. The goal is to determine the best ‘Prediction Model’, which can be used to predict the ‘Class’ for the Test data provided separately. All of the data used and descriptions were taken from Velloso et. al.
Since the goal of the exercise is to classify the Test data into one of the five ‘Classes’ defined in the Training data, different classification models have been used to test the accuracy of the models. This paper will describe the process of generating the model, the cross validation performed, and the expected out of sample error.
 
# Data Loading and Data preparation
Two datasets were available for this project:

1.       "pml-training.csv" - A set of training data for use in creating the model
2.       "pml-testing.csv" - A set of 20 data points for which the classification must be determined without knowledge of the correct classification. 

Now we can load the data, remove near zero covariates and finally remove covariates with more than 80% missing values since these variables will not be useful for prediction.

```r
library(ggplot2)
library(caret)
library(randomForest)
setwd("/Users/199511/Documents/C_Drive/CourseEra/Practical Machine Learning/Project")
# Data loading
training <- read.csv("./pml-training.csv", row.names = 1)
testing <- read.csv("./pml-testing.csv", row.names = 1)
# removing near zero covariates
nsv <- nearZeroVar(training, saveMetrics = T)
training <- training[, !nsv$nzv]
# removing variables with more than 80% missing values
nav <- sapply(colnames(training), function(x) if(sum(is.na(training[, x])) > 0.8*nrow(training)){return(T)}else{return(F)})
training <- training[, !nav]
```

# Finding any strong correlation 
Next we want to find strong predictors that correlates with `classe` so that we can build a model based on those predictors. 

```r
# calculating correlations
cor <- abs(sapply(colnames(training[, -ncol(training)]), function(x) cor(as.numeric(training[, x]), as.numeric(training$classe), method = "spearman")))
# plotting predictors 
summary(cor)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0015  0.0147  0.0524  0.0862  0.1370  0.3170
```

```r
## plot(training[, names(which.max(cor))], training[, names(which.max(cor[-which.max(cor)]))],
##     col = training$classe, pch = 19, cex = 0.1, xlab = names(which.max(cor)),
##     ylab = names(which.max(cor[-which.max(cor)])))
```

The training set has 19622 samples and 57 potential predictors after filtering. There seems to be no strong predictors that correlates with `classe` outcome, so in this case, linear regression model is probably not suitable. We want to try Boosting and random forests algorithms which may generate more robust predictions for our data.

# Model 1:  Boosting Algorithm
Let us use boosting algorithm method with the given data. Let us fit model with boosting algorithm and 10-fold cross validation to predict `classe` with all other predictors and then plot accuracy of this model on the scale `[0.9, 1]`.

```r
set.seed(123)
 boostFit <- train(classe ~ ., method = "gbm", data = training, verbose = F, trControl = trainControl(method = "cv", number = 10))
boostFit
```

```
## Stochastic Gradient Boosting 
## 
## 19622 samples
##    57 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## 
## Summary of sample sizes: 17660, 17661, 17661, 17659, 17659, 17660, ... 
## 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy  Kappa  Accuracy SD  Kappa SD
##   1                   50      0.8       0.8    0.011        0.014   
##   1                  100      0.9       0.9    0.012        0.015   
##   1                  150      0.9       0.9    0.009        0.012   
##   2                   50      1.0       0.9    0.008        0.011   
##   2                  100      1.0       1.0    0.003        0.004   
##   2                  150      1.0       1.0    0.002        0.003   
##   3                   50      1.0       1.0    0.004        0.005   
##   3                  100      1.0       1.0    0.002        0.002   
##   3                  150      1.0       1.0    0.001        0.001   
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were n.trees = 150,
##  interaction.depth = 3 and shrinkage = 0.1.
```

```r
plot(boostFit, ylim = c(0.9, 1))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
The boosting algorithm generated a good model with accuracy = 0.997.

# Model 2: Random Forests 
Let us now fit model with random forests algorithm and 10-fold cross validation to predict `classe` with all other predictors and then plot accuracy of the model on the `same scale`as boosting model.

```r
set.seed(123)
 rfFit <- train(classe ~ ., method = "rf", data = training, importance = T, trControl = trainControl(method = "cv", number = 10))
rfFit
```

```
## Random Forest 
## 
## 19622 samples
##    57 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## 
## Summary of sample sizes: 17660, 17661, 17659, 17660, 17658, 17660, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##    2    1         1      3e-03        3e-03   
##   40    1         1      5e-04        7e-04   
##   79    1         1      5e-04        6e-04   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 40.
```

```r
plot(rfFit, ylim = c(0.9, 1))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
imp <- varImp(rfFit)$importance
imp$max <- apply(imp, 1, max)
imp <- imp[order(imp$max, decreasing = T), ] 
print(plot(varImp(rfFit, scale = FALSE)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

The random forests algorithm generated a very accurate model with accuracy close to 1. Compared to boosting model, this model generally has better performance in terms of accuracy as we see from the plots.

# Cross Validation
Since a boosting and random forest models subsample the training data and covariates several times and averages the results of the multiple trees produced, it is essentially is doing a form of cross validation. Having achieved the primary goal of the model to be able to predict the classifications of the "pml-testing.csv" cases, there was no need to do cross validation for this purpose.

# Concluding Final model and prediction
* Comparing model accuracy of the two models generated, random forests and boosting, random forests model has overall better accuracy. So, I'll use this model for prediction.
* The final random forests model contains 500 trees with 40 variables tried at each split. The five most important predictors in this model are raw_timestamp_part_1, roll_belt, num_window, pitch_forearm, cvtd_timestamp30/11/2011 17:12.

# Out of Sample Error
As described by the random forest predictions, estimated out of sample error rate for the random forests model is `0.04%` as reported by the final model.

```r
 # finalinzing model
rfFit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry, importance = ..1) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 40
## 
##         OOB estimate of  error rate: 0.04%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 5580    0    0    0    0   0.0000000
## B    1 3796    0    0    0   0.0002634
## C    0    3 3419    0    0   0.0008767
## D    0    0    1 3214    1   0.0006219
## E    0    0    0    1 3606   0.0002772
```

```r
 # predicting
(prediction <- as.character(predict(rfFit, testing)))
```

```
##  [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A"
## [18] "B" "B" "B"
```

# Conclusion

The final predictions for the classe from our final model are the following;

 [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A" "B" "B" "B"
 
These output results are written to a file for automatic grading.


```r
# write prediction files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./testdata_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(prediction)
```

# References
The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har
 
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

