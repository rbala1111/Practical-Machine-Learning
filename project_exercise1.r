library(ggplot2)
library(caret)
library(randomForest)
setwd("/Users/199511/Documents/C_Drive/CourseEra/Practical Machine Learning/Project")
--  load("project.Rdata")

# load data
training <- read.csv("./pml-training.csv", row.names = 1)
testing <- read.csv("./pml-testing.csv", row.names = 1)
# remove near zero covariates
nsv <- nearZeroVar(training, saveMetrics = T)
training <- training[, !nsv$nzv]
# remove variables with more than 80% missing values
nav <- sapply(colnames(training),  
    function(x) if(sum(is.na(training[, x])) > 0.8*nrow(training)){return(T)}else{return(F)})
training <- training[, !nav]

# calculate correlations
cor <- abs(sapply(colnames(training[, -ncol(training)]), 
function(x) cor(as.numeric(training[, x]), as.numeric(training$classe), method = "spearman")))

# plot predictors 
summary(cor)
plot(training[, names(which.max(cor))], training[, names(which.max(cor[-which.max(cor)]))],
     col = training$classe, pch = 19, cex = 0.1, xlab = names(which.max(cor)),
     ylab = names(which.max(cor[-which.max(cor)])))
set.seed(123)
boostFit <- train(classe ~ ., method = "gbm", data = training, verbose = F, 
                  trControl = trainControl(method = "cv", number = 10))
boostFit
plot(boostFit, ylim = c(0.9, 1))


set.seed(123)
rfFit <- train(classe ~ ., method = "rf", data = training, 
               importance = T, trControl = trainControl(method = "cv", number = 10))
rfFit
plot(rfFit, ylim = c(0.9, 1))

imp <- varImp(rfFit)$importance
imp$max <- apply(imp, 1, max)
imp <- imp[order(imp$max, decreasing = T), ]
print(plot(varImp(rfFit, scale = FALSE)))

# final model
rfFit$finalModel
# prediction
(prediction <- as.character(predict(rfFit, testing)))


# write prediction files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(prediction)


