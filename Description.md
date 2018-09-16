## how you built your model
Seprate into 5 classes as A B C D E
Observe the different outcomes chooseing different variables. 

  > library(caret)  
  > library(rpart)  
  > library(rpart.plot)  
  > library(randomForest)  
  > library(corrplot)  
  > trainRaw <- read.csv("pml-training.csv")  
  > testRaw <- read.csv("pml-testing.csv")  
  > dim(trainRaw)  
  [1] 19622   160  
  > dim(testRaw)  
  [1]  20 160  
  > sum(complete.cases(trainRaw))  
  [1] 406  
  > trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0] 
  > testRaw <- testRaw[, colSums(is.na(testRaw)) == 0]  
  > classe <- trainRaw$classe  
  > trainRemove <- grepl("^X|timestamp|window", names(trainRaw))  
  > trainRaw <- trainRaw[, !trainRemove]  
  > trainCleaned <- trainRaw[, sapply(trainRaw, is.numeric)]  
  > trainCleaned$classe <- classe  
  > testRemove <- grepl("^X|timestamp|window", names(testRaw))  
  > testRaw <- testRaw[, !testRemove]  
  > testCleaned <- testRaw[, sapply(testRaw, is.numeric)]  
  > set.seed(22519) # For reproducibile purpose  
  > inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)  
  > trainData <- trainCleaned[inTrain, ]  
  > testData <- trainCleaned[-inTrain, ]  
  > controlRf <- trainControl(method="cv", 5)  
  > modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)  
  > modelRf  

***
Random Forest 

13737 samples
   52 predictor
    5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 10989, 10991, 10988, 10989, 10991 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
   2    0.9909729  0.9885802
  27    0.9914091  0.9891325
  52    0.9849311  0.9809363

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was mtry = 27.
***

## how you used cross validation

Clean the raw data and split it into two set for training(70%) and testing(30%).
  > inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)  
  > trainData <- trainCleaned[inTrain, ]  
  > testData <- trainCleaned[-inTrain, ]  

## what you think the expected out of sample error is
Corresponding to the quantity: 1-accuracy in the cross-validation data.

## why you made the choices you did
There are many NA in data, and the classe is a variable with no order.

  > predictRf <- predict(modelRf, testData)  
  > confusionMatrix(testData$classe, predictRf)  
***
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1673    0    0    0    1
         B    6 1129    4    0    0
         C    0    0 1021    5    0
         D    0    0   15  948    1
         E    0    0    0    6 1076

Overall Statistics
                                          
               Accuracy : 0.9935          
                 95% CI : (0.9911, 0.9954)
    No Information Rate : 0.2853          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9918          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9964   1.0000   0.9817   0.9885   0.9981
Specificity            0.9998   0.9979   0.9990   0.9968   0.9988
Pos Pred Value         0.9994   0.9912   0.9951   0.9834   0.9945
Neg Pred Value         0.9986   1.0000   0.9961   0.9978   0.9996
Prevalence             0.2853   0.1918   0.1767   0.1630   0.1832
Detection Rate         0.2843   0.1918   0.1735   0.1611   0.1828
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9981   0.9989   0.9903   0.9926   0.9984
***
  > accuracy <- postResample(predictRf, testData$classe)
  > accuracy
***
 Accuracy     Kappa 
0.9935429 0.9918320 
***
  > oose <- 1 - as.numeric(confusionMatrix(testData$classe, predictRf)$overall[1])    
  > oose  
***
[1] 0.006457094
***
  > result <- predict(modelRf, testCleaned[, -length(names(testCleaned))])  
  > result  
***
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
***
  > corrPlot <- cor(trainData[, -length(names(trainData))])  
  > corrplot(corrPlot, method="color")  
  > treeModel <- rpart(classe ~ ., data=trainData, method="class")  
  > prp(treeModel) # fast plot  
