library('tidyverse')
library('caret')
library(mlbench)
library(caret)
library(gbm)
library(e1071)
library(kernlab)
library(klaR)
set.seed(998)

data(Sonar)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmGrid <- expand.grid(interaction.depth = c(1,5,9),
                       n.trees = (1:30)*50,
                       shrinkage = 0.1,
                       n.minobsinnode=20)
nrow(gbmGrid)

gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2
ggplot(gbmFit2) +
  theme_minimal()

plot(gbmFit2, metric = "Kappa", plotType = "level", 
     scales = list(x = list(rot = 90)))

svmFit <- train(Class ~ ., data = training, 
                method = "svmRadial", 
                trControl = fitControl, 
                preProc = c("center", "scale"),
                tuneLength = 8)
rdaFit <- train(Class ~ ., data = training, 
                method = "rda", 
                trControl = fitControl, 
                tuneLength = 4)

resamps <- resamples(list(GBM = gbmFit2,
                          SVM = svmFit,
                          RDA = rdaFit))

resamps
summary(resamps)
bwplot(resamps, layout = c(2,1))
dotplot(resamps, metric="Accuracy")

splom(resamps)
