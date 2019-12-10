library('tidyverse')
library('caret')
library(mlbench)
library(caret)
library(gbm)
library(e1071)
library(kernlab)
library(klaR)
library(GA)
library(xgboost)
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

gbmGrid3 <- expand.grid(size= (5:10), decay = c(1.0e-3, 1.0e-2, 1.0e-1))
gbmFit3 <- train(x = x, y = y, 
                 method = "xgbTree", 
                 verbose = TRUE)
varImp(gbmFit3, scale = FALSE) %>%
  ggplot() +
  aes(x=Overall) +
  geom_line()


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

n <- 100
p <- 40
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

ga_ctrl <- gafsControl(functions = caretGA,
                       method = "cv",
                       number = 10)

set.seed(10)
rf_ga <- gafs(x = x, y = y,
              iters = 10,
              gafsControl = ga_ctrl,
              method = 'lm',
              trControl = trainControl(method = "cv", allowParallel = TRUE))
rf_ga
