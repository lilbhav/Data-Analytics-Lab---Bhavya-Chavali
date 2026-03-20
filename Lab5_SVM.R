library(ggplot2)
library(e1071)
library(GGally)
library(readr)
library(randomForest)

setwd("C:/Users/chavab/Dropbox/Classes/Data Analytics")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                 "Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                 "Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)
dataset <- wine[, c("Type","Flavanoids","Proline","Color Intensity","Alcohol")]

## scale features
dataset[,2:5] <- scale(dataset[,2:5])

names(dataset) <- c("Type","Flavanoids","Proline","Color_Intensity","Alcohol")

## train/test split
N <- nrow(dataset)
set.seed(42)
train.indexes <- sample(N, 0.7*N)

train <- dataset[ train.indexes,]
test  <- dataset[-train.indexes,]

# SVM Linear Kernel
tuned.linear <- tune.svm(Type ~ ., data = train,
                         kernel = "linear",
                         cost   = c(0.01, 0.1, 1, 5, 10))
tuned.linear

best.C.linear <- tuned.linear$best.parameters$cost

svm.mod0 <- svm(Type ~ ., data = train,
                kernel = "linear",
                cost   = best.C.linear)
svm.mod0

# predict on test data
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

n       = sum(cm)
diagv   = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

accuracy <- sum(diagv) / n
accuracy

recall    = diagv / rowsums
precision = diagv / colsums
f1        = 2 * precision * recall / (precision + recall)

svm.mod0.res <- data.frame(model = "linear", precision, recall, f1)
svm.mod0.res

results <- svm.mod0.res

# SVM radial kernel

tuned.rbf <- tune.svm(Type ~ ., data = train,
                      kernel = "radial",
                      cost   = c(0.1, 1, 5, 10, 50),
                      gamma  = c(0.01, 0.05, 0.1, 0.5, 1))
tuned.rbf

best.C.rbf     <- tuned.rbf$best.parameters$cost
best.gamma.rbf <- tuned.rbf$best.parameters$gamma

svm.mod1 <- svm(Type ~ ., data = train,
                kernel = "radial",
                cost   = best.C.rbf,
                gamma  = best.gamma.rbf)
svm.mod1

### predict on test data
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

n       = sum(cm)
diagv   = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

accuracy <- sum(diagv) / n
accuracy

recall    = diagv / rowsums
precision = diagv / colsums
f1        = 2 * precision * recall / (precision + recall)

svm.mod1.res <- data.frame(model = "radial", precision, recall, f1)
svm.mod1.res

results <- rbind(results, svm.mod1.res)

# random forest
rf.mod <- randomForest(Type ~ ., data = train, ntree = 500)
rf.mod

# predict on test data
test.pred <- predict(rf.mod, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

n       = sum(cm)
diagv   = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

accuracy <- sum(diagv) / n
accuracy

recall    = diagv / rowsums
precision = diagv / colsums
f1        = 2 * precision * recall / (precision + recall)

rf.res <- data.frame(model = "random forest", precision, recall, f1)
rf.res

results <- rbind(results, rf.res)

results
