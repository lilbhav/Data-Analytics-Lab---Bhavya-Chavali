################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)

setwd("C:/Users/chavab/Dropbox/Classes/Data Analytics/lab3")
## read data
abalone <- read.csv("abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))



## EOF ##

#-------------Lab------------------

# Two different feature subsets
features_A <- c("length","diameter","height","whole_weight","shucked_wieght","viscera_wieght","shell_weight")
features_B <- c("whole_weight","shucked_wieght","viscera_wieght","shell_weight")

k_fixed <- 5
grid_fixed <- data.frame(k = k_fixed)

# Model A
knn_A <- train(
  x = train[, features_A],
  y = train$age.group,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = 5)
)

# Model B
knn_B <- train(
  x = train[, features_B],
  y = train$age.group,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = 5)
)

knn_A
knn_B

# Predict on test
pred_A <- predict(knn_A, newdata = test[, features_A])
pred_B <- predict(knn_B, newdata = test[, features_B])

# Contingency tables (confusion matrices)
cm_A <- confusionMatrix(pred_A, test$age.group)
cm_B <- confusionMatrix(pred_B, test$age.group)

cm_A$table
cm_A$overall["Accuracy"]

cm_B$table
cm_B$overall["Accuracy"]

acc_A <- cm_A$overall["Accuracy"]
acc_B <- cm_B$overall["Accuracy"]

better <- if (acc_A >= acc_B) "A" else "B"
better

k_grid <- data.frame(k = seq(1, 91, by = 2))

if (better == "A") {
  tuned <- train(
    x = train[, features_A],
    y = train$age.group,
    method = "knn",
    preProcess = c("center", "scale"),
    tuneGrid = k_grid
  )
  pred_tuned <- predict(tuned, newdata = test[, features_A])
} else {
  tuned <- train(
    x = train[, features_B],
    y = train$age.group,
    method = "knn",
    preProcess = c("center", "scale"),
    tuneGrid = k_grid
  )
  pred_tuned <- predict(tuned, newdata = test[, features_B])
}

tuned
plot(tuned)

cm_tuned <- confusionMatrix(pred_tuned, test$age.group)
cm_tuned$table
cm_tuned$overall["Accuracy"]

# Best k selected by caret (based on CV accuracy on TRAIN)
tuned$bestTune

# EXERCISE 2

library(cluster)

# use the same feature subset you used in exercise 1
features <- c("whole_weight","shucked_wieght","viscera_wieght","shell_weight")

# data matrix (scaled!)
X <- scale(abalone.sub[, features])
D <- dist(X)  

k.values <- 2:10

avg_sil_kmeans <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  k <- k.values[i]
  km <- kmeans(X, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, D)
  avg_sil_kmeans[i] <- mean(sil[, 3])
}

opt_k_kmeans <- k.values[which.max(avg_sil_kmeans)]
opt_k_kmeans

avg_sil_pam <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  k <- k.values[i]
  pm <- pam(X, k)
  sil <- silhouette(pm$clustering, D)
  avg_sil_pam[i] <- mean(sil[, 3])
}

opt_k_pam <- k.values[which.max(avg_sil_pam)]
opt_k_pam

kmeans_final <- kmeans(X, centers = opt_k_kmeans, nstart = 25)
pam_final <- pam(X, opt_k_pam)

sil_km <- silhouette(kmeans_final$cluster, D)
plot(sil_km, col = 1:opt_k_kmeans, border = NA,
     main = paste("Silhouette Plot: k-means (k =", opt_k_kmeans, ")"))

sil_pm <- silhouette(pam_final$clustering, D)
plot(sil_pm, col = 1:opt_k_pam, border = NA,
     main = paste("Silhouette Plot: PAM (k =", opt_k_pam, ")"))

cat("k-means best k:", opt_k_kmeans, " avg silhouette:", max(avg_sil_kmeans), "\n")
cat("PAM best k:", opt_k_pam, " avg silhouette:", max(avg_sil_pam), "\n")
