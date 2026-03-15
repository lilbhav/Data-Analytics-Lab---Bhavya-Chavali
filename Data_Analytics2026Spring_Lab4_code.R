##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/chavab/Dropbox/Classes/Data Analytics")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

###

# Compute PCs and Plot PC1 and PC2
X_scaled <- scale(X)

## Run PCA using princomp() 
pca_result <- princomp(X_scaled, cor = FALSE)

summary(pca_result)

# PC Scores
scores_df <- as.data.frame(pca_result$scores)
scores_df$Type <- Y

# Plot PC1 and PC2
ggplot(scores_df, aes(x = Comp.1, y = Comp.2, color = Type)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("red", "yellow", "blue")) +
  labs(
    title = "PCA: Wine Dataset",
    x = paste0("PC1"),
    y = paste0("PC2")
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "right")

# Variables contributing most to PC1
loadings_pc1 <- pca_result$loadings[, 1]

# sort by absolute value
loadings_sorted <- sort(abs(loadings_pc1), decreasing = TRUE)
print(loadings_sorted)


# kNN on Original Feature Subset

set.seed(42)

#Subset the 4 features (scaled for kNN)
features_subset <- c("Flavanoids", "Proline", "Color Intensity", "Alcohol")
X_sub <- scale(wine[, features_subset])

# Train/test split
n <- nrow(wine)
train_idx<- sample(1:n, size = floor(0.7 * n))
test_idx<- setdiff(1:n, train_idx)

X_train_sub<- X_sub[train_idx, ]
X_test_sub<- X_sub[test_idx, ]
Y_train<- Y[train_idx]
Y_test<- Y[test_idx]

# Choose k
k_val <- round(sqrt(length(train_idx)))
cat("Using k =", k_val, "\n")

# Fit kNN on original feature subset
pred_sub <- knn(train = X_train_sub,
                test = X_test_sub,
                cl = Y_train,
                k = k_val)

# kNN on first 2 PCs
X_pca <- as.matrix(pca_result$scores[, 1:2])

X_train_pca <- X_pca[train_idx, ]
X_test_pca  <- X_pca[test_idx, ]

# Fit kNN on PC scores
pred_pca <- knn(train = X_train_pca,
                test= X_test_pca,
                cl= Y_train,
                k= k_val)

# Compare Models
calc_metrics <- function(actual, predicted, label) {
  tp <- sum(predicted == label & actual == label)
  fp <- sum(predicted == label & actual != label)
  fn <- sum(predicted != label & actual == label)
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
  recall    <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
  f1        <- ifelse((precision + recall) == 0, 0,
                      2 * precision * recall / (precision + recall))
  c(Precision = round(precision, 3),
    Recall    = round(recall, 3),
    F1        = round(f1, 3))
}


metrics_sub <- do.call(rbind, lapply(levels(Y), function(lv) {
  calc_metrics(Y_test, pred_sub, lv)
}))
rownames(metrics_sub) <- paste("Class", levels(Y))
print(as.data.frame(metrics_sub))


metrics_pca <- do.call(rbind, lapply(levels(Y), function(lv) {
  calc_metrics(Y_test, pred_pca, lv)
}))
rownames(metrics_pca) <- paste("Class", levels(Y))
print(as.data.frame(metrics_pca))

# metrics comparison 
cat("Metrics Comparison\n")
macro_sub <- colMeans(metrics_sub)
macro_pca <- colMeans(metrics_pca)
comparison <- rbind(
  "Original Subset (4 vars)" = c(Accuracy = round(acc_sub, 3), macro_sub),
  "First 2 PCs"              = c(Accuracy = round(acc_pca, 3), macro_pca)
)
print(as.data.frame(round(comparison, 3)))
