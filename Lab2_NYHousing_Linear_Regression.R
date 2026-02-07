library("ggplot2")
library("readr")

setwd("C:/Users/chavab/Dropbox/Classes/Data Analytics")

## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset


ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod1)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## filter data

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]


lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


## LAB 2 BEGIN

# Data Cleaning

dataset <- dataset[dataset$PRICE < 30000000, ]
dataset <- dataset[dataset$BEDS < 18, ]
dataset <- dataset[dataset$BATH > 0, ]

# Model 1 Price ~ PROPRTYSQFT + BEDS

lmod3 <- lm(PRICE ~ BEDS, data = dataset)

summary(lmod3)

# plot most significant variable 
plot(PRICE ~ BEDS, data = dataset)
abline(lm(PRICE ~ BEDS, data = dataset), col = "red")

ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Model 2 log(PRICE) ~ log(BEDS)

lmod4 <- lm(log10(PRICE) ~ log10(BEDS), data = dataset)

summary(lmod4)

# plot most significant variable 
plot(log10(PRICE) ~ log10(BEDS), data = dataset)
abline(lm(log10(PRICE) ~ log10(BEDS), data = dataset), col = "red")


ggplot(lmod4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Model 3 log(PRICE) ~ log(BATH) 

lmod5 <- lm(log10(PRICE) ~ log10(BATH), data = dataset)

summary(lmod5)

# plot most significant variable 
plot(log10(PRICE) ~ log10(BATH), data = dataset)
abline(lm(log10(PRICE) ~ log10(BATH), data = dataset), col = "red")

ggplot(lmod5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

### THE END ###


