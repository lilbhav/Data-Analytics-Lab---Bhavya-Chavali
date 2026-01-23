library(readr)
library(EnvStats)
library(nortest)

setwd("/Users/chavab/Dropbox/Data Analytics/Lab 1-selected")

epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

epi.data$ECO.new

epi.data$BDH.new

ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new

# Variable summaries
  
summary(ECO)
summary (BDH)

# Variable boxplots

boxplot(ECO, BDH, names = c("ECO","BDH"))

# Histograms with overlayed theoretical probability distributions

hist(ECO, prob=TRUE)
lines(density(ECO,bw="SJ"))

hist(BDH, prob= TRUE)
lines(density(BDH,bw="SJ"))

# ECDF

plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 

plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 

# QQ plots against the normal distribution

qqnorm(ECO); qqline(ECO)
qqnorm(BDH); qqline(BDH)

# QQ plot against eachother

qqplot(ECO, BDH, xlab = "Q-Q plot for ECO & BDH") 

# Normality Statistical Tests

shapiro.test(ECO)
shapiro.test(BDH)

ad.test(ECO)
ad.test(BDH)

# Statistical tests for identical distributions

ks.test(ECO,BDH)

wilcox.test(ECO,BDH)

var.test(ECO,BDH)
t.test(ECO,BDH)

