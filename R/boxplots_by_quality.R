# boxplots by quality for each predictor (grouped by 4s)
# this could also help us decide which predictors might not be useful,
# a few of them have small ranges/variances and similar across qualities

library(ggplot2)
library(data.table)
library(gridExtra)
red_all =  fread("./data/winequality-red.csv",header=T)
red_all$quality <- as.factor(red_all$quality)
names(red_all) = gsub(" ", "_", names(red_all))

white_all =  fread("./data/winequality-white.csv",header=T)
white_all$quality <- as.factor(white_all$quality)
names(white_all) = gsub(" ", "_", names(white_all))

# red wine
grid.arrange(
  ggplot(red_all, aes(x = quality, y = fixed_acidity)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = volatile_acidity)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = citric_acid)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = residual_sugar)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2)

grid.arrange(
  ggplot(red_all, aes(x = quality, y = chlorides)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = free_sulfur_dioxide)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = total_sulfur_dioxide)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = density)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2)

grid.arrange(
  ggplot(red_all, aes(x = quality, y = pH)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = sulphates)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(red_all, aes(x = quality, y = alcohol)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2
)

# white wine
grid.arrange(
  ggplot(white_all, aes(x = quality, y = fixed_acidity)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = volatile_acidity)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = citric_acid)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = residual_sugar)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2)

grid.arrange(
  ggplot(white_all, aes(x = quality, y = chlorides)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = free_sulfur_dioxide)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = total_sulfur_dioxide)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = density)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2)

grid.arrange(
  ggplot(white_all, aes(x = quality, y = pH)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = sulphates)) +
    geom_boxplot()+geom_jitter(alpha = .25),
  ggplot(white_all, aes(x = quality, y = alcohol)) +
    geom_boxplot()+geom_jitter(alpha = .25), ncol=2
)

#--------------------------------------------------------------#

library(Hmisc)
library(corrplot)
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# correlations and p-values by variable pairs
rcor_red <-rcorr(as.matrix(red_all[,1:11]))
flattenCorrMatrix(rcor_red$r, rcor_red$P) 

# plot of correlations
corrplot(rcor_red$r, type="upper", order="hclust", tl.col = "black",
         p.mat = rcor_red$P, sig.level = 0.01, insig = "blank")

# fixed acidity and pH highly correlated
# free sulfur dioxide and total sulfur dioxide also

# now for white wine:
rcor_white <-rcorr(as.matrix(white_all[,1:11]))
flattenCorrMatrix(rcor_white$r, rcor_white$P) 

# plot of correlations
corrplot(rcor_white$r, type="upper", order="hclust", tl.col = "black",
         p.mat = rcor_white$P, sig.level = 0.01, insig = "blank")


# here density is highly correlated with alcohol and residual sugar

#--------------------------------------------------------------#

# best subset regression variable selection
# shows which variables included for each number of variables

library(MASS)
library(leaps)

red_train = fread("./data/training_data/red_train.csv",header=T)
names(red_train) = gsub(" ", "_", names(red_train))
white_train = fread("./data/training_data/white_train.csv",header=T)
names(white_train) = gsub(" ", "_", names(white_train))

summary(regsubsets(quality ~ ., data = red_train, nvmax = 10))
summary(regsubsets(quality ~ ., data = white_train, nvmax = 10))

