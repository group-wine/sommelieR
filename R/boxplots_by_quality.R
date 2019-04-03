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
