# This script uses random forest to classify red and white wine individually
# by quality score

#-----------------------------------------------------------------------------#

# read in merged data
library(data.table)
red_train = fread("./data/training_data/red_train.csv",header=T)
red_test = fread("./data/testing_data/red_test.csv",header=T)

#-----------------------------------------------------------------------------#
library(caret)

# set up training and testing data
x.train.red = as.data.frame(red_train[,1:11], ncol=11)
y.train.red = factor(red_train$quality)

x.test.red = as.data.frame(red_test[,1:11], ncol=11)
y.test.red = factor(red_test$quality)

x.train.white = as.data.frame(white_train[,1:11], ncol=11)
y.train.white = factor(white_train$quality)

x.test.white = as.data.frame(white_test[,1:11], ncol=11)
y.test.white = factor(white_test$quality)

# train random forest
trCtl <- trainControl(savePredictions=TRUE)
fit.red <- train(x.train.red, y.train.red, method="rf", trControl=trCtl)
fit.white <- train(x.train.white, y.train.white, method="rf", trControl=trCtl)

# random forest training results
fit.red$results
fit.white$results

# use training model to predict y
y.pred.red <- predict(fit.red, x.test.red)
y.pred.white <- predict(fit.white, x.test.white)

# results of rf on test data
cM.red = confusionMatrix(data=y.pred.red, reference=y.test.red)
cM.white = confusionMatrix(data=y.pred.white, reference=y.test.white)

# classification results (into quality score)
cM.red$table
class.red <- as.data.frame(cM.red$table)
class.red$Freq[class.red$Freq == 0] <-NA

cM.white$table
class.white <- as.data.frame(cM.white$table)
class.white$Freq[class.white$Freq == 0] <-NA

# classification results as a plot
library(ggplot2)
cmPlot(class.red, "red", "RF Classification of Quality for Red Wines")
cmPlot(class.white, "white", "RF Classification of Quality for White Wines")

# true classification
table(red_test$quality)
table(white_test$quality)

# prediction accuracy with 95% CI
accuracy.red = cM.red$overall[c(1,3,4)]
accuracy.red

accuracy.white = cM.white$overall[c(1,3,4)]
accuracy.white

#-----------------------------------------------------------------------------#

# Grouped Quality Classifications using Random Forest

#-----------------------------------------------------------------------------#

# set up training and testing data
# group into low (3-4), mid (5-6) and high (7-9)
y.train.red.grp <- y.train.red
levels(y.train.red.grp)[1:2] <- "low"
levels(y.train.red.grp)[2:3] <- "mid"
levels(y.train.red.grp)[3:4] <- "high"

y.test.red.grp <- y.test.red
levels(y.test.red.grp)[1:2] <- "low"
levels(y.test.red.grp)[2:3] <- "mid"
levels(y.test.red.grp)[3:4] <- "high"

y.train.white.grp <- y.train.white
levels(y.train.white.grp)[1:2] <- "low"
levels(y.train.white.grp)[2:3] <- "mid"
levels(y.train.white.grp)[3:5] <- "high"

y.test.white.grp <- y.test.white
levels(y.test.white.grp)[1:2] <- "low"
levels(y.test.white.grp)[2:3] <- "mid"
levels(y.test.white.grp)[3:5] <- "high"

# train random forest
fit.red.grp <- train(x.train.red, y.train.red.grp, method="rf", trControl=trCtl)
fit.white.grp <- train(x.train.white, y.train.white.grp, method="rf", trControl=trCtl)

# random forest training results
fit.red.grp$results
fit.white.grp$results

# use training model to predict y
y.pred.red.grp <- predict(fit.red.grp, x.test.red)
y.pred.white.grp <- predict(fit.white.grp, x.test.white)

# results of rf on test data
cM.red.grp = confusionMatrix(data=y.pred.red.grp, reference=y.test.red.grp)
cM.white.grp = confusionMatrix(data=y.pred.white.grp, reference=y.test.white.grp)

# classification results (into quality score)
cM.red.grp$table
class.red.grp <- as.data.frame(cM.red.grp$table)
class.red.grp$Freq[class.red.grp$Freq == 0] <-NA

cM.white.grp$table
class.white.grp <- as.data.frame(cM.white.grp$table)
class.white.grp$Freq[class.white.grp$Freq == 0] <-NA

# classification results as a plot
cmPlot(class.red.grp, "red", "RF Classification of Grouped Quality for Red Wines")
cmPlot(class.white.grp, "white", "RF Classification of Grouped Quality for White Wines")

# true classification
table(y.test.red.grp)
table(y.test.white.grp)

# prediction accuracy with 95% CI
accuracy.red.grp = cM.red.grp$overall[c(1,3,4)]
accuracy.red.grp

accuracy.white.grp = cM.white.grp$overall[c(1,3,4)]
accuracy.white.grp

#-----------------------------------------------------------------------------#

# Grouped Quality Classifications using Random Forest and Subsampling

#-----------------------------------------------------------------------------#

# subsampled RF classification by group

library(dplyr)
library(tidyverse)

# subsmaple training data based on minimum within group count
train.red.grp <- cbind(x.train.red, y.train.red.grp)
ns.red <- min(table(train.red.grp$y.train.red.grp))
train.red.grp.ss <- train.red.grp %>% group_by(y.train.red.grp) %>% sample_n(ns.red)
x.train.red.grp.ss <- as.data.frame(train.red.grp.ss[,1:11], ncol=11)
y.train.red.grp.ss <- factor(train.red.grp.ss$y.train.red.grp)

train.white.grp <- cbind(x.train.white, y.train.white.grp)
ns.white <- min(table(train.white.grp$y.train.white.grp))
train.white.grp.ss <- train.white.grp %>% group_by(y.train.white.grp) %>% sample_n(ns.white)
x.train.white.grp.ss <- as.data.frame(train.white.grp.ss[,1:11], ncol=11)
y.train.white.grp.ss <- factor(train.white.grp.ss$y.train.white.grp)

# train random forest
fit.red.grp.ss <- train(x.train.red.grp.ss, y.train.red.grp.ss, method="rf", trControl=trCtl)
fit.white.grp.ss <- train(x.train.white.grp.ss, y.train.white.grp.ss, method="rf", trControl=trCtl)

# random forest training results
fit.red.grp.ss$results
fit.white.grp.ss$results

# use training model to predict y
y.pred.red.grp.ss <- predict(fit.red.grp.ss, x.test.red)
y.pred.white.grp.ss <- predict(fit.white.grp.ss, x.test.white)

# results of rf on test data
cM.red.grp.ss = confusionMatrix(data=y.pred.red.grp.ss, reference=y.test.red.grp)
cM.white.grp.ss = confusionMatrix(data=y.pred.white.grp.ss, reference=y.test.white.grp)

# classification results (into quality score)
cM.red.grp.ss$table
class.red.grp.ss <- as.data.frame(cM.red.grp.ss$table)
class.red.grp.ss$Freq[class.red.grp.ss$Freq == 0] <-NA

cM.white.grp.ss$table
class.white.grp.ss <- as.data.frame(cM.white.grp.ss$table)
class.white.grp.ss$Freq[class.white.grp.ss$Freq == 0] <-NA

# classification results as a plot
cmPlot(class.red.grp.ss, "red",
       "RF Classification of Grouped Quality for Red Wines \n Subsampled by Group")
cmPlot(class.white.grp.ss, "white",
       "RF Classification of Grouped Quality for White Wines \n Subsampled by Group")

# prediction accuracy with 95% CI
accuracy.red.grp.ss = cM.red.grp.ss$overall[c(1,3,4)]
accuracy.red.grp.ss

accuracy.white.grp.ss = cM.white.grp.ss$overall[c(1,3,4)]
accuracy.white.grp.ss

