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
ggplot(class.red, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
  scale_size(range=c(2,20)) + geom_label() + theme_minimal() +
  scale_fill_continuous(low="pink1", high="firebrick3") + guides(size=FALSE) +
  ggtitle("RF Classification of Quality for Red Wines") + theme(plot.title = element_text(hjust=.5, size=20))

ggplot(class.white, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
  scale_size(range=c(2,20)) + geom_label() + theme_minimal() +
  scale_fill_continuous(low="lightyellow", high="goldenrod1") + guides(size=FALSE) +
  ggtitle("RF Classification of Quality for White Wines") + theme(plot.title = element_text(hjust=.5, size=20))
  
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
ggplot(class.red.grp, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
  scale_size(range=c(2,20)) + geom_label() + theme_minimal() +
  scale_fill_continuous(low="pink1", high="firebrick3") + guides(size=FALSE) +
  ggtitle("RF Classification of Quality for Red Wines") + theme(plot.title = element_text(hjust=.5, size=20))

ggplot(class.white.grp, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
  scale_size(range=c(2,20)) + geom_label() + theme_minimal() +
  scale_fill_continuous(low="lightyellow", high="goldenrod1") + guides(size=FALSE) +
  ggtitle("RF Classification of Quality for White Wines") + theme(plot.title = element_text(hjust=.5, size=20))

# true classification
table(y.test.red.grp)
table(y.test.white.grp)

# prediction accuracy with 95% CI
accuracy.red.grp = cM.red.grp$overall[c(1,3,4)]
accuracy.red.grp

accuracy.white.grp = cM.white.grp$overall[c(1,3,4)]
accuracy.white.grp
