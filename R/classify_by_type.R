# This script uses random forest to classify red versus white white

#-----------------------------------------------------------------------------#

# read in merged data
library(data.table)
all_train = fread("./data/training_data/all_train.csv",header=T)
all_test = fread("./data/testing_data/all_test.csv",header=T)

#-----------------------------------------------------------------------------#
library(caret)

x.train = as.data.frame(all_train[,1:11], ncol=11)
y.train = factor(all_train$type)

x.test = as.data.frame(all_test[,1:11], ncol=11)
y.test = factor(all_test$type)

# train random forest
trCtl <- trainControl(savePredictions=TRUE)
fit <- train(x.train, y.train, method="rf", trControl=trCtl)

# random forest training results
fit$results

# use training model to predict y
y.pred <- predict(fit, x.test)

# results of rf on test data
cM = confusionMatrix(data=y.pred, reference=y.test)

# classification results (red vs white)
cM$table

# classification results as a plot
class.table <- as.data.frame(cM$table)
class.table$Freq[class.table$Freq == 0] <-NA
ggplot(class.table, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
  scale_size(range=c(8,30)) + geom_label() + theme_minimal() +
  scale_fill_continuous(low="red3", high="lightyellow") + guides(size=FALSE) +
  ggtitle("RF Classification of Wine Type") + theme(plot.title = element_text(hjust=.5, size=20))

# classification results
pred <- round(cM$overall[1:2], 4) # prediction accuracy and kappa
res <- round(diag(cM$table)/table(y.test), 4)*100 # percent correct into each type