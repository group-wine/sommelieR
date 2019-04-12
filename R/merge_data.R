# This script merges the red and white data sets, spearately for testing and training data,
# and adds an indicator for type; to be used in the red vs white classifcation problem

library(data.table)

# read in data
red_train <- fread("./data/training_data/white_train.csv",header=T)
names(red_train) <- gsub(" ", "_", names(red_train))
white_train <- fread("./data/training_data/white_train.csv",header=T)
names(white_train) <- gsub(" ", "_", names(white_train))

red_test <- fread("./data/testing_data/red_test.csv",header=T)
names(red_test) <- gsub(" ", "_", names(red_test))
white_test <- fread("./data/testing_data/white_test.csv",header=T)
names(white_test) <- gsub(" ", "_", names(white_test))

# merge data sets, add type indicator
type <- c(rep("red", dim(red_train)[1]),rep("white", dim(white_train)[1]))
all_train_a <- rbind(red_train, white_train)
all_train <- cbind(all_train_a, type)

type2 <- c(rep("red", dim(red_test)[1]),rep("white", dim(white_test)[1]))
all_test_a <- rbind(red_test, white_test)
all_test <- cbind(all_test_a, type2)

# convert type and quality to factor
all_train$type <- as.factor(all_train$type)
all_train$quality <- as.factor(all_train$quality)

all_test$type <- as.factor(all_test$type)
all_test$quality <- as.factor(all_test$quality)

# write to data folder
write.csv(all_train, file = "./data/training_data/all_train.csv", row.names = FALSE)
write.csv(all_test, file = "./data/testing_data/all_test.csv", row.names = FALSE)
