#This script creates the datasets for training and testing for the group project
#using the caret package to facilitate the data partitioning.
set.seed(13)

#Loading necessary libraries
library(caret)
library(readr)
library(dplyr)

#Read in the data, and make the ordinal response variable a factor. Thus,
#the distributions of the response will be appoximately the same in the training
#and the test set.
red <- read_delim("data/winequality-red.csv", delim = ";",
                  col_types = cols(
                    `total sulfur dioxide` = col_double()
                  )) %>%
  mutate(quality = as.factor(quality))
white <- read_delim("data/winequality-white.csv", delim = ';') %>%
  mutate(quality = as.factor(quality))

#Create indecies to subset the data using createDataPartition. We reserve
#20% of the data for testing.
red_train_index <- createDataPartition(red$quality, p = .8,
                                       list = F,
                                       times = 1)
white_train_index <- createDataPartition(red$quality, p = .8,
                                         list = F,
                                          times = 1)

#Create training and testing datasets.
red_train <- red[red_train_index,]
red_test <- red[-red_train_index,]
white_train <- white[white_train_index,]
white_test <- white[-white_train_index,]

#Then write them to the appropriate folders.
write_csv(red_train, "data/training_data/red_train.csv")
write_csv(red_test, "data/testing_data/red_test.csv")
write_csv(white_train, "data/training_data/white_train.csv")
write_csv(white_test, "data/testing_data/white_test.csv")
