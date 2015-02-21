library(caret)
library(kernlab)
library(randomForest)
library(corrplot)

# -- download and read data

# download the training and test file
training_fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(training_fileUrl, "./data/pml-training.csv")
test_fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(test_fileUrl, "./data/pml-testing.csv")

# read the training and test file and cleanup
training_data <- read.csv("./data/pml-training.csv", na.strings= c("NA",""," "))
training_data_NAs <- apply(training_data, 2, function(x) {sum(is.na(x))})
training_data_clean <- training_data[,which(training_data_NAs == 0)]
training_data_clean <- training_data_clean[8:length(training_data_clean)]

test_data <- read.csv("./data/pml-testing.csv", na.strings= c("NA",""," "))
test_data_NAs <- apply(test_data, 2, function(x) {sum(is.na(x))})
test_data_clean <- test_data[,which(test_data_NAs == 0)]
test_data_clean <- test_data_clean[8:length(test_data_clean)]

# -- begin prediction analysis

# split t training data for training and cross validation
inTrain <- createDataPartition(y = training_data_clean$classe, p = 0.7, list = FALSE)
training <- training_data_clean[inTrain, ]
crossval <- training_data_clean[-inTrain, ]

# fit a model to predict the classe using everything else as a predictor
modFit <- randomForest(classe ~ ., data = training)

# crossvalidate the model using the remaining 30% of data
pred <- predict(modFit, crossval)

table(pred, crossval$classe)
confusionMatrix(crossval$classe, pred)

# predict the classes of the test set
predTest <- predict(modFit, test_data_clean)

# you may use the function on the project submission to write the result into 20 separate txt for submission
# pml_write_files(predTest)

