library(caret)
library(randomForest)
#setwd("/Users/cc/Documents/PracticalMachineLearning/Pro1")

URL1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URL2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filename1 <- "pml-training.csv"
filename2 <- "pml-testing.csv"
download.file(url=URL1, destfile=filename1,method="curl")
download.file(url=URL2, destfile=filename2,method="curl")
training <- read.csv("pml-training.csv",row.names=1,na.strings = "")
testing <- read.csv("pml-testing.csv",row.names=1,na.strings = "NA")


nsv <- nearZeroVar(training,saveMetrics=TRUE)
training <- training[,!nsv$nzv]
testing <- testing[,!nsv$nzv]

# Remove variables with missing values
training_filter_na <- training[,(colSums(is.na(training)) == 0)]
testing_filter_na <- testing[,(colSums(is.na(testing)) == 0)]

# Remove unnecessary columns
colRm_train <- c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","num_window")
colRm_test <- c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","num_window","problem_id")
training_colRm <- training_filter_na[,!(names(training_filter_na) %in% colRm_train)]
testing_colRm <- testing_filter_na[,!(names(testing_filter_na) %in% colRm_test)]
dim(training_colRm)
dim(testing_colRm)

inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training_clean <- training_colRm[inTrain,]
validation_clean <- training_colRm[-inTrain,]

cor <- abs(sapply(colnames(training_clean[, -ncol(training)]), function(x) cor(as.numeric(training_clean[, x]), as.numeric(training_clean$classe), method = "spearman")))

set.seed(1234)
# Fit rf model
rfFit <- train(classe ~ ., method = "rf", data = training_clean, importance = T, trControl = trainControl(method = "cv", number = 4))
validation_pred <- predict(rfFit, newdata=validation_clean)
# Check model performance
confusionMatrix(validation_pred,validation_clean$classe)

# Check important variable
imp <- varImp(rfFit)$importance
varImpPlot(rfFit$finalModel, sort = TRUE, type = 1, pch = 19, col = 1, cex = 1, main = "Importance of the Predictors")

testing_pred <- predict(rfFit, newdata=testing_colRm)
write_files <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    filename <- paste0("problem_id", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE,col.names=FALSE)
  }
}
write_files(testing_pred)


#sample error is approximately 0.8

