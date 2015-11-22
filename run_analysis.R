########################
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set.
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each
##   variable for each activity and each subject.
########################

install.packages("plyr")
library(plyr)

##Setting file name
file <- "getdata-projectfiles-UCI HAR Dataset.zip"

##Checking if fil exists, if not, download it
if(!file.exists(file))
{
        URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(URL, file)
}

##Checking if directory exists, if not, unzip the data
if(!file.exists("UCI HAR Dataset"))
{
        unzip(file)
}

##Reading data
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

##Assigning column names
colnames(activity_labels) = c("activity_id", "activity_type")

colnames(subject_test) = "subject_id"
colnames(x_test) = features[,2]
colnames(y_test) = "activity_id"

colnames(subject_train) = "subject_id"
colnames(x_train) = features[,2]
colnames(y_train) = "activity_id"

##Merging test data
test_data <- cbind(y_test, subject_test, x_test)

##Merging training data
train_data <- cbind(y_train, subject_train, x_train)

##1.Merges the training and the test sets to create one data set.
total_data <- rbind(test_data, train_data)

##2.Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_data <- total_data[, grepl("mean|std|subject_id|activity_id", names(total_data))]

##3.Uses descriptive activity names to name the activities in the data set.
mean_std_data <- join(mean_std_data, activity_labels, by = "activity_id", match = "first")

##4.Appropriately labels the data set with descriptive variable names.
names(mean_std_data) <- gsub("\\(|\\)", "", names(mean_std_data))
names(mean_std_data) <- gsub("^t", "time", names(mean_std_data))
names(mean_std_data) <- gsub("^f", "freq", names(mean_std_data))
names(mean_std_data) <- gsub("-mean", "Mean", names(mean_std_data))
names(mean_std_data) <- gsub("-std", "StdDev", names(mean_std_data))
names(mean_std_data) <- gsub("Acc", "Accelerometer", names(mean_std_data))

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each
##  variable for each activity and each subject.
avg_data <- ddply(mean_std_data, c("subject_id", "activity_id"), numcolwise(mean))
write.table(avg_data, "avgData.txt",row.name = FALSE, sep = ",")
