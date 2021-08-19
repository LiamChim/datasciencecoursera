# Project : Getting and Cleaning Data
# Name : Linyan Le
# Date : 2021-08-19

# 0. Preparation
# 0.1 Load packages
library(plyr)

# 0.2 Download the dataset
if(!file.exists("./data")){dir.create("./data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./data/projectdataset.zip")

# 0.3 Unzip the dataset
unzip(zipfile = "./data/projectdataset.zip", exdir = "./data")

# 1. Merges the training and the test sets to create one data set.

# 1.1 Reading files

# 1.1.1 Reading training datasets
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading test datasets
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector
features <- read.table("./data/UCI HAR Dataset/features.txt")

# 1.1.4 Reading activity labels
activityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt")

# 1.2 Assigning columns variable names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activityLabels) <- c("activityID", "activityType")

# 1.3 Merging all datasets into one set
alltrain <- cbind(y_train, subject_train, x_train)
alltest <- cbind(y_test, subject_test, x_test)
allinone <- rbind(alltrain, alltest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# 2.1 Reading column names
colNames <- colnames(allinone)

# 2.2 Create vector for defining ID, mean, and sd
mean_and_std <- (grepl("activityID", colNames) |
                   grepl("subjectID", colNames) |
                   grepl("mean..", colNames) |
                   grepl("std...", colNames)
                 )

# 2.3 Making necessary subset
setforMeanandStd <- allinone[ , mean_and_std == TRUE]

# 3. Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(setforMeanandStd, activityLabels,
                              by = "activityID",
                              all.x = TRUE)

# 4. Appropriately labels the data set with descriptive variable names
# see 1.3, 2.2, 2.3

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

# 5.1 Making a second tidy data set
tidyDataSet <- aggregate(. ~ subjectID + activityID, data = setforMeanandStd, mean)
tidyDataSet <- tidyDataSet[order(tidyDataSet$subjectID, tidyDataSet$activityID), ]

# 5.2 Writing second tidy data set into a txt file
write.table(tidyDataSet, "tidyDataSet.txt", row.names = FALSE)

