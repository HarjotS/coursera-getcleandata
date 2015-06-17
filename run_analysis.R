# This R script gets and performs some cleaning on human activity data, built
# from recordings of subjects performing daily activities while carrying
# smartphone. The full description of the data set is available at:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

library(plyr)

#Checks for data directory and creates one if it doesn't exist"
  if (!file.exists("Samsungdata")) {
    message("Creating data directory")
    dir.create("Samsungdata")
  }
  
  if (!file.exists("Samsungdata/UCI HAR Dataset")) {
    # download the data
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipfile="Samsungdata/UCI HAR Dataset.zip"
    message("Downloading data")
    download.file(fileURL, destfile=zipfile, method="curl")
    unzip(zipfile, exdir="Samsungdata")
  }
}

# Merge training and test datasets"
# Read data
  message("reading X_train.txt")
  training.x <- read.table("Samsungdata/UCI HAR Dataset/train/X_train.txt")
  message("reading y_train.txt")
  training.y <- read.table("Samsungdata/UCI HAR Dataset/train/y_train.txt")
  message("reading subject_train.txt")
  training.subject <- read.table("Samsungdata/UCI HAR Dataset/train/subject_train.txt")
  message("reading X_test.txt")
  test.x <- read.table("Samsungdata/UCI HAR Dataset/test/X_test.txt")
  message("reading y_test.txt")
  test.y <- read.table("Samsungdata/UCI HAR Dataset/test/y_test.txt")
  message("reading subject_test.txt")
  test.subject <- read.table("Samsungdata/UCI HAR Dataset/test/subject_test.txt")
  # Merge
  merged.x <- rbind(training.x, test.x)
  merged.y <- rbind(training.y, test.y)
  merged.subject <- rbind(training.subject, test.subject)
  

# Given the dataset, extract only the measurements on the mean
# and standard deviation for each measurement.
  
# Read the feature list file
  features <- read.table("Samsungdata/UCI HAR Dataset/features.txt")
# Find the mean and std columns
  mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
  std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
# Extract them from the data
  merged.x <- merged.x[, (mean.col | std.col)]
  colnames(merged.x) <- features[(mean.col | std.col), 2]


# Use descriptive activity names to name the activities in the dataset
  colnames(merged.y) <- "activity"
  merged.y$activity[merged.y$activity == 1] = "WALKING"
  merged.y$activity[merged.y$activity == 2] = "WALKING_UPSTAIRS"
  merged.y$activity[merged.y$activity == 3] = "WALKING_DOWNSTAIRS"
  merged.y$activity[merged.y$activity == 4] = "SITTING"
  merged.y$activity[merged.y$activity == 5] = "STANDING"
  merged.y$activity[merged.y$activity == 6] = "LAYING"

# Use descriptive column name for subjects
colnames(merged.subject) <- c("subject")

# Combine mean-std values, activities and subjects into one data
# frame.
  combined <- cbind(merged.x, merged.y, merged.subject)


# Given X values, y values and subjects, create an independent tidy dataset
# with the average of each variable for each activity and each subject.
  tidy <- ddply(combined, .(subject, activity), function(x) colMeans(x[,1:60]))

# Write tidy dataset as txt
write.table(tidy, "./Samsungdata/UCI_HAR_tidy.txt", row.names=FALSE)