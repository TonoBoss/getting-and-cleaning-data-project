library(dplyr)

##############################################################################################
# STEP 1  Getting data

# downloading zip file if it hasn't already been downloaded
zUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zFile <- "UCI HAR Dataset.zip"

if (!file.exists(zFile)) {
  download.file(zUrl, zFile, mode = "wb")
}

# unziping zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zFile)
}


##############################################################################################
# STEP 2  Reading data

# reading training data
trainingSub <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingVal <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingAct <- read.table(file.path(dataPath, "train", "y_train.txt"))

# reading test data
testSub <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testVal <- read.table(file.path(dataPath, "test", "X_test.txt"))
testAct <- read.table(file.path(dataPath, "test", "y_test.txt"))

# reading features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# reading activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


###############################################################################################
# Step 3  Merge the training and the test sets to create one data set

# concatenate individual data tables
humanActivity <- rbind(
  cbind(trainingSub, trainingVal, trainingAct),
  cbind(testSub, testVal, testAct)
)

# remove individual data tables to save memory
rm(trainingSub, trainingVal, trainingAct, 
   testSub, testVal, testAct)

# assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


##############################################################################################
# Step 4  Extracting only the measurements on the mean and standard deviation
#          for each measurement

# determine columns of data set
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# keeping data in these columns only
humanActivity <- humanActivity[, columnsToKeep]


##############################################################################################
# Step 5  Use descriptive activity names to name the activities in the data
#          set

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


##############################################################################################
# Step 6  Appropriately label the data set with descriptive variable names

# get column names
humanActivityCol <- colnames(humanActivity)

# remove special characters
humanActivityCol <- gsub("[\\(\\)-]", "", humanActivityCol)

# expand abbreviations and clean up names
humanActivityCol <- gsub("^f", "frequencyDomain", humanActivityCol)
humanActivityCol <- gsub("^t", "timeDomain", humanActivityCol)
humanActivityCol <- gsub("Acc", "Accelerometer", humanActivityCol)
humanActivityCol <- gsub("Gyro", "Gyroscope", humanActivityCol)
humanActivityCol <- gsub("Mag", "Magnitude", humanActivityCol)
humanActivityCol <- gsub("Freq", "Frequency", humanActivityCol)
humanActivityCol <- gsub("mean", "Mean", humanActivityCol)
humanActivityCol <- gsub("std", "StandardDeviation", humanActivityCol)

# correct typo
humanActivityCol <- gsub("BodyBody", "Body", humanActivityCol)

# use new labels as column names
colnames(humanActivity) <- humanActivityCol


#################################################################################################
# Step 7 Create a second, independent tidy set with the average of each
#          variable for each activity and each subject

# group by subject and activity and summarise using mean
humanActivityMean <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMean, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
