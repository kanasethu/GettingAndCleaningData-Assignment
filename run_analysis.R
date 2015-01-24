# ----------------------------------------------------------------------------
# setwd("/Users/kana/Coursera/GettingAndCleaningData/ProgrammingAssignment")
# Step1. Merges the training and the test sets to create one data set.
# Step1a.  Create a function to read and merge data
# ----------------------------------------------------------------------------
read_and_join <- function(trainSrc, testSrc) {
    # Read training data
    message("Reading \"", trainSrc, "\"")
    trainData <- read.table(trainSrc)
    trainDim <- dim(trainData)

    # Read test data
    message("Reading \"", testSrc, "\"")
    testData <- read.table(testSrc)
    testDim <- dim(testData)

    # Join data
    message("Merging data from \"", trainSrc, "\" and \"", testSrc, "\"")
    joinData <- rbind(trainData, testData)
    joinDim <- dim(joinData)

    # Check dimensions to ensure that data was joined
    if (trainDim[1] + testDim[1] ==  joinDim[1]) {
         message("... Done")
    } else {
         stop(".... Error merging data")
    }
    return(joinData)
}

# ----------------------------------------------------------------------------
# Step 1b. Merge data
# ----------------------------------------------------------------------------
joinData <- read_and_join("./UCI_HAR_Dataset/train/X_train.txt",
                          "./UCI_HAR_Dataset/test/X_test.txt")

joinLabel <- read_and_join("./UCI_HAR_Dataset/train/y_train.txt",
                           "./UCI_HAR_Dataset/test/y_test.txt")

joinSubject <- read_and_join("./UCI_HAR_Dataset/train/subject_train.txt",
                             "./UCI_HAR_Dataset/test/subject_test.txt")


# ----------------------------------------------------------------------------
# Step2:  Extracts only the measurements on the mean and standard deviation
# ----------------------------------------------------------------------------
features <- read.table("./UCI_HAR_Dataset/features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names


# ----------------------------------------------------------------------------
# Step3. Uses descriptive activity names to name the activities in the data set
# ----------------------------------------------------------------------------
activity <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"


# ----------------------------------------------------------------------------
# Step4. Appropriately labels the data set with descriptive activity names.
# ----------------------------------------------------------------------------
names(joinSubject) <- "subject"
tidyData <- cbind(joinSubject, joinLabel, joinData)
write.table(tidyData, "tidyData.txt") # write out the 1st dataset

# ----------------------------------------------------------------------------
# Step5. Creates a second, independent tidy data set with the average of
# each variable for each activity and each subject.
# ----------------------------------------------------------------------------
library(dplyr)       # Use dplyr package
tidyData_tbl <- tbl_df(tidyData)

data_means <- tidyData_tbl %>%
     group_by(subject, activity) %>%
     summarise_each(funs(mean))

write.table(data_means, "tidyDataMeans.txt", row.name=FALSE)
