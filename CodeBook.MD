## Summary
This analysis downloaded data from :
     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
     on January 24, 2015

This data was obtained from:
     http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


## Data Set Informatiom
The orignal experiment was carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity was captured at a constant rate of 50Hz. The experiments were video-recorded to label the data manually. The obtained dataset was been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

This analysis:
- merges the training and test data
  - read `X_train.txt`, `y_train.txt` and `subject_train.txt` from the "./train" folder
  - read `X_test.txt`, `y_test.txt` and `subject_test.txt` from the "./test" folder
  - `join` the six data sets into three: `joinData`, `joinLabel` and `joinSubject`
- `joinData` has 561 columns corresponding to 561 variables.  Pare this down to the 66 variables which store information on Means and StandardDeviation (`features.txt` was consulted for variable names).
- columns of joinData was named using data from `features.txt`.  column names were cleaned to remove "_" and "()"
- read `activity_labels.txt` and change joinLabel to store descriptive activity names intsead of numbers.
- combine `joinSubject`, `joinLabel` and `joinData` into one single data frame called `tidyData`.  `tidyData` is a data Frame of dimension `10299x68`.  The first columns stores subject data, the second indicates the activity name and the remaining 66 columns store the following Means and StandardDeviation informatiion.
  - "tBodyAccmeanX"
  - "tBodyAccmeanY"
  - "tBodyAccmeanZ"
  - "tBodyAccstdX"
  - "tBodyAccstdY"
  - "tBodyAccstdZ"
  - "tGravityAccmeanX"
  - "tGravityAccmeanY"
  - "tGravityAccmeanZ"
  - "tGravityAccstdX"
  - "tGravityAccstdY"
  - "tGravityAccstdZ"
  - "tBodyAccJerkmeanX"
  - "tBodyAccJerkmeanY"
  - "tBodyAccJerkmeanZ"
  - "tBodyAccJerkstdX"
  - "tBodyAccJerkstdY"
  - "tBodyAccJerkstdZ"
  - "tBodyGyromeanX"
  - "tBodyGyromeanY"
  - "tBodyGyromeanZ"
  - "tBodyGyrostdX"
  - "tBodyGyrostdY"
  - "tBodyGyrostdZ"
  - "tBodyGyroJerkmeanX"
  - "tBodyGyroJerkmeanY"
  - "tBodyGyroJerkmeanZ"
  - "tBodyGyroJerkstdX"
  - "tBodyGyroJerkstdY"
  - "tBodyGyroJerkstdZ"
  - "tBodyAccMagmean"
  - "tBodyAccMagstd"
  - "tGravityAccMagmean"
  - "tGravityAccMagstd"
  - "tBodyAccJerkMagmean"
  - "tBodyAccJerkMagstd"
  - "tBodyGyroMagmean"
  - "tBodyGyroMagstd"
  - "tBodyGyroJerkMagmean"
  - "tBodyGyroJerkMagstd"
  - "fBodyAccmeanX"
  - "fBodyAccmeanY"
  - "fBodyAccmeanZ"
  - "fBodyAccstdX"
  - "fBodyAccstdY"
  - "fBodyAccstdZ"
  - "fBodyAccJerkmeanX"
  - "fBodyAccJerkmeanY"
  - "fBodyAccJerkmeanZ"
  - "fBodyAccJerkstdX"
  - "fBodyAccJerkstdY"
  - "fBodyAccJerkstdZ"
  - "fBodyGyromeanX"
  - "fBodyGyromeanY"
  - "fBodyGyromeanZ"
  - "fBodyGyrostdX"
  - "fBodyGyrostdY"
  - "fBodyGyrostdZ"
  - "fBodyAccMagmean"
  - "fBodyAccMagstd"
  - "fBodyBodyAccJerkMagmean"
  - "fBodyBodyAccJerkMagstd"
  - "fBodyBodyGyroMagmean"
  - "fBodyBodyGyroMagstd"
  - "fBodyBodyGyroJerkMagmean"
  - "fBodyBodyGyroJerkMagstd"
- finally, the `dplyr` package to calculate means of each column by subject and activity and store this in a dplyr table data_means.



