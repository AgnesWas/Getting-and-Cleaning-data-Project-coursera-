#Code Book (codebook.md)

This code book describes the variables, the data, and any transformations or work that i've performed to clean up the data called 'data_average' 

##Identifiers
* subjId  - The ID of the test subject 
* actId  - The type of activity performed when the corresponding measurements were taken

##Measurements
•	"timeBodyAccelerometer-mean()-X" 
•	"timeBodyAccelerometer-mean()-Y"
•	"timeBodyAccelerometer-mean()-Z"
•	"timeBodyAccelerometer-std()-X" 
•	"timeBodyAccelerometer-std()-Y" 
•	"timeBodyAccelerometer-std()-Z" 
•	"timeGravityAccelerometer-mean()-X" 
•	"timeGravityAccelerometer-mean()-Y"
•	"timeGravityAccelerometer-mean()-Z"
•	"timeGravityAccelerometer-std()-X" 
•	"timeGravityAccelerometer-std()-Y" 
•	"timeGravityAccelerometer-std()-Z" 
•	"timeBodyAccelerometerJerk-mean()-X"
•	"timeBodyAccelerometerJerk-mean()-Y"
•	"timeBodyAccelerometerJerk-mean()-Z"
•	"timeBodyAccelerometerJerk-std()-X"
•	"timeBodyAccelerometerJerk-std()-Y"
•	"timeBodyAccelerometerJerk-std()-Z" 
•	"timeBodyGyroscope-mean()-X" 
•	"timeBodyGyroscope-mean()-Y" 
•	"timeBodyGyroscope-mean()-Z" 
•	"timeBodyGyroscope-std()-X" 
•	"timeBodyGyroscope-std()-Y"
•	"timeBodyGyroscope-std()-Z"
•	"timeBodyGyroscopeJerk-mean()-X" 
•	"timeBodyGyroscopeJerk-mean()-Y" 
•	"timeBodyGyroscopeJerk-mean()-Z" 
•	"timeBodyGyroscopeJerk-std()-X" 
•	"timeBodyGyroscopeJerk-std()-Y" 
•	"timeBodyGyroscopeJerk-std()-Z"
•	"timeBodyAccelerometerMagnitude-mean()"
•	"timeBodyAccelerometerMagnitude-std()"
•	"timeGravityAccelerometerMagnitude-mean()"
•	"timeGravityAccelerometerMagnitude-std()"
•	"timeBodyAccelerometerJerkMagnitude-mean()"
•	"timeBodyAccelerometerJerkMagnitude-std()" 
•	"timeBodyGyroscopeMagnitude-mean()"
•	"timeBodyGyroscopeMagnitude-std()"
•	"timeBodyGyroscopeJerkMagnitude-mean()"
•	"timeBodyGyroscopeJerkMagnitude-std()"
•	"frequencyBodyAccelerometer-mean()-X"
•	"frequencyBodyAccelerometer-mean()-Y" 
•	"frequencyBodyAccelerometer-mean()-Z" 
•	"frequencyBodyAccelerometer-std()-X"
•	"frequencyBodyAccelerometer-std()-Y" 
•	"frequencyBodyAccelerometer-std()-Z"
•	"frequencyBodyAccelerometer-meanFreq()-X" 
•	"frequencyBodyAccelerometer-meanFreq()-Y" 
•	"frequencyBodyAccelerometer-meanFreq()-Z"
•	"frequencyBodyAccelerometerJerk-mean()-X" 
•	"frequencyBodyAccelerometerJerk-mean()-Y" 
•	"frequencyBodyAccelerometerJerk-mean()-Z" 
•	"frequencyBodyAccelerometerJerk-std()-X" 
•	"frequencyBodyAccelerometerJerk-std()-Y"
•	"frequencyBodyAccelerometerJerk-std()-Z"
•	"frequencyBodyAccelerometerJerk-meanFreq()-X"
•	"frequencyBodyAccelerometerJerk-meanFreq()-Y"
•	"frequencyBodyAccelerometerJerk-meanFreq()-Z"
•	"frequencyBodyGyroscope-mean()-X"
•	"frequencyBodyGyroscope-mean()-Y"
•	"frequencyBodyGyroscope-mean()-Z"
•	"frequencyBodyGyroscope-std()-X"
•	"frequencyBodyGyroscope-std()-Y" 
•	"frequencyBodyGyroscope-std()-Z" 
•	"frequencyBodyGyroscope-meanFreq()-X" 
•	"frequencyBodyGyroscope-meanFreq()-Y"
•	"frequencyBodyGyroscope-meanFreq()-Z" 
•	"frequencyBodyAccelerometerMagnitude-mean()" 
•	"frequencyBodyAccelerometerMagnitude-std()" 
•	"frequencyBodyAccelerometerMagnitude-meanFreq()" 
•	"frequencyBodyAccelerometerJerkMagnitude-mean()" 
•	"frequencyBodyAccelerometerJerkMagnitude-std()" 
•	"frequencyBodyAccelerometerJerkMagnitude-meanFreq()" 
•	"frequencyBodyGyroscopeMagnitude-mean()"
•	"frequencyBodyGyroscopeMagnitude-std()"
•	"frequencyBodyGyroscopeMagnitude-meanFreq()" 
•	"frequencyBodyGyroscopeJerkMagnitude-mean()" 
•	"frequencyBodyGyroscopeJerkMagnitude-std()"
•	"frequencyBodyGyroscopeJerkMagnitude-meanFreq()"

XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

##Activity Labels (actId)
* WALKING  (value  1 ): subject was walking during the test
* WALKING_UPSTAIRS  (value  2 ): subject was walking up a staircase during the test
* WALKING_DOWNSTAIRS  (value  3 ): subject was walking down a staircase during the test
* SITTING  (value  4 ): subject was sitting during the test
* STANDING  (value  5 ): subject was standing during the test
* LAYING  (value  6 ): subject was laying down during the test

##Work performed to clean up the data;
* Preparation in advance-> download the plyr package
* Reading the Train and Test data (and feature documents)
* Assign names to the columns (make sure column names in Train and Test data are similar)
* Merge Train data and merge Test data apart from each other (into two datasets)
* then, Merge Train and Test data into one dataset
* Create Vector for further use
* Extract the measurements on the mean and standard deviation for each measurement
* Use descriptive activity names to name the activities in the data set
* Appropriately label the data set with descriptive variable names
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
