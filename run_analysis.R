# Preparation in advance
library(plyr)


## Assignment part 1: Merge the training and the test sets to create one data set

# Read the training data
features <- read.table("./features.txt",header=FALSE) 
ActivityLabel <- read.table("./activity_labels.txt",header=FALSE) 
SubjectTrain <-read.table("./train/subject_train.txt", header=FALSE) 
XTrain  <- read.table("./train/X_train.txt", header=FALSE) 
YTrain  <- read.table("./train/y_train.txt", header=FALSE) 

# Assign a name to the columns 
colnames(ActivityLabel)<-c("actId","actType") 
colnames(SubjectTrain) <- "subjId" 
colnames(XTrain) <- features[,2] 
colnames(YTrain) <- "actId" 

# Merge the Train data into 'Datatrain' 
Datatrain <- cbind(YTrain,SubjectTrain,XTrain)

# Read the test data
subjectTest    <-read.table("./test/subject_test.txt", header=FALSE) 
XTest         <- read.table("./test/X_test.txt", header=FALSE) 
YTest         <- read.table("./test/y_test.txt", header=FALSE) 

# Assign a name to the columns (same as in Train data)
colnames(subjectTest) <- "subjId" 
colnames(XTest) <- features[,2] 
colnames(YTest) <- "actId" 

# Merge the Test data into 'Datatest' 
Datatest <- cbind(YTest,subjectTest,XTest)

# Merge Test and Train data into 'TrainTest' 
TrainTest <- rbind(Datatrain,Datatest)

# create Vector for further use
colNames<-colnames(TrainTest)


## Assignment part 2; Extracts only the measurements on the mean and standard deviation for each measurement


MeanStdev <-TrainTest[,grepl("mean|std|subjId|actId",colnames(TrainTest))]

## Assignment part 3; Uses descriptive activity names to name the activities in the data set

MeanStdev <- join(MeanStdev, ActivityLabel, by = "actId", match = "first") 


## Assignment part 4; Appropriately labels the data set with descriptive variable names

# If it starts with a 't', the 't' is replaced by 'time' 
names(MeanStdev)<-gsub("^t", "time", names(MeanStdev))
# If it starts wiht 'f', the 'f' is replaced bij 'frequency' 
names(MeanStdev)<-gsub("^f", "frequency", names(MeanStdev))
# If the name contains 'Acc', 'Acc' is replaced by 'Accelerometer'
names(MeanStdev)<-gsub("Acc", "Accelerometer", names(MeanStdev))
# If the name contains 'Gyro', 'Gyro' is replaced by 'Gyroscope'
names(MeanStdev)<-gsub("Gyro", "Gyroscope", names(MeanStdev))
# If the name contains 'Mag', 'Mag' is replaced by 'Magnitude'
names(MeanStdev)<-gsub("Mag", "Magnitude", names(MeanStdev))
# If the name contains 'BodyBody','Bodybody' is replaced by 'Body'
names(MeanStdev)<-gsub("BodyBody", "Body", names(MeanStdev))

## Assignment part 5; From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Dataaverage<- ddply(MeanStdev, c("subjId","actId"), numcolwise(mean))

write.table(Dataaverage,file="tidydataset.txt")


