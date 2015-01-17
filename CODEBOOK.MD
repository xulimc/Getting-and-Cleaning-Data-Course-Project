#Getting and Cleaning Data Course Project

##Part A: Description of original data set.
The UCI HAR Dataset contains four single files and two folders.

####Four single files

```R
README.txt
```
It introduces the experiments and describe the content of each file.

```R
features.txt
```
It contains the names of all 561 features.

```R
features_info.txt
```
It describes the characteristics of each feature.

```R
activity_labels.txt
```
It contains 6 activity names.


####Folder "train"
It contains three files.

```R
X_train.txt
```
It includes all training data, including 7352 samples and 561 features.

```R
Y_train.txt
```
It includes all training labels (7352 labels).

```R
subject_train.txt
```
It identifies the subject who performs the activity (30 subjects).


####Folder "test"
It contains three files.

```R
X_test.txt
```
It includes all test data, including 2947 samples and 561 features.

```R
Y_test.txt
```
It includes all test labels (2947 labels).

```R
subject_test.txt
```
It identifies the subject who performs the activity (30 subjects).

##Part B: All Variables (measurements) collected
There are 68 variables collected for tidy data set, including Activity, Subject and 66 variables related to mean and standard deviation.

- tBodyAcc-mean()-X : (Type: Numeric) Mean time for acceleration of body for X direction. 
- tBodyAcc-mean()-Y : (Type: Numeric) Mean time for acceleration of body for Y direction.
- tBodyAcc-mean()-Z : (Type: Numeric) Mean time for acceleration of body for Z direction.
- tBodyAcc-std()-X : (Type: Numeric) Standard deviation of time for acceleration of body for X direction.
- tBodyAcc-std()-Y : (Type: Numeric) Standard deviation of time for acceleration of body for Y direction.
- tBodyAcc-std()-Z : (Type: Numeric) Standard deviation of time for acceleration of body for Z direction.
- tGravityAcc-mean()-X : (Type: Numeric) Mean time of acceleration of gravity for X direction.
- tGravityAcc-mean()-Y : (Type: Numeric) Mean time of acceleration of gravity for Y direction.
- tGravityAcc-mean()-Z : (Type: Numeric) Mean time of acceleration of gravity for Z direction.
- tGravityAcc-std()-X : (Type: Numeric) Standard deviation of time of acceleration of gravity for X direction.
- tGravityAcc-std()-Y : (Type: Numeric) Standard deviation of time of acceleration of gravity for Y direction.
- tGravityAcc-std()-Z : (Type: Numeric) Standard deviation of time of acceleration of gravity for Z direction.
- tBodyAccJerk-mean()-X : (Type: Numeric) Mean time of body acceleration jerk for X direction.
- tBodyAccJerk-mean()-Y : (Type: Numeric) Mean time of body acceleration jerk for Y direction
- tBodyAccJerk-mean()-Z : (Type: Numeric) Mean time of body acceleration jerk for Z direction
- tBodyAccJerk-std()-X : (Type: Numeric) Standard deviation of time of body acceleration jerk for X direction.
- tBodyAccJerk-std()-Y : (Type: Numeric) Standard deviation of time of body acceleration jerk for Y direction.
- tBodyAccJerk-std()-Z : (Type: Numeric) Standard deviation of time of body acceleration jerk for Z direction.
- tBodyGyro-mean()-X : (Type: Numeric) Mean body gyroscope measurement for X direction.
- tBodyGyro-mean()-Y : (Type: Numeric) Mean body gyroscope measurement for Y direction.
- tBodyGyro-mean()-Z : (Type: Numeric) Mean body gyroscope measurement for Z direction.
- tBodyGyro-std()-X : (Type: Numeric) Standard deviation of body gyroscope measurement for X direction.
- tBodyGyro-std()-Y : (Type: Numeric) Standard deviation of body gyroscope measurement for Y direction.
- tBodyGyro-std()-Z : (Type: Numeric) Standard deviation of body gyroscope measurement for Z direction.
- tBodyGyroJerk-mean()-X : (Type: Numeric) Mean jerk signal of body for X direction.
- tBodyGyroJerk-mean()-Y : (Type: Numeric) Mean jerk signal of body for Y direction.
- tBodyGyroJerk-mean()-Z : (Type: Numeric) Mean jerk signal of body for Z direction.
- tBodyGyroJerk-std()-X : (Type: Numeric) Standard deviation of jerk signal of body for X direction.
- tBodyGyroJerk-std()-Y : (Type: Numeric) Standard deviation of jerk signal of body for Y direction.
- tBodyGyroJerk-std()-Z : (Type: Numeric) Standard deviation of jerk signal of body for Z direction.
- tBodyAccMag-mean() : (Type: Numeric) Mean magnitude of body Acc
- tBodyAccMag-std() : (Type: Numeric) Standard deviation of magnitude of body Acc
- tGravityAccMag-mean() : (Type: Numeric) Mean gravity acceleration magnitude.
- tGravityAccMag-std() : (Type: Numeric) Standard deviation of gravity acceleration magnitude.
- tBodyAccJerkMag-mean() : (Type: Numeric) Mean magnitude of body acceleration jerk.
- tBodyAccJerkMag-std() : (Type: Numeric) Standard deviation of magnitude of body acceleration jerk.
- tBodyGyroMag-mean() : (Type: Numeric) Mean magnitude of body gyroscope measurement.
- tBodyGyroMag-std() : (Type: Numeric) Standard deviation of magnitude of body gyroscope measurement.
- tBodyGyroJerkMag-mean() : (Type: Numeric) Mean magnitude of body body gyroscope jerk measurement.
- tBodyGyroJerkMag-std() : (Type: Numeric) Standard deviation of magnitude of body body gyroscope jerk measurement.
- fBodyAcc-mean()-X : (Type: Numeric) Mean frequency of body acceleration for X direction.
- fBodyAcc-mean()-Y : (Type: Numeric) Mean frequency of body acceleration for Y direction.
- fBodyAcc-mean()-Z : (Type: Numeric) Mean frequency of body acceleration for Z direction.
- fBodyAcc-std()-X : (Type: Numeric) Standard deviation of frequency of body acceleration for X direction.
- fBodyAcc-std()-Y : (Type: Numeric) Standard deviation of frequency of body acceleration for Y direction.
- fBodyAcc-std()-Z : (Type: Numeric) Standard deviation of frequency of body acceleration for Z direction.
- fBodyAccJerk-mean()-X : (Type: Numeric) Mean frequency of body accerlation jerk for X direction.
- fBodyAccJerk-mean()-Y : (Type: Numeric) Mean frequency of body accerlation jerk for Y direction.
- fBodyAccJerk-mean()-Z : (Type: Numeric) Mean frequency of body accerlation jerk for Z direction.
- fBodyAccJerk-std()-X : (Type: Numeric) Standard deviation frequency of body accerlation jerk for X direction.
- fBodyAccJerk-std()-Y : (Type: Numeric) Standard deviation frequency of body accerlation jerk for Y direction.
- fBodyAccJerk-std()-Z : (Type: Numeric) Standard deviation frequency of body accerlation jerk for Z direction.
- fBodyGyro-mean()-X : (Type: Numeric) Mean frequency of body gyroscope measurement for X direction.
- fBodyGyro-mean()-Y : (Type: Numeric) Mean frequency of body gyroscope measurement for Y direction.
- fBodyGyro-mean()-Z : (Type: Numeric) Mean frequency of body gyroscope measurement for Z direction.
- fBodyGyro-std()-X : (Type: Numeric) Standard deviation frequency of body gyroscope measurement for X direction.
- fBodyGyro-std()-Y : (Type: Numeric) Standard deviation frequency of body gyroscope measurement for Y direction.
- fBodyGyro-std()-Z : (Type: Numeric) Standard deviation frequency of body gyroscope measurement for Z direction.
- fBodyAccMag-mean() : (Type: Numeric) Mean frequency of body acceleration magnitude.
- fBodyAccMag-std() : (Type: Numeric) Standard deviation of frequency of body acceleration magnitude.
- fBodyBodyAccJerkMag-mean() : (Type: Numeric) Mean frequency of body acceleration jerk magnitude.
- fBodyBodyAccJerkMag-std() : (Type: Numeric) Standard deviation of frequency of body acceleration jerk magnitude.
- fBodyBodyGyroMag-mean() : (Type: Numeric) Mean frequency of magnitude of body gyroscope measurement.
- fBodyBodyGyroMag-std() : (Type: Numeric) Standard deviation of frequency of magnitude of body gyroscope measurement.
- fBodyBodyGyroJerkMag-mean() : (Type: Numeric) Mean frequency of magnitude of body gyroscope jerk measurement.
- Activity : (Type: Factor, Range: 6 levels) Activity name.
- Subject : (Type: Integer, Range: 1:30) Subject ID.



##Part C: How does the script work?
####Step 1: Read the data files and store their values in six data.frames.
```R
  XTrainData<-read.table("UCI HAR Dataset/train/X_train.txt",sep="")
  YTrainData<-read.table("UCI HAR Dataset/train/Y_train.txt",sep="")
  SubjectTrainData<-read.table("UCI HAR Dataset/train/subject_train.txt",sep="")
  XTestData<-read.table("UCI HAR Dataset/test/X_test.txt",sep="")
  YTestData<-read.table("UCI HAR Dataset/test/Y_test.txt",sep="")
  SubjectTestData<-read.table("UCI HAR Dataset/test/subject_test.txt",sep="")
```
####Step 2: Combine training and test data.
```R
  XData<-rbind(XTrainData,XTestData)
  YData<-rbind(YTrainData,YTestData)
  SubjectData<-rbind(SubjectTrainData,SubjectTestData)
```
####Step 3: Add column names of XData
```R
  Feature<-read.table("UCI HAR Dataset/features.txt",sep="")
  colnames(XData)<-Feature[,2]
```  
####Step 4: Remove the measurements unrelated to mean or standard deviation.
```R
  NewXData<-XData[,grepl("mean\\(\\)",colnames(XData))|grepl("std\\(\\)",colnames(XData))]
```
####Step 5: Combine YData and SubjectData, then add column names to the combined data.
```R
  YSubjectData<-cbind(YData,SubjectData)
  colnames(YSubjectData)<-c("Activity","Subject")
```
####Step 6: Combine NewXData and YSubjectData.
```R
  XYSData<-cbind(NewXData,YSubjectData)
```
####Step 7: Obtain all possible activities and subjects.
```R
  ActivityList<-unique(XYSData[,"Activity"])
  SubjectList<-unique(XYSData[,"Subject"])
```  
####Step 8: Calculate the average of each variable for each activity and each subject.
```R
  TidyData<-data.frame()
  for(Act in ActivityList){
    for(Sub in SubjectList){
      EachSubset<-subset(XYSData,XYSData[,"Activity"]==Act&XYSData[,"Subject"]==Sub)
      EachMean<-apply(EachSubset,2,mean)
      TidyData<-rbind(TidyData,as.vector(EachMean))
    }
  }
  colnames(TidyData)<-colnames(XYSData)
```
####Step 9: Use descriptive activity names.
```R
  ActivityLabels<-read.table("UCI HAR Dataset/activity_labels.txt",sep="")
  TidyData[,"Activity"]<-ActivityLabels[TidyData[,"Activity"],2]
```  
####Step 10: Output the tidy Data.
```R
  write.table(TidyData, file="TidyData.txt",row.names=FALSE)
```
