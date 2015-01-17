#R program for course project in the class "Getting and Cleaning Data"

run_analysis<-function(){
  #read the data files: X_train.txt, Y_train.txt, subject_train.txt, X_test.txt, Y_test.txt, subject_test.txt
  XTrainData<-read.table("UCI HAR Dataset/train/X_train.txt",sep="")
  YTrainData<-read.table("UCI HAR Dataset/train/Y_train.txt",sep="")
  SubjectTrainData<-read.table("UCI HAR Dataset/train/subject_train.txt",sep="")
  XTestData<-read.table("UCI HAR Dataset/test/X_test.txt",sep="")
  YTestData<-read.table("UCI HAR Dataset/test/Y_test.txt",sep="")
  SubjectTestData<-read.table("UCI HAR Dataset/test/subject_test.txt",sep="")
  
  print(dim(XTrainData))
  print(dim(YTrainData))
  print(dim(SubjectTrainData))
  print(dim(XTestData))
  print(dim(YTestData))
  print(dim(SubjectTestData))
  
  
  #Combine Data
  XData<-rbind(XTrainData,XTestData)
  YData<-rbind(YTrainData,YTestData)
  SubjectData<-rbind(SubjectTrainData,SubjectTestData)
  
  #Add column names of XData
  Feature<-read.table("UCI HAR Dataset/features.txt",sep="")
  colnames(XData)<-Feature[,2]
  
  #Remove the measurements unrelated to mean or std
  NewXData<-XData[,grepl("mean\\(\\)",colnames(XData))|grepl("std\\(\\)",colnames(XData))]
  
  #Combine YData and SubjectData, then add column names
  YSubjectData<-cbind(YData,SubjectData)
  colnames(YSubjectData)<-c("Activity","Subject")
  
  #Combine all data
  XYSData<-cbind(NewXData,YSubjectData)
  
  #Obtain all possible activities and subjects
  ActivityList<-unique(XYSData[,"Activity"])
  SubjectList<-unique(XYSData[,"Subject"])
  
  #Calculate the average of each variable for each activity and each subject
  TidyData<-data.frame()
  for(Act in ActivityList){
    for(Sub in SubjectList){
      EachSubset<-subset(XYSData,XYSData[,"Activity"]==Act&XYSData[,"Subject"]==Sub)
      EachMean<-apply(EachSubset,2,mean)
      TidyData<-rbind(TidyData,as.vector(EachMean))
    }
  }
  colnames(TidyData)<-colnames(XYSData)
  
  #Use descriptive activity names
  ActivityLabels<-read.table("UCI HAR Dataset/activity_labels.txt",sep="")
  TidyData[,"Activity"]<-ActivityLabels[TidyData[,"Activity"],2]
  
  #Output the tidy Data
  write.table(TidyData, file="TidyData.txt",row.names=FALSE)
}