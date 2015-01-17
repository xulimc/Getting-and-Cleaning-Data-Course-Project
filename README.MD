#Getting and Cleaning Data

##Course Project
The R script run_analysis.R implements the following function:<br>
1. Merges the training and the test sets to create one data set.<br>
2. Extracts only the measurements on the mean and standard deviation for each measurement. <br>
3. Uses descriptive activity names to name the activities in the data set.<br>
4. Appropriately labels the data set with descriptive variable names. <br>
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.<br>

##Run the script
1. Open RStudio and set a working directory for the project.<br>
2. Download the data set from [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ), and then upzip it.<br>
3. Download the R script file run_analysis.R from my repository [https://github.com/xulimc/Getting-and-Cleaning-Data-Course-Project/blob/master/run_analysis.R](https://github.com/xulimc/Getting-and-Cleaning-Data-Course-Project/blob/master/run_analysis.R).<br>
4. Put both the script and data set in the working directory.<br>
5. Type "source("run_analysis.R");run_analysis()" in the RStudio console.

##Obtain the tidy data set
The script will generate a tidy data set TidyData.txt. You can also download it from my repository [https://github.com/xulimc/Getting-and-Cleaning-Data-Course-Project/blob/master/TidyData.txt](https://github.com/xulimc/Getting-and-Cleaning-Data-Course-Project/blob/master/TidyData.txt).<br>
