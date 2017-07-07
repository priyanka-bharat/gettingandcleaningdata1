    filename <- "getdata_dataset.zip"

    ## Download the data set:

    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename )

    #Unzip the dataset
    unzip(filename )  
  
    # Read in the data from files features.txt, activity_labels.txt,
    features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE);  
    activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE); 
  
    #Read Train Data from files subject_train.txt, x_train.txt and y_train.txt
    subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);  
    xTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE);  
    yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);  
  
     # Assigin column names to the train data 
    colnames(activityType)  = c('activityId','activityType');
    colnames(subjectTrain)  = "subjectId";
    colnames(xTrain)        = features[,2]; 
    colnames(yTrain)        = "activityId";
#-----------------------------------------------------------------------------------------------  
# 1. Merges the training and the test sets to create one data set.
#-----------------------------------------------------------------------------------------------  
    
   # Merge Train Data :subject_train.txt, x_train.txt and y_train.txt
   trainingData = cbind(yTrain,subjectTrain,xTrain);
  
   #Read Test Data from files subject_test.txt, x_test.txt y_test.txt
   subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
   xTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE);
   yTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); 
  
  
   # Assigin column names to the test data 
   colnames(subjectTest) = "subjectId";
   colnames(xTest)       = features[,2]; 
   colnames(yTest)       = "activityId";
  
  
   # Merge Test Data :  subject_test.txt, x_test.txt y_test.txt
   testData = cbind(yTest,subjectTest,xTest);
  
   #Merge Final data (Test and Train)
   finalData = rbind(trainingData,testData);
  
   colNames  = colnames(finalData); 
#-----------------------------------------------------------------------------------------------
#2. Extracts only the measurements on the mean and standard deviation for each measurement.    
#----------------------------------------------------------------------------------------------- 

    # Create a  Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
    finalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
    
    # Subset finalData table based on the logicalVector to keep only desired columns
    finalData = finalData[finalVector==TRUE];
 
#----------------------------------------------------------------------------------------------- 
# 3. Uses descriptive activity names to name the activities in the data set
#----------------------------------------------------------------------------------------------- 
    
    # Merge the finalData set with the acitivityType table to include descriptive activity names
    finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
    
    # Updating the colNames vector to include the new column names after merge
    colNames  = colnames(finalData); 
    
#----------------------------------------------------------------------------------------------- 
# 4. Appropriately labels the data set with descriptive variable names.
#----------------------------------------------------------------------------------------------- 
    
    # Cleaning up the variable names
    for (i in 1:length(colNames)) 
    {
      colNames[i] = gsub("\\()","",colNames[i])
      colNames[i] = gsub("-std$","StdDev",colNames[i])
      colNames[i] = gsub("-mean","Mean",colNames[i])
      colNames[i] = gsub("^(t)","time",colNames[i])
      colNames[i] = gsub("^(f)","freq",colNames[i])
      colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
      colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
      colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
      colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
      colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
      colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
      colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
    };
    
    # Reassigning the new descriptive column names to the finalData set
    colnames(finalData) = colNames;
    
#----------------------------------------------------------------------------------------------- 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#----------------------------------------------------------------------------------------------- 
    
    # Create a new table, finalDataNoActivityType without the activityType column
    finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
    
    # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
    tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
    
    # Merging the tidyData with activityType to include descriptive acitvity names
    tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
    
    # Export the tidyData set 
    write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
    
  
    
