run_analysis <- function (){
  setwd("C:/Users/laura/Dropbox/Coursera/Data Science/3 - Getting and Cleaning Data/UCI/UCI HAR Dataset")
  # QUESTION 1 Merges the training and the test sets 
   #Create the DF for train, a DF for test and bind them (see the CodeBook for more info)
    train<-cbind(read.table("./train/subject_train.txt"),read.table("./train/y_train.txt"), read.table("./train/X_train.txt"))
    test<-cbind(read.table("./test/subject_test.txt"),read.table("./test/y_test.txt"), read.table("./test/X_test.txt"))
    #Get names for the Variables.
    foo<-read.table("features.txt")
    vs<- c("subject", "activity", as.character(foo[,2]))
    # Merges the training and the test sets and adds labels
    dat<- rbind(train,test)
    colnames(dat)<- vs
  
  #QUESTION 2 Extracts only the measurements on the mean and standard deviation for each measurement.
    #(see the CodeBook to know why I didn't selected the angle() Vs, nor the meanFreq())
    df<-dat[,c("subject", "activity",grep("mean[()]|std", names(dat), value=TRUE))]
  
  # QUESTION 3 Uses descriptive activity names to name the activities in the data set
    #see the CodeBook to know why I didn't merge the tables.
    df$activity<- replace(df$activity, df$activity==1, "walking")
    df$activity<- replace(df$activity, df$activity==2, "walking up")
    df$activity<- replace(df$activity, df$activity==3, "walking down")
    df$activity<- replace(df$activity, df$activity==4, "sitting")
    df$activity<- replace(df$activity, df$activity==5, "standing")
    df$activity<- replace(df$activity, df$activity==6, "laying")
  
  # QUESTION 4 Appropriately labels the data set with descriptive variable names.
    #see the CodeBook for further Info
    names (df)<-gsub("()","",names(df), fixed =TRUE)
    names (df) <- sub("mean", "Mean", names(df))
    names (df) <- sub("std", "Std", names(df))
    names (df)<-gsub("-","",names(df), fixed =TRUE)
  
  # QUESTION 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    #convert de Vs I want to group by to factors
    df$subject<- factor(as.numeric(as.character(df$subject)))
    df$activity<- factor(df$activity)
    #Group by Subject and activity and then calculate the mean of all the other vaariables
    df2<- df %>%
      group_by_if(is.factor) %>%
      summarise_all(mean)
  print(df2)
  View(df2)
  write.table(df2,"Assignment.txt", row.name=FALSE)
}
  


 