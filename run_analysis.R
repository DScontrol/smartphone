# Getting and Cleaning Data Course Project 
# OCT-NOV 2020
# ALEX M.
# The purpose of this project is to demonstrate your ability to collect, work with, and cleana data set.

# Data Source: UCI Machine Learning Rerository
# Human Activity Recognition Using Smartphones Data Set 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#loading libraries
library(dplyr)

#Merges the training and the test sets to create one data set.
# Load activity labels + features
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("./UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

#Extract features mean and standard deviation
featuresMSD <- grep(".*mean.*|.*std.*", features[,2])
featuresMSD.names <- features[featuresMSD,2]

#labeling features for using descriptive activity names to name the activities in the data set
featuresMSD.names = gsub('-mean', 'Mean', featuresMSD.names)
featuresMSD.names = gsub('-std', 'Std', featuresMSD.names)
featuresMSD.names <- gsub('[-()]', '', featuresMSD.names)

#loading training data set
train<-read.table("./UCI HAR Dataset/train/X_train.txt")[featuresMSD]
trainActivities<-read.table("./UCI HAR Dataset/train/Y_train.txt")
trainSubjects<-read.table("./UCI HAR Dataset/train/subject_train.txt")
train<-cbind(trainSubjects, trainActivities, train)

#loading test data set
test<-read.table("./UCI HAR Dataset/test/X_test.txt")[featuresMSD]
testActivities<-read.table("./UCI HAR Dataset/test/Y_test.txt")
testSubjects<-read.table("./UCI HAR Dataset/test/subject_test.txt")
test<-cbind(testSubjects, testActivities, test)

#Merging training and test data sets
dataMerged <- rbind(train, test)
colnames(dataMerged) <- c("subject","activity",featuresMSD.names)

#activities & subjects into factors
dataMerged$activity <- factor(dataMerged$activity, levels = activityLabels[,1], labels = activityLabels[,2])
dataMerged$subject <- as.factor(dataMerged$subject)

# creating Data frame for using dplyr 
tidyData<-tibble::as_tibble(dataMerged)
                            
#grouping de data by subject and acitivy                            ) 
tidyData_groupby<-group_by(tidyData,subject,activity)

#sumarizing the data with the mean of each activity
tidyData_avg<-summarise(tidyData_groupby,tBodyAccMeanX=mean(tBodyAccMeanX),tBodyAccMeanY=mean(tBodyAccMeanY),tBodyAccMeanZ=mean(tBodyAccMeanZ),tBodyAccStdX=mean(tBodyAccStdX),tBodyAccStdY=mean(tBodyAccStdY),tBodyAccStdZ=mean(tBodyAccStdZ),
                        tGravityAccMeanX=mean(tGravityAccMeanX), tGravityAccMeanY=mean(tGravityAccMeanY),tGravityAccMeanZ=mean(tGravityAccMeanZ),tGravityAccStdX=mean(tGravityAccStdX),tGravityAccStdY=mean(tGravityAccStdY),tGravityAccStdZ=mean(tGravityAccStdZ),
                        tBodyAccJerkMeanX=mean(tBodyAccJerkMeanX),tBodyAccJerkMeanY=mean(tBodyAccJerkMeanY),tBodyAccJerkMeanZ=mean(tBodyAccJerkMeanZ),tBodyAccJerkStdX=mean(tBodyAccJerkStdX),tBodyAccJerkStdY=mean(tBodyAccJerkStdY),tBodyAccJerkStdZ=mean(tBodyAccJerkStdZ),
                        tBodyGyroMeanX=mean(tBodyGyroMeanX),tBodyGyroMeanY=mean(tBodyGyroMeanY),tBodyGyroMeanZ=mean(tBodyGyroMeanZ),tBodyGyroStdX=mean(tBodyGyroStdX),tBodyGyroStdY=mean(tBodyGyroStdY),tBodyGyroStdZ=mean(tBodyGyroStdZ),                                                     
                        tBodyGyroJerkMeanX=mean(tBodyGyroJerkMeanX),tBodyGyroJerkMeanY=mean(tBodyGyroJerkMeanY),tBodyGyroJerkMeanZ=mean(tBodyGyroJerkMeanZ),tBodyGyroJerkStdX=mean(tBodyGyroJerkStdX),tBodyGyroJerkStdY=mean(tBodyGyroJerkStdY),tBodyGyroJerkStdZ=mean(tBodyGyroJerkStdZ),
                        tBodyAccMagMean=mean(tBodyAccMagMean),tBodyAccMagStd=mean(tBodyAccMagStd),tGravityAccMagMean=mean(tGravityAccMagMean),tGravityAccMagStd=mean(tGravityAccMagStd),tBodyAccJerkMagMean=mean(tBodyAccJerkMagMean),tBodyAccJerkMagStd=mean(tBodyAccJerkMagStd),
                        tBodyGyroMagMean=mean(tBodyGyroMagMean),tBodyGyroMagStd=mean(tBodyGyroMagStd),tBodyGyroJerkMagMean=mean(tBodyGyroJerkMagMean),tBodyGyroJerkMagStd=mean(tBodyGyroJerkMagStd),
                        fBodyAccMeanX=mean(fBodyAccMeanX),fBodyAccMeanY=mean(fBodyAccMeanY),fBodyAccMeanZ=mean(fBodyAccMeanZ),fBodyAccStdX=mean(fBodyAccStdX),fBodyAccStdY=mean(fBodyAccStdY),fBodyAccStdZ=mean(fBodyAccStdZ),
                        fBodyAccMeanFreqX=mean(fBodyAccMeanFreqX),fBodyAccMeanFreqY=mean(fBodyAccMeanFreqY),fBodyAccMeanFreqZ=mean(fBodyAccMeanFreqZ),fBodyAccJerkMeanX=mean(fBodyAccJerkMeanX),fBodyAccJerkMeanY=mean(fBodyAccJerkMeanY),fBodyAccJerkMeanZ=mean(fBodyAccJerkMeanZ),
                        fBodyAccJerkStdX=mean(fBodyAccJerkStdX),fBodyAccJerkStdY=mean(fBodyAccJerkStdY),fBodyAccJerkStdZ=mean(fBodyAccJerkStdZ),fBodyAccJerkMeanFreqX=mean(fBodyAccJerkMeanFreqX),fBodyAccJerkMeanFreqY=mean(fBodyAccJerkMeanFreqY),fBodyAccJerkMeanFreqZ=mean(fBodyAccJerkMeanFreqZ),
                        fBodyGyroMeanX=mean(fBodyGyroMeanX),fBodyGyroMeanY=mean(fBodyGyroMeanY),fBodyGyroMeanZ=mean(fBodyGyroMeanZ),fBodyGyroStdX=mean(fBodyGyroStdX),fBodyGyroStdY=mean(fBodyGyroStdY),fBodyGyroStdZ=mean(fBodyGyroStdZ),
                        fBodyGyroMeanFreqX=mean(fBodyGyroMeanFreqX),fBodyGyroMeanFreqY=mean(fBodyGyroMeanFreqY),fBodyGyroMeanFreqZ=mean(fBodyGyroMeanFreqZ),fBodyAccMagMean=mean(fBodyAccMagMean),fBodyAccMagStd=mean(fBodyAccMagStd),
                        fBodyAccMagMeanFreq=mean(fBodyAccMagMeanFreq),fBodyBodyAccJerkMagMean=mean(fBodyBodyAccJerkMagMean),fBodyBodyAccJerkMagStd=mean(fBodyBodyAccJerkMagStd),fBodyBodyAccJerkMagMeanFreq=mean(fBodyBodyAccJerkMagMeanFreq),fBodyBodyGyroMagMean=mean(fBodyBodyGyroMagMean),
                        fBodyBodyGyroMagStd=mean(fBodyBodyGyroMagStd),fBodyBodyAccJerkMagMeanFreq=mean(fBodyBodyAccJerkMagMeanFreq),fBodyBodyGyroMagMean=mean(fBodyBodyGyroMagMean),fBodyBodyGyroMagStd=mean(fBodyBodyGyroMagStd),fBodyBodyGyroMagMeanFreq=mean(fBodyBodyGyroMagMeanFreq),
                        fBodyBodyGyroJerkMagMean=mean(fBodyBodyGyroJerkMagMean),fBodyBodyGyroJerkMagStd=mean(fBodyBodyGyroJerkMagStd),fBodyBodyGyroJerkMagMeanFreq=mean(fBodyBodyGyroJerkMagMeanFreq))
                                              
#tidy data set with the average of each variable for each activity and each subject.
write.table(tidyData_avg, "tidy.txt", row.names = FALSE, quote = FALSE)
tidy<-read.table("./tidy.txt")
