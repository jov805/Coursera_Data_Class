
#Getting and Cleaning Data Course Project
#Josey VanOrsdale
#coursera Cleaning data 
#flyingypsygrl@yahoo.com

################################################################################
###### PART 0- PREP ###############

setwd("C:\\Users\\Jeff\\Documents\\clean data class\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")

#Load necessary Packages
library(dplyr)
library(data.table)
library(tidyr)

################################################################################
###### PART 1- Data contains both training and testing data ###############


#Read all data out of zip file into appropriate tables
train_x <-read.table("train\\X_train.txt")
train_y <-read.table("train\\Y_train.txt")
train_subject <- read.table("train\\subject_train.txt")
test_x <- read.table("test\\x_test.txt")
test_y <- read.table("test\\y_test.txt")
test_subject <- read.table("test\\subject_test.txt")
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

#label columns of the training data
colnames(activity_labels) = c('activityID','exerciseTYPE')
colnames(train_subject) = "subjectID"
colnames(train_x) = features[,2]
colnames(train_y) = ("activityID")

#Combine training data into one table
train_totaldata = cbind(train_y, train_subject, train_x)

#label columns of the testing data
colnames(test_subject) = "subjectID"
colnames(test_x) = features[,2]
colnames(test_y) = ("activityID")

#Combine testing data into one table
test_totaldata = cbind(test_y, test_subject, test_x)

#Combine testing and training data into one table
totaldata = rbind(train_totaldata, test_totaldata)

################################################################################
###### PART 2- Data contains only that of Mean and Std.deviations ###############

#search the features table for the column names that contain mean's and std's
col_total <- grep(".*mean().*|.*std.*", features$V2, ignore.case=TRUE, value=TRUE)

#pull out the columns from the data with selected column names (with Mean and std's)
totaldata <- totaldata[,col_total]

#reisolate id columns from test and train data
testid <- test_totaldata[,1:2]
trainid <- train_totaldata[,1:2]

#Combine testing and training id columns into one table
totalid = rbind(trainid, testid)

#add id columns back into final data
finaldata = cbind(totalid, totaldata)

#isolate columns with "meanFreq"
colNames <- colnames(finaldata)
grep("Freq",colNames)

#delete columns with "meanFreq"
finaldata <- finaldata[c(-49,-50, -51, -58, -59, -60, -67, -68, -69, -72, -75, -78,-81)]

#Verify no columns other than id, Mean and standard deviations
colnames(finaldata)

################################################################################
###### PART 3- data contains descriptive activity names  ###############

#assign the activity names
finaldata = merge(finaldata, activity_labels, by="activityID" , all.x = TRUE)

################################################################################
###### PART 4- data contains descriptive activity names  ###############

#assign the column names to its own variable
colnames<-colnames(finaldata)

#rename the column names appropriately
for (i in 1:length(colnames)) 
{
  colnames[i] = gsub("\\()","",colnames[i])
  colnames[i] = gsub("-std$","_STD_DEV-",colnames[i])
  colnames[i] = gsub("-std-","_STD_DEV-",colnames[i])
  colnames[i] = gsub("-mean","_MEAN",colnames[i])
  colnames[i] = gsub("^(t)","Time_",colnames[i])
  colnames[i] = gsub("^(f)","Frequency_",colnames[i])
  colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
  colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
  colnames[i] = gsub("[Gg]yro","Gyroscope",colnames[i])
  colnames[i] = gsub("AccMag","Accelerometer_Magnitude",colnames[i])
  colnames[i] = gsub("([Bb]odyaccjerkmag)","Body_Accelerometer_Jerk_Magnitude",colnames[i])
  colnames[i] = gsub("JerkMag","Jerk_Magnitude",colnames[i])
  colnames[i] = gsub("GyroMag","Gyroscope_Magnitude",colnames[i])
  colnames[i] = gsub("GravityMean","GravityMEAN",colnames[i])
  colnames[i] = gsub("angle", "Angle_", colnames[i])
  colnames[i] = gsub("subjectID", "SubjectID", colnames[i])
  colnames[i] = gsub("exerciseTYPE", "Activity_Type", colnames[i])
  colnames[i] = gsub("activityID", "ActivityID", colnames[i])
  colnames[i] = gsub("Acc", "Accelerometer", colnames[i])
};

#reassign colnames to the data
colnames(finaldata) = colnames

################################################################################
###### PART 5-independent tidy data set with the average of each variable for each activity & subject  ###############

#sort the means of each variable in order of subject and activity
finaldata$Activity_Type <- as.character(finaldata$Activity_Type)
dataAggr <- aggregate(.~ SubjectID - Activity_Type, data = finaldata, mean)
tidydata <- tbl_df(arrange(dataAggr,SubjectID,Activity_Type))

#organize the original data
finaldata <- tbl_df(arrange(finaldata, SubjectID,Activity_Type))

# Exporting tidydata as a .txt
write.table(tidydata, './tidyData.txt', row.names = TRUE, sep = ',')
