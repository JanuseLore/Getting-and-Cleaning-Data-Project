## Unzipping and Downloading Data

# string variables for file download
FileName <- "UCIdata.zip"
URL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
DIR <- "UCI HAR Dataset"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(fileName)){
        download.file(URL,fileName, mode = "wb") 
}

# File unzip verification. If the directory does not exist, unzip the downloaded file.
if(!file.exists(DIR)){
	unzip("UCIdata.zip", files = NULL, exdir=".")
}


## Reading of Data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

## Analysis Phase
# 1. Merges the 'train'and 'test' sets to create one data set.
dataSet <- rbind(X_train,X_test)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement. 
meanstddevOnly <- grep("mean()|std()", features[, 2]) 
dataSet <- dataSet[,meanstddev]


# 4. labeling the data set with descriptive activity names.
FeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(dataSet) <- FeatureNames[meanstddevOnly]

# combining 'test' and 'train' of subject data and activity data
subject <- rbind(subject_train, subject_test)
names(subject) <- 'subject'
activity <- rbind(y_train, y_test)
names(activity) <- 'activity'

# combining subject, activity, mean and std deviation only data set to create final data set.
dataSet <- cbind(subject,activity, dataSet)


# 3. Use descriptive activity names
act_group <- factor(dataSet$activity)
levels(act_group) <- activity_labels[,2]
dataSet$activity <- act_group


# 5. Creates an independent tidy data set with the mean of each variable for each activity and each subject. 

# check if reshape2 package is installed
if (!"reshape2" %in% installed.packages()) {
	install.packages("reshape2")
}

library("reshape2")

# Write the tidy data to the wd as "tidy_data.txt"
base <- melt(dataSet,(id.vars=c("subject","activity")))
seconddataSet <- dcast(base, subject + activity ~ variable, mean)
names(seconddataSet)[-c(1:2)] <- paste("[mean of]" , names(seconddataSet)[-c(1:2)] )
write.table(seconddataSet, "tidy_data.txt", sep = ",")
