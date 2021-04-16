# Specify URL where file is stored
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Specify destination where file should be saved
destfile <- "C:/Users/pietrobartocci/Desktop/datasciencecoursera"

# Apply download.file function in R
fileName <- "UCIdata.zip"

if(!file.exists(fileName)){
  download.file(url,fileName, mode = "wb") 
}

#download.file(url, destfile)

if(!file.exists(destfile)){
  unzip("UCIdata.zip", files = NULL, exdir=".")
}

# we read the data
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")

# 1. Merges the training and tests sets to create one data set
dataSet <- rbind(X_train,X_test)
#print(head(dataSet, n=1))

# 2. Extract only the measurements on the mean and standard deviation for each measurement

# we read the features file
features <- read.table("UCI HAR Dataset/features.txt") 

# we define a variable through the grep() function which mathces the pattern "mean()|std()"
MeanStdOnly <- grep("mean()|std()", features[, 2]) 

# we build the new dataset
dataSet2 <- dataSet[,MeanStdOnly]
#View(dataSet2)

# 3. Uses descriptive activities names to name the activities in the data set.This is done through the command "levels()"

# we read the tables from the folder
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

# we merge together the subject of the tain file and of the test file
subject <- rbind(subject_train, subject_test)

# we name the column
names(subject) <- 'subject'

# we merge together the lables of the test and train files
activity <- rbind(y_train, y_test)

# we name the column
names(activity) <- 'activity'

# we create a new dataset with two clumns to the left reporting subject and activity
dataSet3 <- cbind(subject,activity, dataSet2)

act_group <- factor(dataSet3$activity)
levels(act_group) <- activity_labels[,2]
dataSet3$activity <- act_group

# 4. Appropriatey labels the data set with descriptive variable names

# we clean the fiìeatures strings using sapply() function
CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})

# we name the coumns of the dataset
names(dataSet3) <- c('subject','activity',CleanFeatureNames[MeanStdOnly])
#View(dataSet3)

# 5. From the dataset in step 4, creates a second, independent tidy dataset with the average of each variable for each activity and each subject

# control if reshape package is installed
if (!"reshape2" %in% installed.packages()) {
  install.packages("reshape2")
}
library("reshape2")

# we order the dataset based on both subject and activity
baseData <- melt(dataSet3,(id.vars=c("subject","activity")))
secondDataSet <- dcast(baseData, subject + activity ~ variable, mean)

# we name the columns
names(secondDataSet)[-c(1:2)] <- paste("[mean of]" , names(secondDataSet)[-c(1:2)] )

#we veiw the final database
View(secondDataSet)

# we write the database
write.table(secondDataSet, "tidy_data.txt", sep = ",")