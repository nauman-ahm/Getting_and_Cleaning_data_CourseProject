# Script for steps 1-5 outlined in assignment 
## Download the raw data from given website
if (!file.exists("UCI HAR Dataset"))
{
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, "dataset.zip")
  unzip("dataset.zip")
}
setwd("UCI Har Dataset")

## Set working directory to "UCI Har Dataset/train"
setwd("train")

## Reading the "subject_train.txt", "X_train.txt", and "y_train.txt" files
subject<-read.table("subject_train.txt", header = FALSE, sep = "\n")
training_labels<-read.table("y_train.txt", header = FALSE, sep="\n")
training_data<-read.table("X_train.txt", header=FALSE, sep = "")

## Merging the 3 DF's into 1 Data Frame and removing unwanted DF'S
training_data<-data.frame(subject$V1, training_labels$V1, training_data)
rm(subject, training_labels)

## Set working directory to "UCI Har Dataset/test"
setwd("..")
setwd("test")

## Reading the "subject_test.txt", "X_test.txt", and "y_test.txt" files
subject<-read.table("subject_test.txt", header = FALSE, sep = "\n")
test_labels<-read.table("y_test.txt", header = FALSE, sep="\n")
testing_data<-read.table("X_test.txt", header=FALSE, sep = "")

## Merging the 3 DF's into 1 Data Frame and removing unwanted DF'S
testing_data<-data.frame(subject$V1, test_labels$V1, testing_data)
rm(subject, test_labels)

## change directory to "UCI Har Dataset"
setwd("..")
## script to read the "features.txt" file and remove the numbers on features DF
features<-read.csv("features.txt", header = FALSE, sep = "\n")
features$V1<-sub("^[ |[[:digit:]]+","",features$V1)

## script to add the col names to train_set and test_set
colnames(training_data)[1] = "subject"
colnames(training_data)[2] = "activity_label"
colnames(testing_data)[1] = "subject"
colnames(testing_data)[2] = "activity_label"
colnames(training_data)[3:563] = features$V1
colnames(testing_data)[3:563] = features$V1

## Adding identifier column prior to merge
testing_data$data_catagory = "test"
training_data$data_catagory = "train"
## Merging training and test sets into one data set and removing unwanted DF's
Complete_data<-rbind(testing_data, training_data)
rm(testing_data, training_data, features)

##Adding descriptive labels to activity_label and rearranging
Complete_data$activity_description<- ifelse(Complete_data$activity_label ==1,"WALKING", ifelse(Complete_data$activity_label==2, "WALKING_UPSTAIRS", ifelse(Complete_data$activity_label==3, "WALKING_DOWNSTAIRS", ifelse(Complete_data$activity_label==4, "SITTING", ifelse(Complete_data$activity_label==5,"STANDING", ifelse(Complete_data$activity_label==6, "LAYING", NA))))))
Complete_data<-Complete_data[,c(1,2,564,565,c(3:563))]

## Removing all the unwanted data (keeping only means and stds)
imp<-grep("subject|activity_label|data_catagory|activity_description|mean()|std()", colnames(Complete_data)[])
Complete_data_reduced<-Complete_data[,imp]
rm (Complete_data, imp)


## Script to create second dataset with only means 

means_summary<-data.frame()
means_summary<- reshape2::melt(Complete_data_reduced, id.vars = colnames(Complete_data_reduced[,c(1:4)]))
means_summary<- dplyr::group_by(means_summary, subject, activity_label, activity_description, variable)
means_summary<- dplyr::summarise(means_summary, mean = mean(value))
means_summary<-dcast(means_summary, subject+activity_label+activity_description ~ variable, value = "mean")

## Writing means_summary to a txt file
write.table(means_summary, file = "means_summary.txt", row.names = FALSE)
