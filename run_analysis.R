#project script 

#DATA SOURCE:  
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
#              destfile = "project_data.zip")

#LOAD LIBRARY
library(dplyr)

#LOAD META DATA 
var_names <- readLines("UCI_HAR_Dataset/features.txt")
act_names <- read.table("UCI_HAR_Dataset/activity_labels.txt", 
                        col.names = c("activity_ID", "activity"))

##########################
### LOAD TRAINING DATA ###
##########################

subject_ID <- read.table("UCI_HAR_Dataset/test/subject_test.txt",
                         col.names = c("subject_ID"))

activity_ID <- read.table("UCI_HAR_Dataset/test/y_test.txt", 
                          col.names = c("activity_ID"))

test_data <- read.table("UCI_HAR_Dataset/test/X_test.txt", 
                         col.names = var_names)

test_data <- bind_cols(subject_ID, activity_ID, test_data)

rm(subject_ID, activity_ID)

#########################
### LOAD TESTING DATA ###
#########################

subject_ID <- read.table("UCI_HAR_Dataset/train/subject_train.txt",
                         col.names = c("subject_ID"))

activity_ID <- read.table("UCI_HAR_Dataset/train/y_train.txt", 
                          col.names = c("activity_ID"))

train_data <- read.table("UCI_HAR_Dataset/train/X_train.txt", 
                         col.names = var_names)

train_data <- bind_cols(subject_ID, activity_ID, train_data)

rm(subject_ID, activity_ID)

#######################
### MERGE DATA SETS ###
#######################

data <- rbind(test_data, train_data)
rm(test_data, train_data)


######################################
### Add Descriptive Activity Names ###
######################################

data <- inner_join(data, act_names, by = "activity_ID")

##########################
### EXTRACT MEAN & STD ###
##########################
data <- select(data, 
              subject_ID, 
              activity,
              contains(".mean."), 
              contains(".std.")
)

###############################
### CLEAN UP VARIABLE NAMES ###
###############################

names(data) <- sub(".*?\\.", "", names(data))

rm(act_names, var_names)


############################
### PRODUCE NEW DATA SET ###
############################

# act_aves <- data %>%
#   group_by(activity) %>%
#   summarise_all(mean) %>%
#   select(-subject_ID)
# 
# sub_aves <- data %>%
#   select(-activity) %>%
#   group_by(subject_ID) %>%
#   summarise_all(mean)

sub_aves <- function(data){
  data %>%
    select(-activity) %>%
    group_by(subject_ID) %>%
    summarise_all(mean)
}

summary_data <- split(data, data$activity) %>%
  lapply(sub_aves) 

summary_data <- do.call(rbind, summary_data)

write.csv(summary_data, "summary_data.csv")

