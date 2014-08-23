
require(plyr)

# Listed are Directories and files under operation
uci_directory <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_directory, "/features.txt", sep = "")
activity_labels_file <- paste(uci_directory, "/activity_labels.txt", sep = "")
x_test_file  <- paste(uci_directory, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_directory, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_directory, "/test/subject_test.txt", sep = "")
x_train_file <- paste(uci_directory, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_directory, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_directory, "/train/subject_train.txt", sep = "")


# Loading raw test and training data into environment
features_data <- read.table(feature_file, colClasses = c("character"))
activity_labels_data <- read.table(activity_labels_file, col.names = c("Activity_Id", "Activity"))
x_test_data <- read.table(x_test_file)
y_test_data <- read.table(y_test_file)
subject_test_data <- read.table(subject_test_file)
x_train_data <- read.table(x_train_file)
y_train_data <- read.table(y_train_file)
subject_train_data <- read.table(subject_train_file)


# 1.Merging training and test sets to create single dataset.

# Binding sensor data
training_sensor_data <- cbind(cbind(x_train_data, subject_train_data), y_train_data)
test_sensor_data <- cbind(cbind(x_test_data, subject_test_data), y_test_data)
sensor_merged_data <- rbind(training_sensor_data, test_sensor_data)

# Labeling columns
sensor_data_labels <- rbind(rbind(features_data, c(562, "Subject")), c(563, "Activity_Id"))[,2]
names(sensor_merged_data) <- sensor_data_labels

# 2.Extracting only the measurements on the mean and standard deviation.
sensor_merged_data_mean_std <- sensor_merged_data[,grepl("mean|std|Subject|Activity_Id", names(sensor_merged_data))]

# 3.Uses descriptive activity names to name the activities in the data set
sensor_merged_data_mean_std <- join(sensor_merged_data_mean_std, activity_labels_data, by = "Activity_Id", match = "first")
sensor_merged_data_mean_std <- sensor_merged_data_mean_std[,-1]

# 4.Appropriately labels the data set with descriptive names.
# Removing unecessary parentheses
names(sensor_merged_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_merged_data_mean_std), perl = TRUE)
# Making valid names
names(sensor_merged_data_mean_std) <- make.names(names(sensor_merged_data_mean_std))
# Make names self explanatory 
names(sensor_merged_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('\\.std',".Standard_Deviation",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('Gyro',"Angular_Speed",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('GyroJerk',"Angular_Acceleration",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('^t',"Time_Domain.",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('^f',"Frequency_Domain.",names(sensor_merged_data_mean_std))
names(sensor_merged_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_merged_data_mean_std))


# 5.Creating a second, independent tidy data set with the average of each variable for each activity and each subject.

sensor_tidy_data_set = ddply(sensor_merged_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_tidy_data_set, file = "sensor_tidy_data_set.txt")
