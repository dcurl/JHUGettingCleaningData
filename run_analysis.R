############################################################################
### PART 1 - MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET ###
############################################################################

######################################################################
# IMPORT, EXAMINE AND CLEAN FEATURES AND LABELS ASSOCIATED WITH DATA #
######################################################################

# STEP 1 - Import Features text file
FEATURES <- read.table("~/features.txt", header = FALSE, sep = "", dec = ".")

        # STEP 1A - Examine Features table
        head(FEATURES) #Sample Data
        names(FEATURES) #Column Names
        dim(FEATURES) #Dimensions 561 rows, 2 columns
        
        # STEP 1B - Clean up Features table
        names(FEATURES) <- c("feature_id", "feature_label") #rename columns

# STEP 2 - Import Activity Labels text file
ACTIVITY <- read.table("~/activity_labels.txt", header = FALSE, sep = "", dec = ".")

        # STEP 2A - Examine Activity table
        head(ACTIVITY) #Sample Data
        names(ACTIVITY) #Column Names
        dim(ACTIVITY) #Dimensions 6 rows, 2 columns
        
        # STEP 2B - Clean up Activity table
        names(ACTIVITY) <- c("activity_id", "activity_label") #rename columns

######################################################################
# IMPORT, EXAMINE AND CLEAN TEST SET                                 #
######################################################################

# STEP 3 - Import Subjects text file (1-30, the people that took part in the study)
SUBJ_test <- read.table("~/test/subject_test.txt", header = FALSE, sep = "", dec = ".")

        # STEP 3A - Examine Test Subjects table
        head(SUBJ_test) #Sample Data
        names(SUBJ_test) #Column Names
        dim(SUBJ_test) #Dimensions 2947 rows, 1 column
        
        # STEP 3B - Clean up Test Subjects table
        names(SUBJ_test) <- "subject_id"

# STEP 4 - Import Data associated with each person that took part in the study
DATA_test <- read.table("~/test/X_test.txt", header = FALSE, sep = "", dec = ".")

        # STEP 4A - Examine Data table
        head(DATA_test) #Sample Data
        names(DATA_test) #Column Names
        dim(DATA_test) #Dimensions 2947 rows, 561 columns, 
        # from this I am guessing every row in the FEATURES$feature_label column, 
        # corresponds to a column in the DATA_test table.
        
        # STEP 4B - Clean up Data table
        names(DATA_test) <- FEATURES$feature_label #Rename columns
        # to match the rows of the FEATURES$feature_label column

# STEP 5 - Import numeric Labels associated with each row. These labels correspond to the type of activity
LABELS_test <- read.table("~/test/y_test.txt", header = FALSE, sep = "", dec = ".")

        # STEP 5A - Examine test Labels table
        head(LABELS_test) #Sample Data
        names(LABELS_test) #Column Names
        dim(LABELS_test) #Dimensions 2947 rows, 1 column
        
        # STEP 5B - Clean up test Labels table
        names(LABELS_test) <- c("activity_id") #rename existing column
        library(plyr)
        LABELS_test <- arrange(join(LABELS_test, ACTIVITY), activity_id)

######################################################################
# IMPORT, EXAMINE AND CLEAN TRAINING SET                             #
######################################################################

# STEP 6 - Import Subjects text file (1-30, the people that took part in the study)
SUBJ_train <- read.table("~/train/subject_train.txt", header = FALSE, sep = "", dec = ".")
        
        # STEP 6A - Examine train Subjects table
        head(SUBJ_train) #Sample Data
        names(SUBJ_train) #Column Names
        dim(SUBJ_train) #Dimensions 7352 rows, 1 column
        
        # STEP 3B - Clean up train Subjects table
        names(SUBJ_train) <- "subject_id"
        
# STEP 7 - Import Data associated with each person that took part in the study
DATA_train <- read.table("~/train/X_train.txt", header = FALSE, sep = "", dec = ".")
        
        # STEP 7A - Examine train Data table
        head(DATA_train) #Sample Data
        names(DATA_train) #Column Names
        dim(DATA_train) #Dimensions 7352 rows, 561 columns, 
        # from this I am guessing every row in the FEAT_test$V2 column, 
        # corresponds to a column in the DATA_test table.
        
        # STEP 7B - Clean up train Data table
        names(DATA_train) <- FEATURES$feature_label #Rename columns
        # to match the rows of the FEATURES$feature_label column
        
# STEP 8 - Import numeric Labels associated with each row. These labels correspond to the type of activity
LABELS_train <- read.table("~/train/y_train.txt", header = FALSE, sep = "", dec = ".")
        
        # STEP 8A -Examine train Labels table
        head(LABELS_train) #Sample Data
        names(LABELS_train) #Column Names
        dim(LABELS_train) #Dimensions 7352 rows, 1 column
        
        # STEP 8B - Clean up train Labels table
        names(LABELS_train) <- c("activity_id") #rename existing column
        library(plyr)
        LABELS_train <- arrange(join(LABELS_train, ACTIVITY), activity_id)

######################################################################
# COMBINE TABLES INTO A MASTER TRAINING AND TESTING TABLE            #
######################################################################

# STEP 9 - Combine table of Test Subjects, Test Labels and Test Data
# 2947 rows, 564 columns 
# 1 col from SUBJ_test, 2 cols from LABELS_test, 561 cols from DATA_test
test_master <- cbind(SUBJ_test, LABELS_test, DATA_test) 
        
        # STEP 9A - Examine test table
        head(test_master)

# STEP 10 - Combine table of Train Subjects, Test Labels and Test Data
# 7352 rows, 564 columns 
# 1 col from SUBJ_train, 2 cols from LABELS_train, 561 cols from DATA_train
train_master <- cbind(SUBJ_train, LABELS_train, DATA_train)

        # STEP 10A - Examine train table
        head(train_master)

######################################################################
# COMBINE MASTER TRAINING AND MASTER TEST TABLES INTO ONE DATASET    #
######################################################################

# STEP 11 - Combine Master Training Data Set, Master Testing Dataset
# 10299 rows, 564 columns
# 2947 rows from test_master, 7352 rows from train_master
combined_data <- rbind(train_master, test_master)
        
        # STEP 11A - Examine combined data
        head(combined_data)

############################################################################
### PART 2 - EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD     ###
### DEVIATION ON EACH MEASUREMENT                                        ###
############################################################################

# STEP 1 - Extract column names from combined_data table that contain mean or
# std dev
mean_stddev <- grep("subject|activity|mean|std", names(combined_data), value = TRUE)

# STEP 2 - Subset Data to pull only the mean and std dev fields
filtered_data <- combined_data[mean_stddev]

# STEP 3 - Examine new table of filtered columns
head(filtered_data)


############################################################################
### PART 3 - USES DESCRIPTIVE ACTIVITY NAMES TO NAME                     ###
### THE ACTIVITIES IN THE DATASET                                        ###
############################################################################

## Already completed, see PART 1, STEP 5B AND 8B. This information was
## later pulled into the test_master and train_master datasets and
## into the combined dataset which filtered_data was subsetted on
head(filtered_data$activity_label)


############################################################################
### PART 4 - APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE          ###
### VARIABLE NAMES                                                       ###
############################################################################

## Already completed, see PART 1, STEP 4B AND 7B
## later pulled into the test_master and train_master datasets and
## into the combined dataset which filtered_data was subsetted on
names(filtered_data)


############################################################################
### PART 5 - FROM THE DATA SET IN STEP 4, CREATE A SECOND, INDEPENDENT   ###
### TIDY DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY    ###
### AND EACH SUBJECT                                                     ###
############################################################################

# STEP 1 - Create new, independent data set that is copy of the filtered dataset
# from Part 4
filtered_data2 <- filtered_data

# STEP 2 - Use dplyr pipeline to assign the average (mean) of each variable,
# grouped by Activity and Subject
library(dplyr)
average_of_filtered_data <- filtered_data2 %>% group_by(subject_id, activity_label) %>% summarise_all(funs(mean))
