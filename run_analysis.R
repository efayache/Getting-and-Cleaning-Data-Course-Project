run_analysis<-function(){
    #this function perform the following actions through different procedures calls:
    #1/Merges the training and the test sets to create one data set.
    #2/Extracts only the measurements on the mean and standard deviation for each measurement.
    #3/Uses descriptive activity names to name the activities in the data set
    #4/Appropriately labels the data set with descriptive variable names. 
    
    # Call the gen_table_training function to generate the training data frame
    tbl_training <- gen_table_training()
    
    # Call the gen_table_test function to generate the test data frame
    tbl_test <- gen_table_test()
    
    #Call the merge_sets function to merge the 2 datasets into one
    tbl<- merge_sets(tbl_training,tbl_test)
    rm(list=c('tbl_training','tbl_test')) #Free memory
    
    #Call the simplify_table function that will calculate the means and standard deviation
    #and delete variables when used
    tbl <- simplify_table(tbl)
    
    #Call the Activity_description function to transform the ID value into human understable
    #labels
    tbl <- activity_description(tbl)
    return(tbl)
}


group_by_activity_subject <- function(tbl){
    #This function creates a second, independent tidy data set with the average 
    #of each variable for each activity and each subject.
    #This function is an independant function and requires the output of the run_analysis function a
    #as an input
    tbl_grouped <- data.frame(Subject_id=integer(),
                              Label=factor(),
                              average=numeric(),
                              stringsAsFactors = FALSE)
    
    for (i in unique(tbl$Subject_id)) {
        #cat('Subject ID=', i,'\n')
        for (j in unique(tbl$Labels)){
            #cat('Label=',j,'\n')
            tmp <- filter(tbl,Subject_id == i & Labels == j)
            mean <- mean(tmp$mean)
            #cat('Subject ID=', i, ' Label=', j,' mean=', mean,'\n')
            tbl_grouped <- rbind(data.frame(Subject_id=i,Label=j,average=mean), tbl_grouped)
        }
    }
    tbl_grouped <- arrange(tbl_grouped,Label,Subject_id)
    return(tbl_grouped)
}

activity_description <- function(tbl) {
    #This function replace numbers in column 'Labels' by activities description
    tbl$Labels <- sub(1,'WALKING',tbl$Labels)
    tbl$Labels <- sub(2,'WALKING_UPSTAIRS',tbl$Labels)
    tbl$Labels <- sub(3,'WALKING_DOWNSTAIRS',tbl$Labels)
    tbl$Labels <- sub(4,'SITTING',tbl$Labels)
    tbl$Labels <- sub(5,'STANDING',tbl$Labels)
    tbl$Labels <- sub(6,'LAYING',tbl$Labels)
    return(tbl)
}

simplify_table <- function(tbl){
    #This function take as input the merged data set and extracts only measurement on the mean 
    #and standard deviation for each measurement
    #Because the source values are no more needed the function deal the appropriate columns
    #to free memory
    library(dplyr) # Load the dplyr library to facilitate data frame manipulations
    
    means <- rowMeans(select(tbl,V1:V556)) #Compute the means for each observables
    sd <- rowMeans(select(tbl,V1:V556)) #Compute the Standard deviation of each observable
    
    #Add the 2 columns means and standard deviation to the table then delete the no more
    #needed columns
    tbl <- mutate(tbl,mean= means, Standard_Deviation= sd) 
    tbl <- select(tbl, Subject_id, Activity_set,Labels, mean, Standard_Deviation)
    return(tbl)
}

merge_sets <- function(set1,set2){
    #This function take the 2 datasets as input and merge them
    tbl_merged <- rbind(set1,set2)
    
}

gen_table_training <-function() {
    #This function will read the Training set (X_Train) and the Training labels and generated 
    #a merged data.frame
    #The function also delete the Variables columns that are no more needed
    
    tbl_xtrain <- read.table('data/train/X_train.txt',sep = '')
    
    tbl_ytrain <- read.table('data/train/y_train.txt',sep = '')
    names(tbl_ytrain) <- 'Labels'
    
    tbl_subject <- read.table('data/train/subject_train.txt',sep = '')
    names(tbl_subject) <- 'Subject_id'
    
    tbl_merged <- cbind(tbl_subject,tbl_ytrain,tbl_xtrain) # bind the 3 tables
    
    #Create a vector with the training label and bind it to the table
    v_label <- as.data.frame(rep('training',nrow(tbl_merged)))
    names(v_label) <- 'Activity_set'
    tbl_merged <- cbind(v_label,tbl_merged)

    return(tbl_merged)
}

gen_table_test <-function() {
    #This function will read the Test set (X_Test) and the Test labels and generated 
    #a merged data.frame
    
    tbl_xtest <- read.table('data/test/X_test.txt',sep = '')
   
     tbl_ytest <- read.table('data/test/y_test.txt',sep = '')
    names(tbl_ytest) <- 'Labels'
    
    tbl_subject <- read.table('data/test/subject_test.txt',sep = '')
    names(tbl_subject) <- 'Subject_id'
    
    tbl_merged <- cbind(tbl_subject,tbl_ytest,tbl_xtest) # bind the 3 tables
    
    #Create a vector with the training label and bind it to the table
    v_label <- as.data.frame(rep('test',nrow(tbl_merged)))
    names(v_label) <- 'Activity_set'
    
    tbl_merged <- cbind(v_label,tbl_merged)
    return(tbl_merged)
}