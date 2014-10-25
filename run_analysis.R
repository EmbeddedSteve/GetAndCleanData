## Coursera:    Getting and Cleaning Data - Proeject Assignments 
## Author:      embeddedsteve@gmail.com
#################################################################################
# Data Set Used for this project is located:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# Information on this data set is located:
#
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#
# Grateful Acknowledgment of the DataSet Used:
# [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and 
# Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a 
# Multiclass Hardware-Friendly Support Vector Machine. International Workshop of
# Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012.
# This dataset is distributed AS-IS and no responsibility implied or explicit can
# be addressed to the authors or their institutions for its use or misuse. Any
# commercial use is prohibited. Jorge L. Reyes-Ortiz, Alessandro Ghio,
# Luca Oneto, Davide Anguita. November 2012.
##############################################################################

#
# Please see the Readme.md and Codebook.md for additional details on this Project Assignment
# 

run_analysis <- function() 
{
    ## Load libraries used in this script
    library(reshape2)
    
    ## NOTE:  User MUST set the Working Directory prior to running this script!!
    ## Check that we have the DataSet in this directory - see Readme for assumptions!
    dataSet = "./UCI HAR Dataset/"
    if (file.exists(file.path(dataSet)))  {
        cat("The DataSet Exists - we may continue!\n")
    }
    else {
        stop("The DataSet DOES NOT EXIST:  Please download and unzip!")
    }
    
    cat("Begin \n")
    
    ## The 561 "feature" sets - will be stripped later
    features<-read.table(paste(dataSet, "features.txt", sep=""))
    
    cat("Readin Test Data \n")
    ## Read in the TEST Data tables, all 3 of them - representing the 30% data
    testX_data<-read.table(paste(dataSet,"test/X_test.txt",sep=""), col.names=features[,2], check.names=FALSE)
    
    testY_data<-read.table(paste(dataSet,"test/Y_test.txt",sep=""), col.names="activity")
    
    testSubj_data<-read.table(paste(dataSet,"test/subject_test.txt",sep=""), col.names="subject")
    
    cat("Readin Train Data \n")
    ## Read in the TRAIN Data tables, all 3 of them - representing the 70% data
    trainX_data<-read.table(paste(dataSet,"train/X_train.txt",sep=""), col.names=features[,2], check.names=FALSE)
    
    trainY_data<-read.table(paste(dataSet,"train/Y_train.txt",sep=""), col.names="activity")
    
    trainSubj_data<-read.table(paste(dataSet,"train/subject_train.txt",sep=""), col.names="subject")
    
    cat("Combine the Data \n")
    ## Here we now bind by row the two table together 
    xData <- rbind(testX_data, trainX_data)
    yData <- rbind(testY_data, trainY_data)
    sData <- rbind(testSubj_data, trainSubj_data)
    
    cat("combined data:\n")
    cat("xData =", dim(xData), "\n")
    cat("yData =", dim(yData), "\n")
    cat("sData =", dim(sData), "\n")
    
    cat("Strip Features to mean & std only \n")
    # Strip out all the features not including "mean" and "std" in their names
    filter_features<-grep("mean\\(|std\\(", features[, 2]) #Find only those with mean or std in their names
    # Gives the vector of 66 columns, excludes the meanFreq() and Mean* names!
    
    ## Subset this data
    fltrX_data <- xData[, filter_features]
    
    ## Now, combine all three tables into a single, table
    combFltrData <- cbind(sData, yData, fltrX_data)
    cat("Final Combined dataset =", dim(combFltrData), "\n")
    
    ## read in the Activity labels to translate the index number to a meaningful word.
    act_labels<-read.table(paste(dataSet, "activity_labels.txt", sep=""),  col.names=c("activity", "name"))
    
    ## Apply these meaningful labels to the DataSet
    combFltrData$activity <- factor(combFltrData$activity, levels=act_labels$activity, label=act_labels$name)
    cat("write out:  combFltrData.txt \n")
    write.table(combFltrData, file="combFltrData.txt", quote=FALSE, row.names=FALSE, sep="\t")
    cat("reshpae the data \n")
    
    ## Reshape the data.
    meltSumData <- melt(combFltrData, id=c("subject", "activity"))
    
    ## Here, we do step 5, the final mean of all the reading that were reshaped above.
    tidydata <- dcast(meltSumData, activity + subject ~ variable, mean)
    cat("final tidydata table =", dim(tidydata), "\n")
    cat("write tidydata.txt \n")
    write.table(tidydata, file="tidydata.txt", quote=FALSE, row.names=FALSE, sep="\t")
    
    ## Script complete!!
    return("Done!")
    
}
