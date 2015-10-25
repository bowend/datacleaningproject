## This script merges and cleans the Activity recognition Test and Training datasets.

## This function returns a dataset containing all records from 'test' and 'train' sets, but only the ID, activity, mean and standard deviation columns
## This is the dataset specified in step 4 of the assignment brief
mergeandsubset <- function(x) {
    ## this line provides platform independance of this code between Windows and Unix/Mac systems.
    s <- .Platform$file.sep  
    ## Check that the function has been passed a directory, with the expected subdirectory structure
    if (!dir.exists(x) | !dir.exists(paste(x,"test",sep=s)) | !dir.exists(paste(x,"train",sep=s))) {
        stop("Expected the path to a directory containing the 'UCI HAR Dataset' with sub directories 'test' and 'train'.")
    } else {
        features <- read.table(paste(x,"features.txt",sep=s))
        dataset <- rbind(compileTable(x, "test", features),compileTable(x, "train", features))
        ## keepcol  is set to a logical vector which is true for the first two columns ('subjectID' and 'activity') and all mean and standard deviaton columns
        keepcol <- c(TRUE,TRUE,keepcols(features[,2]))
        dataset[,keepcol]
    }
}

## examines a vector containing the feature names and returns a logical vector indicating which are means and standard deviations
keepcols <- function(x) {
    y <- c()
    for (i in seq_along(x)) {
        ## the if statement adds TRUE to the vector if the feature name contains 'mean()' or 'std()' or FALSE otherwise
        if (length(grep("*mean()*",x[i],ignore.case=TRUE)) > 0 || length(grep("*std()*",x[i],ignore.case=TRUE)) > 0) {
            y[i] <- TRUE
        } else {
            y[i] <- FALSE
        }
    }
    y
}

## Takes the location of the UCI HAR Dataset as the first argument and either 'test' or 'train' as the second argument
## Builds the inidcated dataset into a table
compileTable <- function(x, y, features=NULL) {
    ## the plafrom specific separator is assigned to 's' for readability
    ## this also provides platform independance of this code between Windows and Unix/Mac systems.
    s <- .Platform$file.sep
    z <- paste(x,y,sep=s)
    subjectID <- read.table(paste(z,paste("subject_",y,".txt",sep=""),sep=s))
    labels <- cut(read.table(paste(z,paste("y_",y,".txt",sep=""),sep=s))[,1],breaks=c(0:6),labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
    observations <- read.table(paste(z,paste("x_",y,".txt",sep=""),sep=s))
    ## features is usually a free variable supplied by the calling function, the if statement allows this function to be used independantly
    if (is.null(features)) features <- read.table(paste(x,"features.txt",sep=s))
    colnames(observations) <- features[,2]
    dt <- data.frame(subjectID,labels)
    colnames(dt) <- c("subjectID","activity")
    dt <- data.frame(dt,observations)
    dt
}

## returns a table with the average of each variable for each activity and each subject
## these are the data specified at step 5 of the assignment
avgbysubjectandactivity <- function(x) {
    #tp <- levels(interaction(x$subjectID,x$activity))
    tp <- NULL
    for (i in 3:length(x[1,])) {
        t <- tapply(x[,i], interaction(x$subjectID,x$activity), mean)
        tp <- cbind(tp,t)
    }
    colnames(tp) <- colnames(x)[3:length(colnames(x))]
    rownames(tp) <- levels(interaction(x$subjectID,x$activity))
    tp
}

## takes the location of the original dataset and outputs a file with the step5 tidy dataset
createdatafile <- function(x="UCI HAR Dataset") {
    set4 <- mergeandsubset(x)
    set5 <- avgbysubjectandactivity(set4)
    write.table(set5,"tidydataset.txt",row.names = FALSE)
}