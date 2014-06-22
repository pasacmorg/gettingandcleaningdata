library(data.table)
library(reshape2)

setwd('c:/users/paul/documents/R_projects/jhuds/c3p')

activity_labels <- fread('activity_labels.txt')
features        <- fread('features.txt')

subject_train   <- fread('train/subject_train.txt')
Y_train         <- data.table(read.table('train/Y_train.txt'))
X_train         <- data.table(read.table('train/X_train.txt'))

subject_test    <- fread('test/subject_test.txt')
Y_test          <- data.table(read.table('test/Y_test.txt'))
X_test          <- data.table(read.table('test/X_test.txt'))

# Appropriately labels the data set with descriptive variable names. 

setnames(subject_train, names(subject_train), 'subject')
setnames(subject_test,  names(subject_test),  'subject')

setnames(X_test,  names(X_test),  features$V2)
setnames(X_train, names(X_train), features$V2)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Labeling Activities

mean_sd_features <- grep('-mean\\(|std\\(',names(X_test))

X_test_mean_sd  <- X_test[,mean_sd_features, with=FALSE]
XY_test_mean_sd <- cbind(subject_test, activity=activity_labels$V2[Y_test$V1], X_test_mean_sd)

X_train_mean_sd  <- X_train[,mean_sd_features, with=FALSE]
XY_train_mean_sd <- cbind(subject_train, activity=activity_labels$V2[Y_train$V1], X_train_mean_sd)

merged <- rbind(XY_test_mean_sd, XY_train_mean_sd) 

mergedMelt <- melt(merged,id=c("subject","activity"))

# mean of each variable for each activity and each subject

castedMean <- dcast.data.table(mergedMelt, subject + activity ~ variable, mean)

# append '-mean()' to all non-id variable names

oldMeasures <- names(castedMean)[3:ncol(castedMean)]
newMeasures <- paste0(oldMeasures,'-mean()')
setnames(castedMean, oldMeasures, newMeasures)

write.table(castedMean, 'tidy_data.txt', row.names=FALSE, quote=FALSE, sep='\t')


