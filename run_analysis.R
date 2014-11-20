library(stringr)
library(dplyr)
library(tidyr)

#read activity info
activity<-read.table("./UCI HAR Dataset/activity_labels.txt",sep="",stringsAsFactor=FALSE,header=FALSE,col.names=c("ActivityID","Activity"))

#read features
features<-read.table("./UCI HAR Dataset/features.txt",sep="",stringsAsFactor=FALSE, header=FALSE)

#make features easier to read
features<-features[,2]
features<-gsub("-[mM]ean","Mean",features)
features<-gsub("-[sS]td", "Std", features)
features<-gsub("^t","Time",features)
features<-gsub("^f","Freq",features)
features<-gsub("\\(\\)","",features)

#read test data set
testX<-read.table("./UCI HAR Dataset/test/X_test.txt",sep="",header=FALSE)
testY<-read.table("./UCI HAR Dataset/test/y_test.txt",sep="",header=FALSE)
testSubject<-read.table("./UCI HAR Dataset/test/subject_test.txt",sep="",header=FALSE)
test<-cbind(testX,testY,testSubject)

#read train data set
trainX<-read.table("./UCI HAR Dataset/train/X_train.txt",sep="",header=FALSE)
trainY<-read.table("./UCI HAR Dataset/train/y_train.txt",sep="",header=FALSE)
trainSubject<-read.table("./UCI HAR Dataset/train/subject_train.txt",sep="",header=FALSE)
train<-cbind(trainX,trainY,trainSubject)

#combine train and test data sets
data<-rbind(test,train)

#rename column names
names(data)<-c(features,"ActivityID","Subject")

#keep mean and standard deviation features
keepColumns<-c(grepl("mean|std",tolower(names(data))))
#also keep the last 2 columns
temp<-(length(keepColumns)-1):length(keepColumns)
keepColumns[temp]<-TRUE

#merge with activity labels data and drop activity ID
data<-data[,keepColumns]
data<-merge(data,activity,by="ActivityID",all=FALSE)
data$ActivityID<-NULL

#tidy up data to form a "narrow and long" data set
tidydata<-gather(data,Feature,Value,-c(Activity, Subject))
#summarize average by activity, subject, and feature
tidydata<-tidydata%>%
    group_by(Activity, Subject, Feature)%>%
    summarize(Mean=mean(Value))%>%
    arrange(Activity,Subject)

#output to a file
write.table(tidydata,"tidy.txt",sep="\t",row.names=FALSE)
