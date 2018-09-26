#20180907 Data load
library(readr)
test<-read_csv("D:\\Maggie\\nk university\\kaggle_g_store\\all\\test.csv")
train<-read_csv("D:\\Maggie\\nk university\\kaggle_g_store\\all\\train.csv")
#discription:
# fullVisitorId- A unique identifier for each user of the Google Merchandise Store.
# channelGrouping - The channel via which the user came to the Store.
# date - The date on which the user visited the Store.
# device - The specifications for the device used to access the Store.
# geoNetwork - This section contains information about the geography of the user.
# sessionId - A unique identifier for this visit to the store.
# socialEngagementType - Engagement type, either "Socially Engaged" or "Not Socially Engaged".
# totals - This section contains aggregate values across the session.
# trafficSource - This section contains information about the Traffic Source from which the session originated.
# visitId - An identifier for this session. This is part of the value usually stored as the _utmb cookie. This is only unique to the user. For a completely unique ID, you should use a combination of fullVisitorId and visitId.
# visitNumber - The session number for this user. If this is the first session, then this is set to 1.
# visitStartTime - The timestamp (expressed as POSIX time).

#20180920 Cleaning Data
library(jsonlite)
library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(VIM)
library(mice)

#1. datetime format cleaning
train[[12]]<-as.POSIXlt(train[[12]],origin="1970-01-01")
test[[12]]<-as.POSIXlt(test[[12]],origin="1970-01-01")
train[[2]]<-ymd(train[[2]])
test[[2]]<-ymd(test[[2]])

#2. extract wanted data, credits: Fangyang Chen
index=c()
for(i in 1:dim(train)[1])
{
  if(!is.null(fromJSON(train$totals[i])$transactionRevenue))
  {index = append(index,i)}
}
trainy=train[index,]

#3. json to dataframe

# #jsontrans1
# jsontrans<-function(dat,columnindex){
#   all<-lapply(dat[[columnindex]],fromJSON)
#   df<-data.frame(matrix(NA,nrow=length(all),ncol=length(all[[1]])))
#   for (i in 1:nrow(dat)){
#     df[i,]<-all[[i]]
#   }
#   colnames(df)<-names(all[[1]])
#   return(df)
# }
# test0<-test[sample(10000),]
# testdevice<-jsontrans(test0,3)
# testgeoNetwork<-jsontrans(test0,5)
# #testtotals<-jsontrans(test0,8)#problem
# #testtrafficSource<-jsontrans(test0,9)#problem
# train0<-train[sample(10000),]
# traindevice<-jsontrans(train0,3)
# traingeoNetwork<-jsontrans(train0,5)
# #testtotals<-jsontrans(test0,8)#problem
# #testtrafficSource<-jsontrans(test0,9)#problem

# #jsontrans2, a better but slower approach
# jsontrans2<-function(dat,columnindex){
#   all<-lapply(dat[[columnindex]],fromJSON)
#   df<-data.frame()
#   for (i in 1:nrow(dat)){
#     df<-union_all(df,data.frame(all[[i]],stringsAsFactors = FALSE))
#   }
#   return(df)
# }

#jsontrans3, my newest version!
jsontrans3<-function(dat,columnindex){
  all<-lapply(dat[[columnindex]],fromJSON)
  df<-data.frame()
  for(i in 1:length(all)){
    df<-union_all(df,data.frame(transpose(all[[i]]),stringsAsFactors = FALSE))
  }
  return(df)
}
set.seed(1)
test0<-test[sample(10000),]
testdevice<-jsontrans3(test0,3)
testgeoNetwork<-jsontrans3(test0,5)
testtotals<-jsontrans3(test0,8)
testtrafficSource<-jsontrans3(test0,9)
traindevice<-jsontrans3(trainy,3)
traingeoNetwork<-jsontrans3(trainy,5)
traintotals<-jsontrans3(trainy,8)
traintrafficSource<-jsontrans3(trainy,9)

#4. Dealing with NA
#recognizing na
trainy[c(3,5,8,9)]<-NULL
sum(trainy[-c(2,8)]=="not available in demo dataset")#there's no such nas in trainy, so we can ignore them
makena<-function(df){
  data.frame(lapply(df, function(x){replace(x, x=="(none)"|x=="not available in demo dataset"|x=="(not set)"|x=="(not provided)",NA)}))
}
dflst<-c(traindevice,traingeoNetwork,traintotals,traintrafficSource)
trainyall<-cbind(trainy,makena(dflst))
#deleting na
missing<-aggr(trainyall,plot = FALSE)[[5]]#missing values count in each column
deletenalst<-function(alldf,missingdf,tol=1){
  threshold<-nrow(alldf)*tol
  dellist<-missingdf[missingdf$Count>=threshold,"Variable"]
  return(dellist)
}
dellist<-deletenalst(trainyall,missing)
trainyall[,dellist]<-NULL

#5.Dealing with repetitive values
trainyall["uniqueid"]<-paste(trainyall$fullVisitorId,trainyall$sessionId)
trainyall[trainyall[['uniqueid']]=='2571951630476198714 2571951630476198714_1472105745',]