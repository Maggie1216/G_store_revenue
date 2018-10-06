#20180907 Data load
library(readr)
library(tidyr)
library(dplyr)
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

#1. datetime format cleaning
library(lubridate)
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
library(jsonlite)
library(purrr)#transpose
#jsontrans3, my newest version!
jsontrans3<-function(dat,columnindex){
  all<-lapply(dat[[columnindex]],fromJSON)
  df<-data.frame()
  for(i in 1:length(all)){
    df<-union_all(df,data.frame(purrr::transpose(all[[i]]),stringsAsFactors = FALSE))#注意不要让函数覆盖！！
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
library(VIM)
library(mice)
library(nnet)
#recognizing na
trainy[c(3,5,8,9)]<-NULL
sum(trainy[-c(2,8)]=="not available in demo dataset")#there's no such nas in trainy, so we can ignore them
makena<-function(df){
  data.frame(lapply(df, function(x){replace(x, x=="(none)"|x=="not available in demo dataset"|x=="(not set)"|x=="(not provided)",NA)}))
}
dflst<-c(traindevice,traingeoNetwork,traintotals,traintrafficSource)
trainyall<-cbind(trainy,makena(dflst))
#filling na-must
trainyall[,"newVisits"]<-as.numeric(as.character(trainyall[,"newVisits"]))#un-factor this column
trainyall[,"newVisits"]<-replace(trainyall[,"newVisits"],is.na(trainyall[,"newVisits"]),0)
trainyall[,"isTrueDirect"]<-replace(trainyall[,"isTrueDirect"],is.na(trainyall[,"isTrueDirect"]),FALSE)
trainyall[,"bounces"]<-as.numeric(as.character(trainyall[,"bounces"]))#un-factor this column
trainyall[,"bounces"]<-replace(trainyall[,"bounces"],is.na(trainyall[,"bounces"]),0)
#deleting na
missing<-aggr(trainyall,plot = FALSE)[[5]]#missing values count in each column
deletenalst<-function(alldf,missingdf,tol=1){
  threshold<-nrow(alldf)*tol
  dellist<-missingdf[missingdf$Count>=threshold,"Variable"]
  return(dellist)
}
dellist<-deletenalst(trainyall,missing,0.15)#
trainyall[,dellist]<-NULL
#filling na-rest
trainyall[is.na(trainyall$country),"country"]<-"United States"#fill in mode value
trainyall[is.na(trainyall$subContinent),"subContinent"]<-"Northern America"
trainyall[is.na(trainyall$continent),"continent"]<-"Americas"


#5.Dealing with repetitive values
trainyall["uniqueid"]<-paste(trainyall$fullVisitorId,trainyall$sessionId)
trainyall[trainyall[['uniqueid']]=='2571951630476198714_1472105745',]
trainyall<-trainyall[-5812,]
trainyall[,"uniqueid"]<-NULL


#6. More cleaning: the class of each column should be correct
#(with no factors before making them into dummy)
classlist<-lapply(trainyall,class)
classdf<-data.frame(classlist,stringsAsFactors = FALSE)
classdf<-rbind(classdf,colnames(classdf))
classdf<-transpose(classdf)
colnames(classdf)<-c("class","col")
classdf#see the class of trainyall columns
for (i in c("browser","operatingSystem","deviceCategory","continent","subContinent","country","source")){
  trainyall[,i]<-as.character(trainyall[[i]])
}#turn them into character
for (i in c("visits","hits","pageviews","transactionRevenue","newVisits")){
  trainyall[,i]<-as.numeric(as.character(trainyall[[i]]))
}#turn them into integer

#7. Correcting the range of y values
#https://www.kaggle.com/c/ga-customer-revenue-prediction/discussion/65775
trainyall["transactionRevenue"]<-trainyall["transactionRevenue"]/10^6

#7. EDA
library(ggplot2)
#for continuous variables
cols<-classdf[classdf$class=="numeric","col",drop=T]
boxplot(trainyall[,cols[c(2,3)]],horizontal = TRUE)
boxplot(trainyall[,cols[4]],horizontal = TRUE)
hist(trainyall[,cols[2]])
hist(trainyall[,cols[3]])
hist(trainyall[,cols[4]])
#outliers detection
otht<-trainyall[,cols[2]]
otpv<-trainyall[,cols[3]]
oty<-trainyall[,cols[4]]
stotht<-(otht-mean(otht))/sd(otht)
mean(abs(stotht)>3)
stotpv<-(otpv-mean(otpv))/sd(otpv)
mean(abs(stotpv)>3)
stoty<-(oty-mean(oty))/sd(oty)
mean(abs(stoty)>3)
#time series
#revenue by date
transbydate<-trainyall %>% select(c(date,transactionRevenue)) %>% group_by(date) %>% summarise(sr=sum(transactionRevenue))
p1<-ggplot(aes(date, sr),data=transbydate)
p1+geom_line()+geom_smooth()
#visits by date
vstbydate<-trainyall %>% select(c(date,visits)) %>% group_by(date) %>% summarise(sr=sum(visits))
p2<-ggplot(aes(date, sr),data=vstbydate)
p2+geom_line()+geom_smooth()
#views by date
vwbydate<-trainyall %>% select(c(date,pageviews)) %>% group_by(date) %>% summarise(sr=sum(pageviews))
p3<-ggplot(aes(date, sr),data=vwbydate)
p3+geom_line()+geom_smooth()
#hits by date
htbydate<-trainyall %>% select(c(date,hits)) %>% group_by(date) %>% summarise(sr=sum(hits))
p3<-ggplot(aes(date, sr),data=htbydate)
p3+geom_line()+geom_smooth()
#corr
con_data<-trainyall%>%select(c(transactionRevenue,pageviews,hits))
#corr:pageviews and y
ggplot(aes(pageviews,transactionRevenue),data=con_data)+geom_point()
ggplot(aes(pageviews,transactionRevenue),data=con_data)+geom_point()+xlim(0,125)+ylim(0,2000)
cor(otpv,oty)
#corr:hits and y
ggplot(aes(hits,transactionRevenue),data=con_data)+geom_point()
ggplot(aes(hits,transactionRevenue),data=con_data)+geom_point()+xlim(0,125)+ylim(0,2000)
cor(otht,oty)
#corr:hits and pageviews
ggplot(aes(hits,pageviews),data=con_data)+geom_point()
cor(otht,otpv)
#corr: pageviews and log(y+1)
ggplot(aes(pageviews,log(transactionRevenue+1)),data=con_data)+geom_point()
cor(otpv,log(oty+1))
#corr: hits and log(y+1)
ggplot(aes(hits,log(transactionRevenue+1)),data=con_data)+geom_point()
cor(otht,log(oty+1))
#distribution
#distribution: log(y+1)
qqnorm(log(oty+1))#after log, y is normal
hist(log(oty+1))