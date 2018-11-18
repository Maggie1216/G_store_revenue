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

#20180920-20181015 Cleaning Data

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

#8. EDA
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
#for categorical variables
catevar<-data.frame(lapply(trainyall,is.character))
vec<-colnames(catevar[,catevar==TRUE])
plot_category<-function(vec,df){
  for (i in vec){
    barplot(table(df[[i]]),main=i)
  }
}
plot_category(vec,trainyall)

#9.final touch- delete visits column, because its all ones
onehottedtrain$visits<-NULL
trainyall$visits<-NULL

#10.One-hot Code
#TRY1: encoding try, credits: Fangyang Chen
library(magrittr)
tyl=trainyall
tyl$socialEngagementType=NULL
tyl$source=NULL
oh=data.frame(model.matrix(~operatingSystem+browser+channelGrouping+browser+operatingSystem+deviceCategory+continent+subContinent+country-1,data=tyl))
tyl$operatingSystem=NULL
tyl$channelGrouping=NULL
tyl$browser=NULL
tyl$operatingSystem=NULL
tyl$deviceCategory=NULL
tyl$continent=NULL
tyl$subContinent=NULL
tyl$country=NULL
onehottedtrain=cbind(tyl,oh)
#TRY2: another try before encoding and then excoding: combine vars
#1) columns that need to be one hotted
col_to_do<-colnames(trainyall)[unlist(lapply(trainyall, is.character))]
col_to_do<-col_to_do[-2]#exclude session_id
#2) check how to combine, the summary of all character variables
for (i in col_to_do){
  print(i)
  print(table(trainyall[,i]))
}
#3) combi_funcs: a function to combine two values into one
combi_val<-function(data, col,combine1,combine2){
  ide_v1<-function(x,y,z){
        if(identical(x,y) | identical(x,z)){
          paste(y,z,sep=", ")
        }else{
          x
        }
  }
  result<-sapply(trainyall[[col]],function(x) ide_v1(x,combine1,combine2))
  return(data.frame(result,stringsAsFactors = FALSE))
}
#4) combi_funcs: a function to combine values in a same category, used on "source" column
ide_v2<-function(x,match){
  vec<-paste("*",match,sep='')
  if(is.element(x,source[grepl(vec,source)])){
    paste(match,"type")
  }else{
    x
  }
}
#5) combi_func: a function to combine values below a defined frequency, used on "source" column
ide_v3<-function(x){
  others<-source_2%>%filter(Freq<=2000)%>%select(Var1)
  if(is.element(x,others[[1]])){
    "other websites"
  }else{
    x
  }
}
#6) dealing with different columns
#for channelGrouping
trainyall["channelGrouping_com"]<-combi_val(trainyall,col_to_do[1],"(Other)","Affiliates")
table(trainyall[,"channelGrouping_com"])
trainyall["channelGrouping_com"]<-combi_val(trainyall,"channelGrouping_com","(Other), Affiliates","Display")
table(trainyall[,"channelGrouping_com"])
trainyall["channelGrouping_com"]<-combi_val(trainyall,"channelGrouping_com","(Other), Affiliates, Display","Social")
table(trainyall[,"channelGrouping_com"])
trainyall["channelGrouping_com"]<-combi_val(trainyall,"channelGrouping_com","(Other), Affiliates, Display, Social","Paid Search")
table(trainyall[,"channelGrouping_com"])
#for browser
result<-sapply(trainyall[["browser"]],function(x) ifelse(x=="Chrome",x,"Other Browsers"))
trainyall["browser_com"]<-data.frame(result, stringsAsFactors = FALSE)
table(trainyall["browser_com"])
#for operatingSystem
trainyall["operatingSystem_com"]<-combi_val(trainyall,col_to_do[3],"Windows","Windows Phone")
table(trainyall[,"operatingSystem_com"])
#for deviceCategory
trainyall["deviceCategory_com"]<-combi_val(trainyall,col_to_do[4],"mobile","tablet")
table(trainyall[,"deviceCategory_com"])
#for country
result<-sapply(trainyall[["country"]],function(x) ifelse(x=="United States",x,"Other Countries"))
trainyall["country_com"]<-data.frame(result, stringsAsFactors = FALSE)
table(trainyall[,"country_com"])
#for source
source<-unique(trainyall[["source"]])
trainyall["source_com"]<-sapply(trainyall[["source"]],function(x) ide_v2(x,"google"))
trainyall["source_com"]<-sapply(trainyall[["source_com"]],function(x) ide_v2(x,"yahoo"))
trainyall["source_com"]<-sapply(trainyall[["source_com"]],function(x) ide_v2(x,"facebook"))
source_2<-data.frame(table(trainyall["source_com"]))
trainyall["source_com"]<-sapply(trainyall[["source_com"]],function(x) ide_v3(x))
#7) wow we are almost done, lets do exploratory analysis on these columns again!
vec1<-colnames(trainyall)[19:24]
plot_category(vec1,trainyall)#after adjustment
vec0<-colnames(trainyall)[c(1,6,7,9,12,17)]
plot_category(vec0,trainyall)#after adjustment
#8) FINALLY for Encoding part
tyl_com<-trainyall
tyl_com[,c(col_to_do)]<-NULL
tyl_com[,colnames(trainyall)[19:24]]<-NULL
onehottedtrain_com<-data.frame(model.matrix(~channelGrouping_com+browser_com+operatingSystem_com+deviceCategory_com+country_com+source_com-1,data=trainyall))
onehottedtrain_com<-cbind(tyl_com,onehottedtrain_com)

#11. Create a log revenue column and adjust the onehottedtrain_com data to make it more model compatible...
onehottedtrain_com["transactionRevenue_ln"]<-log(onehottedtrain_com$transactionRevenue)
modeldata<-onehottedtrain_com[,4:25]#exclude columns of id and date
modeldata<-modeldata[,-5]#exclude y, we only need lny
modeldata$isMobile<-as.integer(modeldata$isMobile)#change the typeof of isMobile from logic to int
modeldata$isTrueDirect<-as.integer(modeldata$isTrueDirect)#same as above

#12. More exploratory data analysis(I didn't do this corrplot before...)
library(corrplot)
#corrplot the variables, the last one is lny
corrplot(cor(modeldata),method="color",type="upper",tl.col = "black",tl.pos="lt",tl.cex = 0.5)
corrplot(cor(modeldata),add=TRUE,method="number",type="lower",number.cex = 0.6,diag=FALSE,cl.pos="n",tl.pos = "n")
#make adjustments
modeldata$isMobile<-NULL#this variable is highly relevant with device category, no use then
modeldata$hits<-NULL#this variable is hightly relevant with pageviews and it is not recommended using due to kaggle's description
#corrplot it again, the last one is lny
corrplot(cor(modeldata),method="color",type="upper",tl.col = "black",tl.pos="lt",tl.cex = 0.5)
corrplot(cor(modeldata),add=TRUE,method="number",type="lower",number.cex = 0.6,diag=FALSE,cl.pos="n",tl.pos = "n")

#13. dealing with date data (adding two more variables, going through the process 10-12 again, including encoding)
modeldata["weekday"]<-wday(onehottedtrain_com[[1]],label=TRUE)#adding weekday variable,1 is monday and so on
modeldata["month"]<-month(onehottedtrain_com[[1]],label=TRUE)#adding month variable
modeldata["year"]<-year(onehottedtrain_com[[1]])
modeldata["quarter"]<-quarter(onehottedtrain_com[[1]])
wddf<-modeldata%>%select(weekday,transactionRevenue_ln)%>%group_by(weekday)%>%summarise(tr=sum(transactionRevenue_ln))
mdf<-modeldata%>%select(month,transactionRevenue_ln)%>%group_by(month)%>%summarise(tr=sum(transactionRevenue_ln))
ydf<-modeldata%>%select(year,transactionRevenue_ln)%>%group_by(year)%>%summarise(tr=sum(transactionRevenue_ln))
qdf<-modeldata%>%select(quarter,transactionRevenue_ln)%>%group_by(quarter)%>%summarise(tr=sum(transactionRevenue_ln))
ggplot(aes(weekday,tr),data=wddf)+geom_col()
ggplot(aes(month,tr),data=mdf)+geom_col()
ggplot(aes(year,tr),data=ydf)+geom_col()#decided to ignore this variable
ggplot(aes(quarter,tr),data=qdf)+geom_col()#decided to ignore this variable
modeldata$year<-NULL
modeldata$quarter<-NULL
modeldata$month<-as.character(modeldata$month)
modeldata$weekday<-as.character(modeldata$weekday)
#going though ENCODING(part 10) again!!! encoding these two columns and put them into onehottedtrain_com and then to modeldata
trainyall["month"]<-modeldata$month
trainyall["weekday"]<-modeldata$weekday
col_to_do<-colnames(trainyall)[unlist(lapply(trainyall, is.character))]
col_to_do<-col_to_do[-2]#exclude session_id
tyl_com<-trainyall
tyl_com[,c(col_to_do)]<-NULL
tyl_com[,colnames(trainyall)[19:24]]<-NULL
onehottedtrain_com<-data.frame(model.matrix(~channelGrouping_com+browser_com+operatingSystem_com+deviceCategory_com+country_com+source_com+month+weekday-1,data=trainyall))
onehottedtrain_com<-cbind(tyl_com,onehottedtrain_com)
#going through data manipulating (part 11) again!!!
onehottedtrain_com["transactionRevenue_ln"]<-log(onehottedtrain_com$transactionRevenue)
modeldata<-onehottedtrain_com[,4:length(onehottedtrain_com)]#exclude columns of id and date
modeldata<-modeldata[,-5]#exclude y, we only need lny
modeldata$isMobile<-as.integer(modeldata$isMobile)#change the typeof of isMobile from logic to int
modeldata$isTrueDirect<-as.integer(modeldata$isTrueDirect)#same as above
#going through another eda solution (part 12) again!!!
modeldata$isMobile<-NULL
modeldata$hits<-NULL

#20181015 Modeling
#Ok I'm calling the variable "hits" back!
modeldata["hits"]<-trainyall["hits"]
#0. Some useful tools and dividing data sets into train and test
get_rse<-function(testdataset,y_testdataset,y_predict){
  a<-sum((y_predict-testdataset[[y_testdataset]])^2)
  b<-var(testdataset[[y_testdataset]])
  return(a/b)
}
get_rmse<-function(testdataset,y_testdataset,y_predict){
  a<-sum((y_predict-testdataset[[y_testdataset]])^2)
  return(sqrt(a/nrow(testdataset)))
}
# assign test and train set
modeldata_copy<-modeldata
n<-nrow(modeldata_copy)
l<-length(modeldata_copy)
modeldata_copy['train']<-rep(0,n)
modeldata_copy[sample(n,n/2),'train']<-1
train_copy<-modeldata_copy[modeldata_copy['train']==1,-(l+1)]
test_copy<-modeldata_copy[modeldata_copy['train']!=1,-(l+1)]

#1. RandomForest
library(randomForest)
r2<-rep(NA,l-1)
mse<-rep(NA,l-1)
set.seed(100)
for (i in 1:(l-1)){
  rf<-randomForest(transactionRevenue_ln~.,data=train_copy,mtry=i)
  r2[i]<-mean(rf$rsq)
  mse[i]<-mean(rf$mse)
  cat("mtry is ",i,", mse is ",mse[i],"\n")
}
#choose mtry=3
rf_best<-randomForest(transactionRevenue_ln~.,data=train_copy,mtry=3)
plot(rf_best)
rf_mse<-sqrt(mean(rf$mse))#1.087

#2. GBDT
library(gbm)
# modeling with GBDT, use cross validation to get best trees num
gbdt<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=3000,shrinkage=0.1,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt,method="cv")
summary(gbdt,best_trees_iter)
#evaluate transactionRevenue_ln prediction RSE
predict_test2<-predict(gbdt,test_copy,best_trees_iter)
gbdt_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.083
#adjust parameters...
#attempt 2
set.seed(100)
gbdt2<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=1000,shrinkage=0.01,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt2,method="cv")
predict_test2<-predict(gbdt2,test_copy,best_trees_iter)
gbdt2_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.079324
#attempt 3
set.seed(100)
gbdt3<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.05,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt3,method="cv")
predict_test2<-predict(gbdt3,test_copy,best_trees_iter)
gbdt3_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.078397
#attempt 4
set.seed(100)
gbdt4<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.02,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt4,method="cv")
predict_test2<-predict(gbdt4,test_copy,best_trees_iter)
gbdt4_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.078758
#attempt 5
set.seed(100)
gbdt5<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.03,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt5,method="cv")
predict_test2<-predict(gbdt5,test_copy,best_trees_iter)
gbdt5_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.077228
#attempt 6
set.seed(100)
gbdt6<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.04,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt6,method="cv")
predict_test2<-predict(gbdt6,test_copy,best_trees_iter)
gbdt6_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.078652
#attempt 7
set.seed(100)
gbdt7<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.03,interaction.depth = 2,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt7,method="cv")
predict_test2<-predict(gbdt7,test_copy,best_trees_iter)
gbdt7_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.080073
#attempt 8
set.seed(100)
gbdt8<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.03,interaction.depth = 4,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt8,method="cv")
predict_test2<-predict(gbdt8,test_copy,best_trees_iter)
gbdt8_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.077747
#Result:
set.seed(100)
gbdt<-gbm(transactionRevenue_ln~.,data=train_copy,distribution="gaussian",n.trees=2000,shrinkage=0.03,interaction.depth = 3,cv.folds=5)
best_trees_iter <- gbm.perf(gbdt,method="cv")
predict_test2<-predict(gbdt,test_copy,best_trees_iter)
gbdt_mse<-get_rmse(test_copy,"transactionRevenue_ln",predict_test2)#1.077228
