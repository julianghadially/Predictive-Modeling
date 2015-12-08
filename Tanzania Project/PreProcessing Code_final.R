#APM project

library(sqldf)
library(stringr)
library(doBy)
library(zoo)
library(dplyr)
library(rpart)
library(caret)
library(randomForest)
library(gbm)
library(ROCR)

x_train <- read.csv("Data/x_train.csv")
x_train$id=trimws(x_train$id)
colnames(x_train)

x_test <- read.csv("Data/x_test.csv")
y_train <- read.csv("Data/y_train.csv")
y_train$id=trimws(y_train$id)
colnames(y_train)

####X TRAIN
data_year_n0 = x_train[x_train$construction_year!=0,]
data_year_0 = x_train[x_train$construction_year==0,]


mergedf = summaryBy(construction_year ~ region + waterpoint_type, data = data_year_n0, FUN = list(mean,median))
mergedf2 = summaryBy(construction_year ~ waterpoint_type, data = data_year_n0, FUN = list(mean,median))

colnames(mergedf)[4] = "construction_year_median"
colnames(mergedf)[3] = "construction_year_mean"
colnames(mergedf2)[3] = "construction_year_median"
colnames(mergedf2)[2] = "construction_year_mean"

x_train_0 = sqldf('select A.*, construction_year_median FROM data_year_0 A LEFT JOIN mergedf B ON A.region = B.region AND A.waterpoint_type = B.waterpoint_type')
colnames(x_train_0)[41] =  "construction_year_median_NULL" 
x_train_0 = sqldf('select A.*, construction_year_median FROM x_train_0 A INNER JOIN mergedf2 B ON A.waterpoint_type = B.waterpoint_type WHERE A.construction_year_median_NULL IS NULL UNION ALL SELECT A.*, A.construction_year_median_NULL as  construction_year_median FROM x_train_0 A WHERE A.construction_year_median_NULL IS NOT NULL')

x_train_0 = (x_train_0[,-c(24,41)])
colnames(x_train_0)[40] = "construction_year"
x_train_1 = data_year_n0

x_train = rbind(x_train_0,x_train_1)

x_train$age = as.numeric(str_sub(as.character(x_train$date_recorded),-4,-1)) - as.numeric(x_train$construction_year)
x_train = x_train[,-40] # remove construction year


##############################
data_year_n0 = x_test[x_test$construction_year!=0,]
data_year_0 = x_test[x_test$construction_year==0,]

library(doBy)
mergedf = summaryBy(construction_year ~ region + waterpoint_type, data = data_year_n0, FUN = list(mean,median))
mergedf2 = summaryBy(construction_year ~ waterpoint_type, data = data_year_n0, FUN = list(mean,median))

colnames(mergedf)[4] = "construction_year_median"
colnames(mergedf)[3] = "construction_year_mean"
colnames(mergedf2)[3] = "construction_year_median"
colnames(mergedf2)[2] = "construction_year_mean"

x_test_0 = sqldf('select A.*, construction_year_median FROM data_year_0 A LEFT JOIN mergedf B ON A.region = B.region AND A.waterpoint_type = B.waterpoint_type')
colnames(x_test_0)[41] =  "construction_year_median_NULL" 
x_test_0 = sqldf('select A.*, construction_year_median FROM x_test_0 A INNER JOIN mergedf2 B ON A.waterpoint_type = B.waterpoint_type WHERE A.construction_year_median_NULL IS NULL UNION ALL SELECT A.*, A.construction_year_median_NULL as  construction_year_median FROM x_test_0 A WHERE A.construction_year_median_NULL IS NOT NULL')

x_test_0 = (x_test_0[,-c(24,41)])
colnames(x_test_0)[40] = "construction_year"
x_test_1 = data_year_n0

x_test = rbind(x_test_0,x_test_1)

x_test$age = as.numeric(str_sub(as.character(x_test$date_recorded),1,4)) - as.numeric(x_test$construction_year)
x_test = x_test[,-40] # remove construction year



#############################Adding employment rate and popdeath_ratio##########################

####TRAIN

data_employment = read.csv('tanzania employment rate.csv')
data_death_rate = read.csv('tanzania water related death rate.csv')

x_train = merge(x_train, data_employment, by = "region", all.x = TRUE)
x_train$employment_rate = as.factor(x_train$employment_rate)

x_train= merge(x_train, data_death_rate, by="region", all.x=TRUE)
x_train$popdeath_rate = as.factor(x_train$popdeath_rate)

####TEST

x_test = merge(x_test, data_employment, by = "region", all.x = TRUE)
x_test$employment_rate = as.factor(x_test$employment_rate)

x_test= merge(x_test, data_death_rate, by="region", all.x=TRUE)
x_test$popdeath_rate = as.factor(x_test$popdeath_rate)

########################################Add population related information ############### 

###Train
data_ward_pop = read.csv('ward population.csv')
data_ward_pop = data_ward_pop[data_ward_pop$status != "District",]
x_train_0 = x_train[x_train$population==0,]
x_train_1 = x_train[x_train$population!=0,]
x_train_0 = x_train_0[,-which(colnames(x_train_0)=="population")]
x_train_0 = sqldf('SELECT A.*, B.population FROM x_train_0 A LEFT JOIN data_ward_pop B ON A.lga = B.district AND A.ward = B.ward')
x_train_0[is.na(x_train_0$population)==TRUE,'population'] <- 0
x_train = rbind(x_train_1,x_train_0)

###Test
data_ward_pop = data_ward_pop[data_ward_pop$status != "District",]
x_test_0 = x_test[x_test$population==0,]
x_test_1 = x_test[x_test$population!=0,]
x_test_0 = x_test_0[,-which(colnames(x_test_0)=="population")]
x_test_0 = sqldf('SELECT A.*, B.population FROM x_test_0 A LEFT JOIN data_ward_pop B ON A.lga = B.district AND A.ward = B.ward')
x_test_0[is.na(x_test_0$population)==TRUE,'population'] <- 0
x_test = rbind(x_test_1,x_test_0)

############################################################

data_train=sqldf('Select a.*,b.status_group from x_train a join y_train b on a.id=b.id ')
colnames(data_train)
data_train_fun_repair=sqldf('select * FROM data_train where status_group= "functional needs repair" ')
str(data_train_fun_repair)
NUM_LEVELS_FUNDER = 10

#######FUNDER
funderNames <- names(summary(data_train_fun_repair$funder)[1:NUM_LEVELS_FUNDER])

#######XTRAIN
funder <- factor(data_train$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] ="Other"
levels(funder)[levels(funder)==""] <- "unknown"
data_train$funder=funder

#######XTEST
funder <- factor(x_test$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] ="Other"
levels(funder)[levels(funder)==""] <- "unknown"
x_test$funder=funder

#######installer

installerNames <- names(summary(data_train_fun_repair$installer)[1:NUM_LEVELS_FUNDER])

#######XTRAIN
installer <- factor(data_train$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] ="Other"
levels(installer)[levels(installer)==""] <- "unknown"
data_train$installer=installer

#######XTEST
installer <- factor(x_test$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] ="Other"
levels(installer)[levels(installer)==""] <- "unknown"
x_test$installer=installer

#######scheme_name

scheme <- names(summary(data_train_fun_repair$scheme_name)[1:NUM_LEVELS_FUNDER])

#######XTRAIN
scheme_name <- factor(data_train$scheme_name, levels=c(scheme, "Other"))
scheme_name[is.na(scheme_name)] ="Other"
levels(scheme_name)[levels(scheme_name)==""] <- "unknown"
data_train$scheme_name=scheme_name

#######XTEST
scheme_name <- factor(x_test$scheme_name, levels=c(scheme, "Other"))
scheme_name[is.na(scheme_name)] ="Other"
levels(scheme_name)[levels(scheme_name)==""] <- "unknown"
x_test$scheme_name=scheme_name
data_test=x_test


column_names = colnames(data_train)
factorcols = sapply(data_train, is.factor)


#####Train
for (c in column_names){
  if (factorcols[c]){
    na.fill(data_train$c,"unknown")
    }}
a=as.character(data_train$permit)
a[is.na(a)]="unknown"
data_train$permit=a
data_train$permit=as.factor(data_train$permit)

a=as.character(data_train$public_meeting)
a[is.na(a)]="unknown"
data_train$public_meeting=a
data_train$public_meeting=as.factor(data_train$public_meeting)


######Test
for (c in column_names){
  if (factorcols[c]){
    na.fill(x_test$c,"unknown")
  }}
a=as.character(x_test$permit)
a[is.na(a)]="unknown"
x_test$permit=a
x_test$permit=as.factor(x_test$permit)

a=as.character(x_test$public_meeting)
a[is.na(a)]="unknown"
x_test$public_meeting=a
x_test$public_meeting=as.factor(x_test$public_meeting)

a=sqldf("
SELECT  CASE 
        WHEN wpt_name like('%School%') OR wpt_name like('%Secondary%') OR wpt_name like('%primary%')  OR wpt_name like('%sekondari%')  OR wpt_name like('%Shul%')
        OR wpt_name like('%Teacher%') OR wpt_name like('%College%') OR wpt_name like('%ya msingi%') OR wpt_name like('%ySec%') 
        THEN 'school' 
        WHEN wpt_name like('%health%') OR wpt_name like('%Dispen%') OR wpt_name like('%hospital%')  OR wpt_name like('%clinic%')  OR wpt_name like('%Zahanati%') 
        THEN 'hospital'
        WHEN wpt_name like('%Office%') OR wpt_name like('%Ofis%')  
        THEN 'office'
        WHEN wpt_name like('%Relegious%') OR wpt_name like('%Church%') OR wpt_name like('%Mosque%')  OR wpt_name like('%Kanisa%')  OR wpt_name like('%Msikitini%') 
        THEN 'relegious_place'
        WHEN wpt_name like('%market%') OR wpt_name like('%Sokoni%') OR wpt_name like('%market%')  OR wpt_name like('%Maduka%')   
        THEN 'market'
        ELSE 'other' END AS abc
        FROM  data_train
        ")
waterpoint=as.factor(a$abc)

data_train$wpt_name=waterpoint

a=sqldf("
        SELECT  CASE 
        WHEN wpt_name like('%School%') OR wpt_name like('%Secondary%') OR wpt_name like('%primary%')  OR wpt_name like('%sekondari%')  OR wpt_name like('%Shul%')
        OR wpt_name like('%Teacher%') OR wpt_name like('%College%') OR wpt_name like('%ya msingi%') OR wpt_name like('%ySec%') 
        THEN 'school' 
        WHEN wpt_name like('%health%') OR wpt_name like('%Dispen%') OR wpt_name like('%hospital%')  OR wpt_name like('%clinic%')  OR wpt_name like('%Zahanati%') 
        THEN 'hospital'
        WHEN wpt_name like('%Office%') OR wpt_name like('%Ofis%')  
        THEN 'office'
        WHEN wpt_name like('%Relegious%') OR wpt_name like('%Church%') OR wpt_name like('%Mosque%')  OR wpt_name like('%Kanisa%')  OR wpt_name like('%Msikitini%') 
        THEN 'relegious_place'
        WHEN wpt_name like('%market%') OR wpt_name like('%Sokoni%') OR wpt_name like('%market%')  OR wpt_name like('%Maduka%')   
        THEN 'market'
        ELSE 'other' END AS abc
        FROM  x_test
        ")

waterpoint=as.factor(a$abc)
x_test$wpt_name=waterpoint
data_test=x_test

####### Adding the crime indicator

crime <- read.csv("crime.csv")
data_train=sqldf("SELECT a.*,COALESCE(b.crime_rating,0) crime_rating FROM data_train a LEFT JOIN crime b on a.region=b.Region  ")
data_train$crime_rating=as.factor(data_train$crime_rating)
table(is.na(data_train$crime_rating))

data_test=sqldf("SELECT a.*,COALESCE(b.crime_rating,0) crime_rating FROM data_test a LEFT JOIN crime b on a.region=b.Region  ")
data_test$crime_rating=as.factor(data_test$crime_rating)
table(is.na(data_test$crime_rating))
#######XTRAIN

data_train_pre <- data_train[, -which(names(data_train) == "subvillage")]
data_train_pre <- data_train_pre[, -which(names(data_train_pre) == "ward")]
data_train_pre <- data_train_pre[, -which(names(data_train_pre) == "recorded_by")]
data_train_pre <- data_train_pre[, -which(names(data_train_pre) == "id")]


#######XTEST
data_test_pre <- data_test[, -which(names(data_test) == "subvillage")]
data_test_pre <- data_test_pre[, -which(names(data_test_pre) == "ward")]
data_test_pre <- data_test_pre[, -which(names(data_test_pre) == "recorded_by")]
data_test_pre <- data_test_pre[, -which(names(data_test_pre) == "id")]


##########################sampling the training data

summary(data_train$status_group)

write.csv(data_train_pre, 'dat_train_pre.csv')

func = data_train_pre[data_train_pre$status_group == 'functional',]
non_func = data_train_pre[data_train_pre$status_group == 'non functional',]
func_needs_repair = data_train_pre[data_train_pre$status_group == 'functional needs repair',]
str(data_train_pre)


func_needs_repair = rbind(func_needs_repair,sample_n(func,2158))
data_train_sampled = rbind(func_needs_repair,sample_n(non_func,2159))
dim(data_train_sampled)
str(data_train_sampled)

#########################
set.seed(1)
#x = data_train_sampled
#xt = data_train_pre
id = sample(1:nrow(data_train_pre), 20000, replace=FALSE)
x = data_train_pre[id,]
xt = data_train_pre[-id,]

write.csv(data_test_pre,'data_test_pre.csv')

###################only with test
#levels(x$scheme_management)
#levels(data_test_pre$scheme_management)
#x$scheme_management[x$scheme_management=="None"] <- ""
#x$scheme_management <- factor(as.character(x$scheme_management))
#x$extraction_type[x$extraction_type=="other - mkulima/shinyanga"] <- ""
#x$extraction_type <- factor(as.character(x$extraction_type))
#x=x[ , -which(names(x)=='c')]
############################################


################Random Forest########################

a = array()
num_tree = c(10,20,50,100,150,200)
for (i in num_tree){
  set.seed(1)
  rf = randomForest(status_group ~ .-longitude-latitude-lga-date_recorded-extraction_type, data = x, ntree=i)
  predrf = predict(rf, data_train_pre, type = "class")
  cfrf = confusionMatrix(predrf,data_train_pre$status_group)
  a  <- append(a, 1-cfrf$overall['Accuracy'])
}


print(a)

RF100 = randomForest(status_group ~ .-longitude-latitude-lga-date_recorded-extraction_type, data = x, ntree=50)
pred100 = predict(RF100, data_train_pre, type = "class")
#pred100 = predict(RF100, test, type = "class")
#pred100 = predict(RF100, data_test_pre[,-which(names(data_test_pre) %in% c("longitude" ,"latitude" ,"lga" ,"date_recorded"))], type = "class")
confusionMatrix(pred100,data_train_pre$status_group)
RF100$importance

write.csv(pred100,'results.csv')


####################### GBM ##########################

gbmGrid = expand.grid(.interaction.depth = (1:5)*2, .n.trees = seq(2,10,2)*10, 
                      .shrinkage = c(0.01, 0.05, 0.1, 0.2),
                      .n.minobsinnode = c(1,5,10,20))
fitcontrol = trainControl(method = "cv", number =10)
#gbmFit = train(status_group ~ .-longitude-latitude-lga-date_recorded-extraction_type, data = x, method = "gbm", 
#               trControl = fitcontrol,
#               tuneGrid = gbmGrid)
#predGBM = predict(gbmFit,newdata = test)
#confusionMatrix(test$classvariable, predGBM)



#################### SVM ##########################

library(e1071)

svm.model = svm(status_group ~ .-lga-date_recorded-extraction_type, data = x, kernel = "linear", probability = TRUE)
svm.pred = predict(svm.model, xt[,-which(colnames(data_train_pre)=="status_group")], probability = TRUE, decision.values = TRUE)

table(svm.pred, xt$status_group)

# compute decision values and probabilities:
confusionMatrix(svm.pred, xt$status_group)

###rbf

wts <- 100/table(x$status_group)

svm.model = svm(status_group ~ .-lga-date_recorded-extraction_type, data = x, kernel = "radial", probability = TRUE, class.weights = wts)
svm.pred = predict(svm.model, xt[,-which(colnames(xt)=="status_group")], probability = TRUE, decision.values = TRUE)

table(svm.pred, xt$status_group)
confusionMatrix(svm.pred,xt$status_group)

d <- array()
d <- attr(svm.pred, "probabilities")[,3]

xt$twoclass = ifelse(xt$status_group == "functional needs repair", "functional needs repair", "func/non func")

svm.roc <- prediction(d,xt$twoclass)
svm.auc <- performance(svm.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
plot(svm.auc) 

cutoffs = data.frame(cut = svm.auc@alpha.values[[1]], fpr = svm.auc@x.values[[1]], tpr = svm.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))

b<- array()

thres = 0.14917

b <- ifelse(attr(svm.pred, "probabilities")[,3]>thres, "functional needs repair", ifelse(attr(svm.pred,"probabilities")[,2] > attr(svm.pred,"probabilities")[,1],"functional", "non functional"))
c <- xt$status_group

confusionMatrix(b,c)





####################### Naive Bayes ########################

nb.model = naiveBayes(status_group ~ .-lga-date_recorded-extraction_type, data = x)
nb.pred = predict(nb.model, data_train_pre[,-which(colnames(data_train_pre)=="status_group")], type = "raw")

length(nb.pred)
length(data_train_pre)

table(nb.pred, data_train_pre$status_group)
confusionMatrix(nb.pred,data_train_pre$status_group)
attr(nb.pred,"probabilities")[1:100,]

d <- array()
d <- attr(nb.pred, "probabilities")[,1]

data_train_pre$twoclass = ifelse(data_train_pre$status_group == "functional needs repair", "functional needs repair", "func/non func")

nb.roc <- prediction(d,data_train_pre$twoclass) 
nb.auc <- performance(nb.roc, 'tpr', 'fpr') 
plot(nb.auc) 

cutoffs = data.frame(cut = nb.auc@alpha.values[[1]], fpr = nb.auc@x.values[[1]], tpr = nb.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
head(subset(cutoffs, fpr < 0.1))

b<- array()

thres = 0.7196

b <- ifelse(attr(nb.pred, "probabilities")[,1]>thres, "functional needs repair", ifelse(attr(nb.pred,"probabilities")[,2] > attr(nb.pred,"probabilities")[,3],"functional", "non functional"))
c <- data_train_pre$status_group

confusionMatrix(b,c)

