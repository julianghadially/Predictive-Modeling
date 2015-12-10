library(ggplot2)
library(tree)
library(sampling)
library(dplyr)
library(e1071)
library(nlme)
library(lme4)
library(dummy)
library(glmnet)
library(ROCR)
library(caret)
library(sqldf)

#Load Functions written for project
source('MyScriptsJG.R')



#===========================================================================================


#                       LOADING OF PREPROCESSED DATA + ADDITIONAL CLEANING AND MANIPULATION


#===========================================================================================


#import Clean Data Files into two data sets. One for multi-class analysis, and one for
#binary analysis. Water2.train is for multiclas. water01.train is for binary.

water2.train = read.csv('Data/dat_train_pre.csv')
water01.train = read.csv('Data/dat_train_pre.csv')
attach(water2.train)
n = length(water2.train$status_group)
n01 = length(water01.train$status_group)

####Further cleaning and manipulation of Data###


water2.train$crime_rating = ordered(water2.train$crime_rating)
water2.train$region_code = as.factor(water2.train$region_code)

water01.train$crime_rating = ordered(water01.train$crime_rating)
water01.train$region_code = as.factor(water01.train$region_code)

#dummied.extraction_type_group = dummy(water.train[,c(1,21)])

#create training, holdout, and test set for multiclass data
   #train.train is train set. train.test is holdout. water2.test is test set.
   #We have two "test" sets. water2.test has the response variable present. The test set
   #provided by driven data does not have a response variable that we can download.
set.seed(1)
train.train.index = sample(1:n, n/2)
water2.train.train = water2.train[train.train.index,]
holdout.index = sample(train.train.index, n/4)
water2.train.test = water2.train[holdout.index,]
water2.test = water2.train[-holdout.index,]



#func_needs_repair = water.train[water.train$status_group == "functional needs repair",]
#func = water.train[water.train$status_group== "functional",]
#non_func = water.train[water.train$status_group== "non functional",]
#func_sample = sample_n(func, 2158)
#non_func_sample = sample_n(non_func, 2159)
#water.train.reduced = rbind(func_sample, non_func_sample,func_needs_repair)


# Convert water01.train status group to binary with 'functional needs repair' as positive class. 
water01.train$status_group = as.character(water01.train$status_group)
for (i in 1:length(water01.train$status_group)){
  if (water01.train$status_group[i] == 'functional needs repair'){
    water01.train$status_group[i] = 1
  } else {
    water01.train$status_group[i] = 0
  }
}
water01.train$status_group = as.numeric(water01.train$status_group)


#Dummy Coding for MLM: in order to test different levels for MLM. Still failed to converge for a large
  #number of levels. Couldn't find any apparent pattern to failure.
  #dummy coding for mlm on extraction type and scheme name. Did not end up using Scheme name dummy categories for MLM (see discussion of MLM in results section of the paper)
  #dummied.extraction_type = dummy(water01.train[,c(1,21)])
  #water01.train$extraction_type_cemo = dummied.extraction_type$extraction_type_cemo
  #dummied.scheme_name = dummy(water01.train[,c(1,19)])
  #water01.train$scheme_name_Mkongoro_Two = dummied.scheme_name$scheme_name_Mkongoro.Two
  #water01.train = cbind(water01.train, dummied.extraction_type)

#Dummy coding for PCA before MLM: Trying out pca in place of above dummy method.
dummied = dummy(water01.train[,c(-1,-3,-4,-6,-8,-9,-16,-22,-25,-29,-31,-36,-37,-38,-41)])
#water01.dummied.train = cbind(water01.train[,c()], dummied)

#train01.test split
set.seed(1)
train.train.index = sample(1:n01, n01/2)
water01.train.train = water01.train[train.train.index,]
holdout.index = sample(train.train.index, n01/4)
test.index = -holdout.index
water01.train.test = water01.train[holdout.index,]
water01.test = water01.train[test.index,]


# 1 VS ALL DOWNSAMPLING on water01.train
   #using 20% positive class size, which was tuned from the Logistic Lasso.

func_needs_repair = water01.train.train[water01.train.train$status_group == 1,]
func_and_non_func = water01.train.train[water01.train.train$status_group== 0,]
func_and_non_func_sample = sample_n(func_and_non_func, 2124/.5)
water01.train.train.reduced = rbind(func_and_non_func_sample,func_needs_repair)

# MULTI-CLASS DOWNSAMPLING on water2.train

downsample_size = c(0.0770235, .1, .2,.33) #vector (formerly) used for tuning. 
d = downsample_size[3] #selecting the optimal downsample size found from tuning.
set.seed(1)
n_func_nonfunc = table(water2.train.train$status_group)[2]*(1/d)-table(water2.train.train$status_group)[2]
func_needs_repair = water2.train.train[water2.train.train$status_group == "functional needs repair",]
func = water2.train.train[water2.train.train$status_group== "functional",]
non_func = water2.train.train[water2.train.train$status_group== "non functional",]
func_sample = sample_n(func, n_func_nonfunc/2)
non_func_sample = sample_n(non_func, n_func_nonfunc/2)
water2.train.train.reduced = rbind(func_sample, non_func_sample,func_needs_repair)




#===========================================================================================
#===========================================================================================


#                                 MODELING


#===========================================================================================
#===========================================================================================

#baselineSVM. See SVM code for final SVM.
#model = svm(status_group~., data = water2.train.reduced)
#yhat = predict(model, water2.train.reduced[,-39])
#table(yhat, water2.train.reduced$status_group)

#LASSO LOGISTIC REGRESSION
#normalize numeric variables (Normalization of these three variables resulted in 
   #a smaller f1 score in the hold-out set. Thus, our final model uses non-normalized age 
   #and population, despite knowledge that lasso requires scaled variables.
water2.train.train.reduced$age = (water2.train.train.reduced$age - mean(water2.train.train.reduced$age))/sd(water2.train.train.reduced$age)
water2.train.train.reduced$population = (water2.train.train.reduced$population - mean(water2.train.train.reduced$population))/sd(water2.train.train.reduced$population)

#water2.train.train.reduced$amount_tsh = (log(water2.train.train.reduced$amount_tsh) - mean(log(water2.train.train.reduced$amount_tsh)))/sd(log(water2.train.train.reduced$amount_tsh))
water2.train.test$age = (water2.train.test$age - mean(water2.train.train.reduced$age))/sd(water2.train.train.reduced$age)
water2.train.test$population = (water2.train.test$population - mean(water2.train.train.reduced$population))/sd(water2.train.train.reduced$population)
#water2.train.test$amount_tsh = (water2.train.test$amount_tsh - mean(water2.train.train.reduced$amount_tsh))/sd(water2.train.train.reduced$amount_tsh)

summary(water2.train.train.reduced$age)
summary(water2.train.train.reduced$population)
summary(water2.train.train.reduced$amount_tsh)
summary(water2.train.test$age)
summary(water2.train.test$population)
summary(water2.train.test$amount_tsh)

#Create Design Matrices with downsampling
train.x = model.matrix(~0+.,water2.train.train.reduced[,c(-3,-4,-16,-37,-40)])
train.y = as.matrix(water2.train.train.reduced[,40])
train.test.x = model.matrix(~0+.,water2.train.test[,c(-3,-4,-16,-37,-40)])
train.test.y = as.matrix(water2.train.test$status_group)
test.x = model.matrix(~0+.,water2.test[,c(-3,-4,-16,-37,-40)])
test.y = as.matrix(water2.test$status_group)

#Run Logistic Lasso Model and generate probabilities:
logistic.lasso = glmnet(train.x,train.y, alpha = 1, family = 'multinomial')
   #plot(logistic.lasso, xvar = 'lambda', main = 'Regularized Logistic Regression')
   #cv = cv.glmnet(train.x, train.y, family = 'multinomial', alpha = 0, nfolds = 5)
   #print(paste('Choose Lambda = ', cv$lambda.1se, 'for Standardized Data'))
   #lambda.idx = which(logistic.lasso$Lambda== cv$lambda.1se)
yhat = predict(logistic.lasso, newx = train.test.x, s = logistic.lasso$lambda[50], type = 'response')
yhat.prob = yhat[,,1]

#ROC
yhat.binary.prob = yhat.prob[,2]
train.test.binary.y = ifelse(train.test.y== 'functional needs repair', 1, 0)
glmnet.roc <- prediction(yhat.binary.prob, train.test.binary.y) 
glmnet.auc <- performance(glmnet.roc, 'tpr', 'fpr') 
plot(glmnet.auc, col = 'blue', main='ROC: Logistic Lasso for different thresholds') 


#obtain threshold using f1 score
glmnet.roc <- prediction(yhat.binary.prob, train.test.binary.y) 
glmnet.auc.f1 <- performance(glmnet.roc, 'f') 
cutoffs = data.frame(cut = glmnet.auc.f1@x.values[[1]], f1 = glmnet.auc.f1@y.values[[1]])
head(cutoffs,5)
cutoffs = cutoffs[order(cutoffs$f1, decreasing = TRUE),]
glmnet.threshold.f1 = cutoffs[1,1]
   #write.csv(cutoffs, file = 'Lasso Logistic ROC.csv')


#obtain threshold using tpr and fpr. We abandoned this method.
cutoffs = data.frame(cut = glmnet.auc@alpha.values[[1]], fpr = glmnet.auc@x.values[[1]], tpr = glmnet.auc@y.values[[1]])
cutoffs = cutoffs[order(cutoffs$cut, decreasing = TRUE),]
threshold.point = subset(cutoffs, cut < glmnet.threshold.f1)[1,]
   #write.csv(cutoffs, file = 'Lasso Logistic ROC')
plot(glmnet.auc, col = 'blue', main='ROC: Logistic Lasso for different thresholds') 
points(threshold.point[,c(2,3)], col = 'red', lwd = 3)

#apply Threshold to get test confusion matrix, f1 score, and other metrics.
yhat = predict(logistic.lasso, newx = test.x, s = logistic.lasso$lambda[50], type = 'response')
yhat.prob = yhat[,,1]
yhat.resp = to.response(yhat.prob, glmnet.threshold.f1, n_class = 3)
confusion_matrix = table(yhat.resp, test.y)
confusionMatrix(yhat.resp,test.y)
precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])
f1 = (2*precision*recall)/(precision + recall)
print(paste("F1 score is ", f1))

#Results of Downsample proportion tuning:
#no downsampling: .375132
#10%: .37583
#20%:.37892
#33%: .378637
#plot of downample proportion tuning:
tuning_f1_scores = c(.375132,.37583,.37892,.378637)
plot(downsample_size, tuning_f1_scores, type = 'o', main = 'Optimal Downsampling Level', ylab = 'F1 Score', xlab = 'Positive Class Proportion', col = 'blue')


#Tune and select appropriate lambda by f1 score: #This block has been appended 
   #to the end of the lasso logistic model to easily allow for omission when running 
   #line by line 
logistic.lasso = glmnet(train.x,train.y, alpha = 1, family = 'multinomial')
lambdas = logistic.lasso$lambda[c(1,10,20,30,40,50,60,70,80,90,99)]
f1_scores = NULL
for (lambda in lambdas) {
  yhat = predict(logistic.lasso, newx = train.test.x, s = lambda, type = 'response')
  yhat.prob = yhat[,,1]
  yhat.resp = to.response(yhat.prob, glmnet.threshold.f1, n_class = 3)
  yhat.binary.resp = ifelse(yhat.resp == 'functional needs repair', 1, 0)
  glmnet.roc <- prediction(yhat.binary.resp, train.test.binary.y) 
  glmnet.auc <- performance(glmnet.roc, 'f') 
  #add the f1 scores to f1_scores
  f1_scores = c(f1_scores, glmnet.auc@y.values[[1]][2])
}
plot(log10(lambdas), f1_scores, type = 'o', main = 'Tuning Lambda in Lasso Logistic Regression', 
     xlab = 'Log Lambda', ylab = 'F1 Score in Hold-out Set', col = 'blue', lwd = 2)

#select appropriate lambda by accuracy. We abandoned this method because we care about f1 not
   #accuracty:
   #logistic.lasso = glmnet(train.x,train.y, alpha = 1)
   #lambdas = logistic.lasso$lambda[c(1,10,20,30,40,50,60,70,80,90,99)]
   #accuracy_scores = NULL
   #for (lambda in lambdas) {
   #  yhat = predict(logistic.lasso, newx = train.test.x, s = lambda, type = 'response')
   #  yhat.prob = yhat[,,1]
   #  yhat.resp = to.response(yhat.prob, glmnet.threshold, n_class = 3)
   #  yhat.binary.resp = ifelse(yhat.resp == 'functional needs repair', 1, 0)
   #  accuracy_scores = c(accuracy_scores, accuracy_score(yhat.binary.resp, train.test.binary.y))
   #}
   #plot(log(lambdas), accuracy_scores, type = 'l')


#LASSO feature reduction
logistic.lasso = glmnet(train01.x[,-(24:378)],train01.y, alpha = 1, lambda = .0013)
features = logistic.lasso$beta
df_features = data.frame('feature' = features@Dimnames[[1]][features@i], 'value' = abs(features@x))
sorted_features = sqldf('select * from df_features order by value DESC')

reduced.features = features@Dimnames[[1]][features@i]
reduced.features.design = reduced.features[1]
for(feature in reduced.features[2:length(reduced.features)]) {
  reduced.features.design = paste(reduced.features.design,feature, sep = '+')
}
reduced.features.design = paste('status_group', reduced.features.design, sep = '~')




#===========================================================================================

  # MULTI LEVEL MODELS

#===========================================================================================

#Note: this section is a bit messier than the rest because of several complications
#(-3,-4,-16,-37,-40)

#ALSO NOTE THAT THE MLM SECTION HAS A FEW BROKEN COMPONENTS

train01.y = as.matrix(water01.train.train.reduced[,40])
train01.x = model.matrix(~.,water01.train.train.reduced[,c(-4,-40)])
train01 = model.matrix(status_group~.,water01.train.train.reduced)
train01.test.x = model.matrix(~0+.,water01.train.test[,c(-4,-40)])
     #train01.test.y = as.matrix(water2.train.test$status_group)

#pr.obj = prcomp(~., data = dummied[,1:5], center = TRUE, scale = TRUE)


#scale and normalize
water01.train.train.reduced$age = (water01.train.train.reduced$age - mean(water01.train.train.reduced$age))/sd(water01.train.train.reduced$age)
water01.train.train.reduced$population = (water01.train.train.reduced$population - mean(water01.train.train.reduced$population))/sd(water01.train.train.reduced$population)
water01.train.test$age = (water01.train.test$age - mean(water01.train.train.reduced$age))/sd(water01.train.train.reduced$age)
water01.train.test$population = (water01.train.test$population - mean(water01.train.train.reduced$population))/sd(water01.train.train.reduced$population)
water01.train.train.reduced$extraction_type <- water01.train.train.reduced$extraction_type





#mlm.obj2 <- lme(status_group~source_type+scheme_name, data = water.train.train.reduced, random = ~ extraction_type | region_code, method = 'REML')
mlm.obj <- lmer(status_group~source_type+quantity_group+funder+scheme_name+region_code 
                +installer + (extraction_type_windmill | region_code)
                + (extraction_type_cemo | region_code)
                + (extraction_type_climax | region_code)
                + (extraction_type_mono | region_code), data = water01.train.train.reduced, REML = TRUE)





a = factor(water01.train.train.reduced$extraction_type)
levels(a)
water01.train.train.reduced$extraction_type = a

#mlm.obj <- lmer(status_group~ age+population + (extraction_type | region_code), data = water01.train.train.reduced, REML = FALSE)

mlm.v2 <- lmer(status_group~source_type+quantity_group+funder+scheme_name+region_code +installer +(extraction_type_cemo | region_code) + (scheme_name_Mkongoro_Two | region_code), data = water01.train.train.reduced, REML = TRUE)

#  
#glmer.obj <- glmer(status_group~crime_rating + popdeath_rate + employment_rate+
#                     source_class+quantity_group+ age + (1 | region_code), data = 
#                     water01.train.train.reduced, family = multinomial, REML = TRUE)


yhat = predict(mlm.obj, newdata=water01.train.test[,-40])

#plot roc
mlm.roc <- prediction(yhat, water01.train.test$status_group) 
mlm.auc <- performance(mlm.roc, 'tpr', 'fpr') 
plot(mlm.auc, col = 'blue', main='ROC: MLM at Varying Thresholds') 

#get threshold
cutoffs = data.frame(cut = mlm.auc@alpha.values[[1]], fpr = mlm.auc@x.values[[1]], tpr = mlm.auc@y.values[[1]])
head(cutoffs,5)
cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
mlm.threshold = subset(cutoffs, fpr < 0.1)[1,1]  #thres = 0.70917


yhat.resp = to.response(yhat, mlm.threshold)
table(water01.train.test$status_group, yhat.resp)
confusionMatrix(yhat.resp, water01.train.test$status_group, positive = '1')
print(890/2746)


?lmer
