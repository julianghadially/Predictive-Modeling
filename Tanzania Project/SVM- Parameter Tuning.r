#################### SVM ##########################

## input  x = train sample (data_train_pre)
## input xt = train validation (data_train_test)
## also run preprocessing

###################################################

library(e1071)

svm.model = svm(status_group ~ .-lga-date_recorded-extraction_type, data = x, kernel = "linear", probability = TRUE)
svm.pred = predict(svm.model, xt[,-which(colnames(data_train_pre)=="status_group")], probability = TRUE, decision.values = TRUE)

table(svm.pred, xt$status_group)

# compute decision values and probabilities:
confusionMatrix(svm.pred, xt$status_group)

###rbf

#wts <- 100/table(x$status_group)

slack <- c(1,10,100,1000)
j <- data.frame(slack = numeric(4), f1 = double(4))
f1 <- array()
k = 1

for (i in slack){
  svm.model = svm(status_group ~ .-lga-date_recorded-extraction_type, data = x, kernel = "radial", probability = TRUE, cost=slack)
  svm.pred = predict(svm.model, xt[,-which(colnames(xt)=="status_group")], probability = TRUE, decision.values = TRUE)
  
  
  table(svm.pred, xt$status_group)
  d <- array()
  d <- attr(svm.pred, "probabilities")[,3]
  
  xt$twoclass = ifelse(xt$status_group == "functional needs repair", "functional needs repair", "func/non func")
  
  svm.roc <- prediction(d,xt$twoclass)
  svm.auc <- performance(svm.roc, 'tpr', 'fpr', cost.fp = 100, cost.fn = 1) 
  plot(svm.auc) 
  
  cutoffs = data.frame(cut = svm.auc@alpha.values[[1]], fpr = svm.auc@x.values[[1]], tpr = svm.auc@y.values[[1]])
  cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE),]
  confusionMatrix(svm.pred,xt$status_group)
  
  thres = as.numeric(head(subset(cutoffs, fpr < 0.1), n = 1)[1])
  #thres = 0.1456
  
  b<- array()
  
  b <- ifelse(attr(svm.pred, "probabilities")[,3]>thres, "functional needs repair", ifelse(attr(svm.pred,"probabilities")[,2] > attr(svm.pred,"probabilities")[,1],"functional", "non functional"))
  
  xt$status_group = as.factor(xt$status_group)
  b <- as.factor(b)
  
  c <- xt$status_group
  
  confusionMatrix(b,c)
  
  library(MLmetrics)
  
  b1 <- ifelse(b == "functional", 1, ifelse(b == "functional needs repair", 2, 3))
  c1 <- ifelse(c == "functional", 1, ifelse(c == "functional needs repair", 2, 3)) 
  
  f1[k] <- F1_Score(y_true = b1, y_pred = c1,positive = "2") 
  k = k+1
}

j$slack = slack
j$f1 = f1
j