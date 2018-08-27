bank<-read.csv("bank-additional-full.csv",header=T,sep=";")
set.seed(123456789)
random<-sample(1:nrow(bank))
num.bank.training<-as.integer(0.75*length(random))
bank.indices<-random[1:num.bank.training]
train<-bank[bank.indices,]
testing.indices<-random[(num.bank.training+1):length(random)]
testing.set<-bank[testing.indices,]
logis<-glm(y ~ ., data=train,family=binomial)
summary(logis)
plot(logis)
plot(logis,which=6)



boxplot(train[,1:17])

bank1<-train
bank1$dffits<-0
bank1$dffits<-dffits(logis)#peyda kardane noghati ke dar regresion tasir darand
bank2<-bank1[!bank1$dffits>2*sqrt(21/41188),]

bank1$dffits<-NULL
bank2$dffits<-NULL
bank1$out<-NULL
bank2$out<-NULL





logit<-glm(y~.,data=bank2,family=binomial)
summary(logit)
plot(logit)

plot(logit,which=4)
outlierTest(logit)

bank3<-bank2[-c(5763,6671,20075,25236,24902,36044,24092,36044,19633),]
logit.2<-glm(y~.,data=bank3,family=binomial)
summary(logit.2)
plot(logit.2,which=6)
plot(logit.2)
vif(logit.2)
library(stats)
alias(logit.2)
library(MASS)
stepAIC(logit.2,k=2)
logit.aic<-glm(formula = y ~ contact + month + day_of_week + duration + 
                 pdays + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, 
               family = binomial, data = bank3)

plot(logit.aic)
stepAIC(logit.2,k=log(length(bank3[,1])))


logit.bic<- glm(formula = y ~ contact + duration + poutcome + cons.price.idx + 
                  cons.conf.idx + euribor3m, family = binomial, data = bank3)



summary(logit.bic)
plot(logit.bic)

prediction <- data.frame(predict(logit.bic,bank3,type="response"))
prediction[prediction<0.5]=0
prediction[prediction>=0.5]=1
predictions <- data.frame(Prediction = as.numeric(prediction[,1]),Actual = as.numeric(bank3$y)-1)
predictions$Correct <- (predictions$Actual == predictions$Prediction)
logistic_accuracy<-table(predictions$Correct)/length(predictions$Correct)*100

prediction.test<-data.frame(predict(logit.bic,testing.set,type="response"))
prediction.test[prediction.test<0.5]=0
prediction.test[prediction.test>=0.5]=1
predictions.test <- data.frame(Prediction = as.numeric(prediction.test[,1]),Actual = as.numeric(testing.set$y)-1)
predictions.test$Correct <- (predictions.test$Actual == predictions.test$Prediction)
logistic_accuracy.test<-table(predictions.test$Correct)/length(predictions.test$Correct)*100
#################################################################################

library(ElemStatLearn)
library(tree)
require(rpart)
library(rpart)
tree <- rpart(y~contact + duration + poutcome + cons.price.idx + 
                cons.conf.idx + euribor3m, data=bank3, method="class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree,main = "", sub = "",cex=0.5)
printcp(tree)
plotcp(tree) 
tree.prune = prune(tree, cp = 0.0195)
fancyRpartPlot(tree.prune,main = "", sub = "",cex=0.5)

####### Make prediction#################
prediction.tree <- data.frame(predict(tree.prune, bank3, type = "class"))
predictions.tree <- data.frame(Prediction = as.numeric(prediction.tree[,1])-1,Actual = as.numeric(bank3$y)-1)
predictions.tree$Correct <- (predictions.tree$Actual == predictions.tree$Prediction)
Tree_Accuracy <- table(predictions.tree$Correct)/length(predictions.tree$Correct)*100
Tree_Accuracy

####### predict with test############
prediction.tree.test<-data.frame(predict(tree.prune,testing.set,type="class"))
predictions.tree.t <- data.frame(Prediction = as.numeric(prediction.tree.test[,1])-1,Actual = as.numeric(testing.set$y)-1)
predictions.tree.t$Correct <- (predictions.tree.t$Actual == predictions.tree.t$Prediction)
Tree_Accuracy.t <- table(predictions.tree.t$Correct)/length(predictions.tree.t$Correct)*100
Tree_Accuracy.t
######################################################################################################
library(e1071)
library(kernlab)

svm.fit = ksvm(y~ ., data = bank3, type="C-svc", kernel="rbfdot", C=10)
plot(svm.fit,data=bank3,y~duration,type=1)

summary(svm.fit)

# If we only have two classes, we can get a nice 2-dimensional plot. Try this out for a variety of kernels and kernel parameters.
two.class.data = bank3[,c(8,21)]
two.class.data= two.class.data[1:10,]
svm.fit1 = ksvm(y ~contact, data = two.class.data, type="C-svc", kernel="rbfdot", C=10)
plot(svm.fit1,two.class.data)


####### Make prediction#################
prediction.svm <- data.frame(predict(svm.fit, bank3))
predictions.svm <- data.frame(Prediction = as.numeric(prediction.svm[,1])-1,Actual = as.numeric(bank3$y)-1)
predictions.svm$Correct <- (predictions.svm$Actual == predictions.svm$Prediction)
svm_Accuracy <- table(predictions.svm$Correct)/length(predictions.svm$Correct)*100
svm_Accuracy

####### predict with test############
prediction.svm.test<-data.frame(predict(svm.fit,testing.set))
predictions.svm.t <- data.frame(Prediction = as.numeric(prediction.svm.test[,1])-1,Actual = as.numeric(testing.set$y)-1)
predictions.svm.t$Correct <- (predictions.svm.t$Actual == predictions.svm.t$Prediction)
svm_Accuracy.t <- table(predictions.svm.t$Correct)/length(predictions.svm.t$Correct)*100
svm_Accuracy.t
