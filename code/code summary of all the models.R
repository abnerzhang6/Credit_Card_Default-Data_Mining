library(class)
library(smotefamily)
library(rpart)
library(e1071)

#F1_score
F1_score=function(Y_pred,Y_true){
  CM=table(Y_true,Y_pred)
  ## F1 score for class1
  TP1=CM[2,2]
  TN1=CM[1,1]
  FP1=CM[1,2]
  FN1=CM[2,1]
  prec1=TP1/(TP1+FP1)
  recall1=TP1/(TP1+FN1)
  F1_C1=2*prec1*recall1/(prec1+recall1)
  f=F1_C1
  return(f)
}


# knn
data=read.csv("data_no_missing.csv",header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
##choose k from1:20
tree_CV=function(n,X,k)
{
  # n: n-fold
  N=dim(X)[1] # N observation
  p=dim(X)[2]
  num=round(N/n) # num of group
  pred_value=factor(rep(0,N),level=c(0,1))
  
  for (i in 1:n){
    if(i != n){
      a=((i-1)*num+1):(i*num)
    }else{
      a=((n-1)*num+1):N
    }
    train=X[-a,]
    test=X[a,]
    ### new added
    sm=SMOTE(train[,-p],train[,p])
    smote_train=sm$data
    smote_train$X2=round(smote_train$X2)
    smote_train$X3=round(smote_train$X3)
    smote_train$X4=round(smote_train$X4)
    smote_train$X5=round(smote_train$X5)
    smote_train$X6=round(smote_train$X6)
    smote_train$X7=round(smote_train$X7)
    smote_train$X8=round(smote_train$X8)
    smote_train$X9=round(smote_train$X9)
    smote_train$X10=round(smote_train$X10)
    smote_train$X11=round(smote_train$X11)
    ## smote_train is new training data
    tree_model=knn(smote_train[,-p],test[,-p],smote_train[,p],k)
    pred_value[a]=tree_model
  }
  return(pred_value)
}
m=20
Y_true=newdata[,13]
for (k in 1:m){
  Y_pred=tree_CV(10,newdata,k)
  test.error[k]=sum(Y_pred!=Y_true)/len
  F1[k]=F1_score(Y_pred,Y_true)
}
kerror=mean(test.error)
F1
which.max(F1)
##KNN--k=20
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
Y_true=newdata[,13]
tree_model=knn(smote_train[,1:12],test[,1:12],smote_train[,13],20)

Y_pred[a]=tree_model
}
Y_pred=Y_pred-1
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


#decision tree 
data=read.csv(file.choose(),header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
Y_true=newdata[,13]
cvD=rpart(class~.,data=train,method='class')
predD=predict(cvD,newdata=test,type='class')
Y_pred[a]=predD
}
Y_pred=Y_pred-1

F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


#naive bayes
data=read.csv(file.choose(),header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
Y_true=newdata[,13]
cvN=naiveBayes(class~.,data=trainN,method='class')
predN=predict(cvN,newdata=test,type='class')
Y_pred[a]=predN
}
Y_pred
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


#bagging---knn+naive+decision tree+svm
data=read.csv(file.choose(),header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
d=dim(newdata)
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
pred=vector()
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
aN<-sample(ds, ds, replace = TRUE, prob = NULL)
aK<-sample(ds, ds, replace = TRUE, prob = NULL)
aD<-sample(ds, ds, replace = TRUE, prob = NULL)
aS<-sample(ds, ds, replace = TRUE, prob = NULL)
trainS<-smote_train[aS,]
trainN<-smote_train[aN,]
trainK<-smote_train[aK,]
trainD<-smote_train[aD,]
trainS[,13]=as.factor(trainS[,13])
Y_true=newdata[,13]
cvK=knn(trainK[,1:12],test[,1:12],trainK[,13],3)
cvN=naiveBayes(class~.,data=trainN,method='class')
predN=predict(cvN,newdata=test,type='class')
cvD=rpart(class~.,data=trainD,method='class')
predD=predict(cvD,newdata=test,type='class')
cvS=svm(class~.,data=trainS,kernel="poly")
predS=predict(cvS,test)
cvK=as.numeric(cvK)
cvK=cvK-1
predN=as.numeric(predN)
predN=predN-1
predD=as.numeric(predD)
predD=predD-1
predS=as.numeric(predS)
predS=predS-1
pred=(cvK+predN+predD+predS)>2
Y_pred[a]=pred+0
}
Y_pred
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


##bagging---SVM+naive+desicion tree
data=read.csv(file.choose(),header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
d=dim(newdata)
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
aN<-sample(ds, ds, replace = TRUE, prob = NULL)
aD<-sample(ds, ds, replace = TRUE, prob = NULL)
aS<-sample(ds, ds, replace = TRUE, prob = NULL)
trainS<-smote_train[aS,]
trainN<-smote_train[aN,]
trainD<-smote_train[aD,]
trainS[,13]=as.factor(trainS[,13])
Y_true=newdata[,13]
cvN=naiveBayes(class~.,data=trainN,method='class')
predN=predict(cvN,newdata=test,type='class')
cvD=rpart(class~.,data=trainD,method='class')
predD=predict(cvD,newdata=test,type='class')
cvS=svm(class~.,data=trainS,kernel="poly")
predS=predict(cvS,test)
predN=as.numeric(predN)
predN=predN-1
predD=as.numeric(predD)
predD=predD-1
predS=as.numeric(predS)
predS=predS-1
pred=(predN+predD+predS)>1
Y_pred[a]=pred+0
}
Y_pred
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


##bagging--knn+desicion tree+naive
data=read.csv(file.choose(),header=T)
odata=data
d=dim(odata)
len=d[1]
n=10
num<-d[1]%/%n
test.error<-vector()
f1<-vector()
F1<-vector()
kerror<-vector()
Y_pred<-vector()
##PCA
A=odata[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y","class")
d=dim(newdata)
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-13],train[,13])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
aN<-sample(ds, ds, replace = TRUE, prob = NULL)
aK<-sample(ds, ds, replace = TRUE, prob = NULL)
aD<-sample(ds, ds, replace = TRUE, prob = NULL)
trainN<-smote_train[aN,]
trainK<-smote_train[aK,]
trainD<-smote_train[aD,]
Y_true=newdata[,13]
cvK=knn(trainK[,1:12],test[,1:12],trainK[,13],3)
cvN=naiveBayes(class~.,data=trainN,method='class')
predN=predict(cvN,newdata=test,type='class')
cvD=rpart(class~.,data=trainD,method='class')
predD=predict(cvD,newdata=test,type='class')
cvK=as.numeric(cvK)
cvK=cvK-1
predN=as.numeric(predN)
predN=predN-1
predD=as.numeric(predD)
predD=predD-1
pred=(cvK+predN+predD)>1
Y_pred[a]=pred+0
}
Y_pred=Y_pred-1
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


#SVM with normalization data
normal=read.csv("normalized_data.csv",header=T)
A=normal[,12:23]
sigma=cov(A)
E=eigen(sigma)$vectors
lam=eigen(sigma)$values
con=cumsum(lam)/sum(lam)
con
A=odata[,12:23]
A=as.matrix(A)
E=as.matrix(E)
Y=A%*%E[,1:6]
newdata=cbind(odata[,1:11],Y,odata[,24])
colnames(newdata) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","Y1","Y2","Y3","Y4","Y5","Y6","class")
n=10
for (i in (1: n))
{if (i!=n) 
  a<-((i-1)*num+1): (i*num)
if (i==n) 
  a<-((i-1)*num+1):d[1]
train=newdata[-a,]
test=newdata[a,]
sm=SMOTE(train[,-18],train[,18])
smote_train=sm$data
smote_train$X2=round(smote_train$X2)
smote_train$X3=round(smote_train$X3)
smote_train$X4=round(smote_train$X4)
smote_train$X5=round(smote_train$X5)
smote_train$X6=round(smote_train$X6)
smote_train$X7=round(smote_train$X7)
smote_train$X8=round(smote_train$X8)
smote_train$X9=round(smote_train$X9)
smote_train$X10=round(smote_train$X10)
smote_train$X11=round(smote_train$X11)
ds=dim(smote_train)[1]
Y_true=newdata[,18]
smote_train[,18]=as.factor((smote_train)[,18])
tree_model=svm(class~.,data=smote_train,kernel="poly")
pred=predict(tree_model,test)
Y_pred[a]=pred
}
Y_pred=Y_pred+0
F1_score(Y_pred,Y_true)
error=1-sum(Y_pred==Y_true)/d[1]
error


#LightGBM (all following codes are it)
install.packages("lightgbm")
library(smotefamily)
library(caret)
library(lightgbm) 

X <- read.csv(file="data.csv",header=TRUE)
X$client.Y=as.numeric(X$client.Y)


acc=rep(0,10)
F1=rep(0,10)


F1_score=function(Y_pred,Y_true){
  CM=table(Y_true,Y_pred)
  ## F1 score for class 0
  
  TP1=CM[2,2]
  TN1=CM[1,1]
  FP1=CM[2,1]
  FN1=CM[1,2]
  prec1=TP1/(TP1+FP1)
  recall1=TP1/(TP1+FN1)
  F1_C1=2*prec1*recall1/(prec1+recall1)
  f=F1_C1
  return(f)
}
####################
# 1
p=24
k=10
N=dim(X)[1] # N observation
num=round(N/k)
i=1
a=((i-1)*num+1):(i*num)
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.03,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.334249, 0,1) #picking the correct threshold. 0.336 0.334249
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[1]=0.7607         
F1[1]=F1_score(pred_y,test_y)
F1[1] #0.7673  0.4852507
################
#2
i=2
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.01,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.334254, 0,1) #picking the correct threshold.0.338 0.3362 0.334254
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[2]=0.8007 
F1[2]=F1_score(pred_y,test_y)
F1[2]

################
# 3
i=3
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.01,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.332127, 0,1) #picking the correct threshold. 0.342 0.33873 0.3373764 0.337096 0.33240750 0.332353 0.332127
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[3]=0.7377            
F1[3]=F1_score(pred_y,test_y)
F1[3]

################
# 4
i=4
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 23,
  learning_rate = 0.03,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=15
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.33408346, 0,1) #picking the correct threshold. 0.34 0.3369252
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[4]=0.7903          
F1[4]=F1_score(pred_y,test_y)
F1[4]

################
# 5
i=5
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.03,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.334017752, 0,1) #picking the correct threshold. 0.34 0.334368
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[5]=0.775     
F1[5]=F1_score(pred_y,test_y)
F1[5]

################
# 6
i=6
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.03,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 1,reshape=T)
pred_y=ifelse(pred < 0.336, 0,1) #picking the correct threshold. 0.336
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[6]=0.817   
F1[6]=F1_score(pred_y,test_y)
F1[6]


################
# 7
i=7
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.01,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 10,reshape=T)
pred_y=ifelse(pred < 0.3445653304, 0,1) #picking the correct threshold. 0.3522
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[7]=0.827 
F1[7]=F1_score(pred_y,test_y)
F1[7]



################
# 8
i=8
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.01,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=10
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 10,reshape=T)
pred_y=ifelse(pred < 0.342900, 0,1) #picking the correct threshold. 0.3522 0.34618632 0.3460087
confusionMatrix(as.factor(test_y), as.factor(pred_y)) 
acc[8]=0.8377
F1[8]=F1_score(pred_y,test_y)
F1[8]


################
# 9
i=9
a=((i-1)*num+1):(i*num)
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)

# train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.005,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=15
)

# validataion data
valids = list(test = dtest)

# train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids,
)

# prediction
pred=predict(model, as.matrix(test_x), num_iteration = 10,reshape=T)
pred_y=ifelse(pred < 0.3373, 0,1) #picking the correct threshold.0.3386
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[9]=0.833 
F1[9]=F1_score(pred_y,test_y)
F1[9]

################
##10
i=10
a=(i-1)*num+1:N
p=dim(X)[2]
train_x=X[-a,-p]
train_y=X[-a,p]
test_x=X[a,-p]
test_y=X[a,p]

dtrain = lgb.Dataset(as.matrix(train_x), label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, data = as.matrix(test_x), label = test_y)
##train model 
valids = c(train = dtrain)
train_params <- list(
  num_leaves = 20,
  learning_rate = 0.01,
  objective = "binary", 
  metric = c("binary_logloss", "binary_error"),
  is_unbalance = F,
  boosting="dart",
  bagging_freq=15)
##validataion data
valids = list(test = dtest)
##train model 
model <- lgb.train(
  data = dtrain,
  params = train_params, 
  valids = valids)
##prediction
pred=predict(model, as.matrix(test_x), num_iteration = 10,reshape=T)
pred_y=ifelse(pred < 0.335, 0,1) #picking the correct threshold. 0.3490
confusionMatrix(as.factor(test_y), as.factor(pred_y))
acc[10]=0.779              
F1[10]=F1_score(pred_y,test_y)
F1[10]
##final
mean(acc)   # 0.82079
mean(F1)

