
## ANN cross validation
install.packages('RSNNS')
library(RSNNS)
install.packages('Rcpp')
library(Rcpp)
install.packages('smotefamily')
library(smotefamily)


k=10
# client=read.csv('client.csv',header=TRUE)
# client=read.csv('data_no_missing.csv',header=TRUE)
client=read.csv('normalized_data.csv',header=TRUE)

## data prepocessing

n=dim(client)[1]
p=dim(client)[2]
names(client)[1]='X1'
names(client)[p]='Y'

client$Y=as.factor(client$Y)

num=round(n/k)

#### function for normalization
# input : dataset need reordered
# value_record=factor(sample(c(0,1),replace = TRUE),level=c(0,1))
value_record=rep(3,n)
value_record2=rep(3,n)
i=1

for (i in 1:k){
  if(i != k){
    a=((i-1)*num+1):(i*num)
  }else{
    a=((k-1)*num+1):n
  }
  
  train=client[-a,]
  test=client[a,]
  
  x_train=train[,-p]
  y_train=train[,p]
  x_test=test[,-p]
  y_test=test[,p]

  st=SMOTE(as.data.frame(x_train),y_train)
  # 
  smote_train=st$data
  smote_train$X2=round(smote_train$X2)
  smote_train$X3=round(smote_train$X3)
  smote_train$X4=round(smote_train$X4)
  
  target_train=decodeClassLabels(smote_train[,p])
  ## one layer

  ####### WITH SMOTE ANN
  ann_model=mlp(smote_train[,-p],target_train, size=c(7,8),learnFunc = "Std_Backpropagation",
                learnFuncParams = c(0.1,0.2),maxit=50,inputTest=x_test, linOut = TRUE,
                targetsTest = decodeClassLabels(y_test))
  
  ann_model2=mlp(smote_train[,-p],target_train, size=c(7,8),learnFunc = "Std_Backpropagation",
                learnFuncParams = c(0.1,0.2),maxit=50,inputTest=x_test, linOut = FALSE,
                targetsTest = decodeClassLabels(y_test))
  ## model 1
  pred=predict(ann_model,newdata=x_test)
  label0=which(pred[,1]>pred[,2])
  pred_value=rep(3,dim(pred)[1])
  pred_value[label0]=0
  pred_value[-label0]=1

  sum(pred_value==1)
  value_record[a]=pred_value
  
  ## model 2
  pred2=predict(ann_model2,newdata=x_test)
  label0=which(pred2[,1]>pred2[,2])
  pred_value2=rep(3,dim(pred2)[1])
  pred_value2[label0]=0
  pred_value2[-label0]=1
  
  sum(pred_value2==1)
  value_record2[a]=pred_value2
  
}

CM=table(client[,p],value_record)
TP1=CM[2,2]
TN1=CM[1,1]
FP1=CM[2,1]
FN1=CM[1,2]
prec1=TP1/(TP1+FP1)
recall1=TP1/(TP1+FN1)
F1_C1=2*prec1*recall1/(prec1+recall1)

CM2=table(client[,p],value_record2)
TP1=CM2[2,2]
TN1=CM2[1,1]
FP1=CM2[2,1]
FN1=CM2[1,2]
prec2=TP1/(TP1+FP1)
recall2=TP1/(TP1+FN1)
F1_C1_2=2*prec2*recall2/(prec2+recall2)


accuracy=(CM[2,2]+CM[1,1])/sum(CM)
accuracy2=sum(diag(CM2))/sum(CM2)

F1_C1
accuracy
1-accuracy

F1_C1_2
1-accuracy2

