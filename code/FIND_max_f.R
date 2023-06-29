## ANN set parameter

## save for smote
smote_train=as.data.frame(matrix(rep(0,40000*24)))
  
smote_data=list(s1=smote_train,s2=smote_train,s3=smote_train,s4=smote_train,
                s5=smote_train,s6=smote_train,s7=smote_train,s8=smote_train,
                s9=smote_train,s10=smote_train)
k=10
num=round(n/k) # num of group

for (i in 1:k){
  if(i != k){
    a=((i-1)*num+1):(i*num)
  }else{
    a=((k-1)*num+1):n
  }
  
  train=client[-a,]
  x_train=train[,-p]
  y_train=train[,p]
  st=SMOTE(as.data.frame(x_train),y_train)
  smote_train=st$data
  smote_train$X2=round(smote_train$X2)
  smote_train$X3=round(smote_train$X3)
  smote_train$X4=round(smote_train$X4)
  smote_data[[i]]=smote_train
}
  
  




F1_score=matrix(rep(-1,100),ncol=10)
F1_score2=matrix(rep(-1,100),ncol=10)
acc=matrix(rep(-1,100),ncol=10)
acc2=matrix(rep(-1,100),ncol=10)

for (j in 1:10 )
{
  for (m in 1:10)
  {
    value_record=rep(3,n)
    value_record2=rep(3,n)
    for (i in 1:k){
      if(i != k){
        a=((i-1)*num+1):(i*num)
      }else{
        a=((k-1)*num+1):n
      }
      test=client[a,]
      
      x_test=test[,-p]
      y_test=test[,p]
      smote_train=smote_data[[i]]
      target_train=decodeClassLabels(smote_train[,p])
      ## one layer
      
      ####### WITH SMOTE ANN
      ann_model=mlp(smote_train[,-p],target_train, size=c(j,m),learnFunc = "Std_Backpropagation",
                    learnFuncParams = c(0.1,0.2),maxit=50,linOut = TRUE,inputTest=x_test,
                    targetsTest = decodeClassLabels(y_test))
      
      ann_model2=mlp(smote_train[,-p],target_train, size=c(j,m),learnFunc = "Std_Backpropagation",
                     learnFuncParams = c(0.1,0.2),maxit=50,inputTest=x_test,
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
    
    value_record=factor(value_record,levels=c(0,1))
    value_record2=factor(value_record2,levels=c(0,1))
    
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
    
    F1_score[j,m]=F1_C1
    F1_score2[j,m]=F1_C1_2
    acc[j,m]=accuracy
    acc2[j,m]=accuracy2

  }
}
  
A=F1_score[,-1]
A[which.max(A)]

colnames(F1_score)=(seq(1:10))
rownames(F1_score)=(seq(1:10))

View(F1_score)

write.csv(F1_score,'f1_score_linear.csv')
B=F1_score2
B[which.max(B)]
write.csv(F1_score2,'f1_score_sigmoid.csv')

err1=1-acc
colnames(err1)=(seq(1:10))
rownames(err1)=(seq(1:10))
write.csv(err1,'err1.csv')

err2=1-acc2
write.csv(err2,'err2.csv')
