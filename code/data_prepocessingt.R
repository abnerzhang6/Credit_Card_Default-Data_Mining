### data prepocessing
attach(client)

n=dim(client)[1]
p=dim(client)[2]
names(client)[1]='X1'
names(client)[p]='Y'

sum(X3==0)
summary(client)
1-sum(Y==1)/dim(client)[1]

## change the label of X3
X3[X3==5]=4
X3[X3==6]=4
X3[X3==0]=2
## change the label of X4
X4[X4==0]=2

# # for numerical data--> normalize it
X1=scale(X1)
X5=scale(X5)
X6=scale(X6)
X7=scale(X7)
X8=scale(X8)
X9=scale(X9)
X10=scale(X10)
X11=scale(X11)
X12=scale(X12)
X13=scale(X13)
X14=scale(X14)
X15=scale(X15)
X16=scale(X16)
X17=scale(X17)
X18=scale(X18)
X19=scale(X19)
X20=scale(X20)
X21=scale(X21)
X22=scale(X22)
X23=scale(X23)


client=data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,client$Y)
write.csv(client,'normalized_data.csv')
## save

## PCA

S=cov(client[,12:23])
E=eigen(S)$vectors
lambda=eigen(S)$values

contri=round(lambda/sum(lambda),4)

E=round(E,4)
lambda=round(lambda,4)
c1=c('principal','component','','','','','','','','','','','','contribution')
c2=c('pc1',E[,1],contri[1])
c3=c('pc2',E[,2],contri[2])
c4=c('pc3',E[,3],contri[3])
c5=c('pc4',E[,4],contri[4])

T1=cbind(c1,c2,c3,c4,c5)
T1=as.matrix(T1)
View(T1)
