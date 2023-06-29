% sample_OverSampling
% sample routine to use OverSampling algorithm
%   step 1: Generate new training set using FUNCTION OverSampling
%   step 2: Train a neural network N from the new training set
%   step 3: Get cost-sensitive predictions or their real-value outputs using N

clear;
%------------prepare data------------

load echocardiogram
Cost=CostMatrix(2,5);
C=sum(Cost);%convert cost matrix to cost vector

%-------------step 1: Generate new training set using FUNCTION OverSampling -------------------

[newTrain,newTrainLabel]=OverSampling(Train,TrainLabel,ClassType,C);
newTrainTarget=LabelFormatConvertion(newTrainLabel,ClassType);% change to 2-value format

%-------------step 2: Train a neural network from the new training set-------------

net=newff(minmax(newTrain),[10,NumClass],{'logsig','logsig'},'trainrp');
net.trainParam.show = 200;
net.trainParam.epochs = 200;
net=train(net,newTrain,newTrainTarget);

%-------------step 3: Get cost-sensitive predictions or their real-value outputs using N----

cs_out=sim(net,Test);
cs_out=normalize(cs_out);% real-value outputs
cs_prediction=LabelFormatConvertion(cs_out,ClassType,2);%prediction
