% sample_HardEnsemble_SoftEnsemble
% sample routine to use hard-ensemble and soft-ensemble algorithms
%   step 1: Generate a cost-sensitive NN using over-sampling method and test
%   step 2: Generate a cost-sensitive NN using under-sampling method and test
%   step 3: Generate a cost-sensitive NN using threshold-moving method and test
%   step 4: ensemble the above 3 cost-sensitive methods using FUCTION
%               HardEnsemble
%   step 5: ensemble the above 3 cost-sensitive methods using FUCTION
%               SoftEnsemble

clear;
%------------prepare data------------

load echocardiogram
Cost=CostMatrix(2,5);
C=sum(Cost);%convert cost matrix to cost vector

%-------------step 1: Generate a cost-sensitive NN using over-sampling method and test -------------------

[newTrain,newTrainLabel]=OverSampling(Train,TrainLabel,ClassType,C);
newTrainTarget=LabelFormatConvertion(newTrainLabel,ClassType);% change to 2-value format

net=newff(minmax(newTrain),[10,NumClass],{'logsig','logsig'},'trainrp');
net.trainParam.show = 200;
net.trainParam.epochs = 200;
net=train(net,newTrain,newTrainTarget);

over_out=sim(net,Test);
over_out=normalize(over_out);% real-value outputs
over_prediction=LabelFormatConvertion(over_out,ClassType,2);%prediction

%-------------step 2: Generate a cost-sensitive NN using under-sampling method and test -------------------

[newTrain,newTrainLabel]=UnderSampling(Train,TrainLabel,ClassType,C,attribute);
newTrainTarget=LabelFormatConvertion(newTrainLabel,ClassType);% change to 2-value format

net=newff(minmax(newTrain),[10,NumClass],{'logsig','logsig'},'trainrp');
net.trainParam.show = 200;
net.trainParam.epochs = 200;
net=train(net,newTrain,newTrainTarget);

under_out=sim(net,Test);
under_out=normalize(under_out);% real-value outputs
under_prediction=LabelFormatConvertion(under_out,ClassType,2);%prediction

%-------------step 3: Generate a cost-sensitive NN using threshold-moving method and test -------------------

TrainTarget=LabelFormatConvertion(TrainLabel,ClassType);% change to 2-value format
net=newff(minmax(Train),[10,NumClass],{'logsig','logsig'},'trainrp');
net.trainParam.show = 200;
net.trainParam.epochs = 200;
net=train(net,Train,TrainTarget);

out=sim(net,Test);
out=normalize(out);
[TM_prediction,TM_out]=ThresholdMovNN(out,Cost,ClassType);

%-----------------------step 4: HardEnsemble----------------------------
component{1}=over_prediction;
component{2}=under_prediction;
component{3}=TM_prediction;
HE_prediction=HardEnsemble(component,ClassType,C);
%-----------------------step 4: SoftEnsemble----------------------------
component{1}=over_out;
component{2}=under_out;
component{3}=TM_out;
SE_prediction=SoftEnsemble(component,ClassType,C);


