% sample_ThresholdMovNN
% sample routine to use ThresholdMovNN algorithm
%   step 1: train a cost-blind  neural network N
%   step 2: test exampling using N to get real-value outputs o
%   step 3: using FUNCTION ThresholdMovNN to get cost-sensitive prediction
%               or its real-value output o*
%



clear;
%------------prepare data ------------

load echocardiogram
Cost=CostMatrix(2,5);
TrainTarget=LabelFormatConvertion(TrainLabel,ClassType);% change to 0-1 label vector format

%-------------step 1: train a cost-blind  neural network -------------------

net=newff(minmax(Train),[10,NumClass],{'logsig','logsig'},'trainrp');
net.trainParam.epochs = 200;
net=train(net,Train,TrainTarget);

%-------------step 2: test exampling using N to get real-value outputs----

out=sim(net,Test);
out=normalize(out);

%-------------step 3: using FUNCTION ThresholdMovNN to get cost-sensitive prediction
%                              or its real-value output o*

[cs_prediction,cs_out]=ThresholdMovNN(out,Cost,ClassType);
    
