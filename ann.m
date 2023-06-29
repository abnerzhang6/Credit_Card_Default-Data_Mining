newy_train = zeros(29999,2);
for i = 1:30000
    if lable(i) == 0
        newy_train(i,1) = 1;
    else
        newy_train(i,2) = 1;
    end
end
newy_train


TP = 3873;FP = 2174;
FN = 1072;TN = 5863;

% pre = TP/(TP+FP)
precision = TP/(TP+FP);
% rec = TP/(TP+FN)
recall = TP/(TP+FN);

f1_1 = 2*precision*recall/(precision+recall)

% pre = TP/(TP+FP)
precision = TN/(TN+FN);
% rec = TP/(TP+FN)
recall = TN/(TN+FP);

f1_2 = 2*precision*recall/(precision+recall)

f1 = (f1_1+f1_2)/2


%%

features = new(:,1:23);
lables = new(:,24);

idx = randperm(30000);
trainl = 30000*0.7;
testl = 30000-trainl;
X_train = features(idx(1:trainl),:);
y_train = lables(idx(1:trainl),:);  
X_test = features(idx(trainl+1:30000),:);
y_test = lables(idx(trainl+1:30000),:); 

newy_train = zeros(21000,2);
for i = 1:21000
    if y_train(i) == 0
        newy_train(i,1) = 1;
    else
        newy_train(i,2) = 1;
    end
end
newy_train

newy_test = zeros(9000,2);
for i = 1:9000
    if y_test(i) == 0
        newy_test(i,1) = 1;
    else
        newy_test(i,2) = 1;
    end
end
newy_test


maxEpochs = 1000;
miniBatchSize = 1;

layers = [
    featureInputLayer(23,"Name","featureinput")
    fullyConnectedLayer(2,"Name","fc")
    softmaxLayer("Name","softmax")
    classificationLayer("Name","classoutput")];

options = trainingOptions('adam',...
    'ExecutionEnvironment','cpu',...
    'GradientThreshold',1,...
    'MaxEpochs',maxEpochs,...
    'MiniBatchSize',miniBatchSize,...
    'SequenceLength','longest',...
    'Shuffle','once',...
    'Verbose',0,...
    'Plots','training-progress');

trainedNet = trainNetwork(X_train,newy_train,layers,options);














