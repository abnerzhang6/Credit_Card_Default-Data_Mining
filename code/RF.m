%% RF
% read "RF.csv" as new1

index = randperm(30000);

fetrs = new1(:,1:23);
lables = new1(:,24);

k = 10; % k-fold

uti_pred = zeros(1,30000);

for i = 1:k
    index_te = index((i-1)*3000+1:i*3000);
    index_tr = setxor(index,index_te);

    tr_fetrs = fetrs(index_tr,:);
    te_fetrs = fetrs(index_te,:);

    tr_lables = lables(index_tr,:);
    te_lables = lables(index_te,:);


    nTrees = 500;
    B = TreeBagger(nTrees,tr_fetrs,tr_lables,'Method','classification');
    pred = B.predict(te_fetrs);
    pred = cell2mat(pred);

    pred_n = str2num(pred);

    uti_pred(index_te) = pred_n;
end

    uti_pred = uti_pred';

    error = sum(abs(uti_pred - lables))
    error/size(lables,1)

    TP = 0;
    FP = 0;
    FN = 0;
    TN = 0;

    for i = 1:30000/k
        if pred_n(i)==1 && te_lables(i)==1
            TP = TP + 1;
        elseif pred_n(i)==0 && te_lables(i)==0
            TN = TN + 1;
        elseif pred_n(i) == 0 && te_lables(i) == 1
            FN = FN + 1;
        elseif pred_n(i) == 1 && te_lables(i) == 0
            FP = FP + 1;
        end
    end

    [TP FN;FP TN]

    % pre = TP/(TP+FP)
    precision = TP/(TP+FP);
    % rec = TP/(TP+FN)
    recall = TP/(TP+FN);

    f1 = 2*precision*recall/(precision+recall)