function    [sample,sampleLabel]=OverSampling(data,Label,ClassType,C)
% Implement the over-sampling algorithm.
% It changes the training data distribution by duplicating
% higher-cost training examples until the appearances of different training
% examples are proportional to their costs. Here, the duplication method is
% random over-sampling with replacement
%
%Usage:
%  [sample,sampleLabel]=OverSampling(data,Label,ClassType,C)
%  
%  sample: new training set after over-sampling to build cost-sensitive NN
%               format - row indexes attributes and column indexes
%               instances                             
%  sampleLabel: class labels for instances in new training set. 
%                       format - row vector
%  data: original training set.
%           format - row indexes attributes and column indexes instances
%  Label: class labels for instances in original training set
%            format - row vector
%  ClassType: class type
%  C: cost vector. C[i] is the cost of misclassifying the i-th class
%      instance, without considering the concrete class the instance has
%      been wrongly assigned to.
%                         

%check parameters

NumClass=length(ClassType);
if(length(C)~=NumClass)
    error('class number does not consistent.')
end
if(size(data,2)~=size(Label,2))
    error('instance number in data and Label do not consistent.')
end

%compute class distribution
ClassD=zeros(1,NumClass);
for i=1:NumClass
    id=find(Label==ClassType(i));
    ClassData{i}=data(:,id);
    ClassD(i)=length(id);
end
%compute new class distribution
cn=C./ClassD;
[tmp,baseClassID]=min(cn);
newClassD=floor(C/C(baseClassID)*ClassD(baseClassID));

%sampling
sample=data;
sampleLabel=Label;
clear data Label;
i=1;
while(i<NumClass | i==NumClass )    
    if(newClassD(i)>ClassD(i))
        diff=newClassD(i)-ClassD(i);
        ids=round(rand(1,diff)*(ClassD(i)-1)+1);    % random over-sampling with replacement   
        sample=[sample ClassData{1}(:,ids)];
        sampleLabel=[sampleLabel repmat(ClassType(i),1,diff)];       
        clear ids 
    end
    if(length(ClassData)>1)
           ClassData=ClassData(2:end);
    end
    i=i+1;
end


%end

