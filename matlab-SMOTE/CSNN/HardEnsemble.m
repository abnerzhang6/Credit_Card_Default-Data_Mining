function    prediction=HardEnsemble(component,ClassType,C)
% Implement hard-ensemble method
% In this method, every component learner votes (0-1 vote method)
% for a class and then the class receiving the biggest number of votes 
% is returned. If a tie appears, that is, there are multiple classes 
% receiving the biggest number of votes, then the class with the
% biggest cost is returned.
%
%Usage
%  prediction=HardEnsemble(component,ClassType,C)
%
%  prediction: predictions of the hard-ensemble
%  component: cell array which contains at least 3 component learner's
%                     prediction vectors of test instances. 
%  ClassType: class type.
%  C: cost vector. C[i] is the cost of misclassifying ith class
%      instance, without considering the concrete class the instance has
%      been wrongly assigned to.

if(nargin<3)
    help HardEnsemble
end
Nc=length(component);
if(Nc<3)
    error('not enough components.')
end
N=length(component{1});
for i=2:Nc
        if(size(component{i},2)~=N)
            error('the number of predictions of component learners do no consistent.')
        end
        if(size(component{i},1)~=1)
            error('input prediction vector should be row vector.')
        end
end

pred=LabelFormatConvertion(component{1},ClassType);
for i=2:Nc
    pred=pred+LabelFormatConvertion(component{i},ClassType);
end
maxv=max(pred);
maxv=repmat(maxv,size(pred,1),1);
c=repmat(C',1,N);
maxclass=(pred==maxv).*c;
[tmp,id]=max(maxclass);
prediction=ClassType(id);
%end
      