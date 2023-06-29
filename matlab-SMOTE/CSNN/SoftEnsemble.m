function    prediction=SoftEnsemble(component,ClassType,C)
% Implement soft-ensemble method
% In this method, every component learner votes using real-values
% for a class and then the class receiving the biggest number of votes 
% is returned. If a tie appears, that is, there are multiple classes 
% receiving the biggest number of votes, then the class with the
% biggest cost is returned.
%
%Usage
%  prediction=SoftEnsemble(component,ClassType,C)
%
%  prediction: predictions of the soft-ensemble
%  component: cell array which contains at least 3 component learner's
%                     normalized real-value outputs. 
%  ClassType: class type
%  C: cost vector. C[i] is the cost of misclassifying ith class
%      instance, without considering the concrete class the instance has
%      been wrongly assigned to.

if(nargin<3)
    help SoftEnsemble
end
Nc=length(component);
if(Nc<3)
    error('not enough components.')
end
NumClass=length(ClassType);
N=size(component{1},2);
for i=2:Nc
    if(NumClass~=size(component{i},1))
        error('class number does not consistent')
    end
    if(N~=size(component{i},2))
        error('the number of predictions of component learners do no consistent.')
    end
end
    
    
out=component{1};
for i=2:Nc
    out=out+component{i};
end
maxv=max(out);
maxv=repmat(maxv,size(out,1),1);
c=repmat(C',1,N);
maxclass=(abs(out-maxv)<1e-6).*c;
[tmp,id]=max(maxclass);
prediction=ClassType(id);
%end
      