function   [TMprediction, TMout]=ThresholdMovNN(out,Cost,ClassType)
% Impelementation of threshlod-moving algorithm that is a post process 
% of Neural Network to make NN cost sensitive by threshold moving,
% therefore it becomes harder for higher-cost instances to be misclassified.
%
%Usage:
%  [TMprediction, TMout]=ThresholdMovNN(out,Cost,ClassType)
%
%  TMout: cost-sensitive real-value outputs (0-1 class label vector format,
%              row indexes class type and column indexes instances) of test 
%              instances after threshlod-moving
%  TMprediction: cost-sensitive predictions (scalar class label format) of
%                       test instances after threshlod-moving. An instance is predicted 
%                       as the class with the biggest TMout. when there are more than
%                       1 maximum values in TMout, take the one which has the 
%                        biggest value in out.  
%  out  : real-value outputs of the cost-blind NN which is trained first for
%           threshold-moving method. It's class label format is 0-1 class
%           label vector,  the row must index class type and column must 
%           index instances
%  Cost : Cost Matrix.Cost[i,j] denotes the cost of misclassifying a j-th 
%            class instance as belonging to the i-th class;and Cost(i,i)=0 
%  ClassType: class type.



%check
if(nargin<3)
    help ThresholdMovNN;
end
if(size(out,1)~=size(Cost,1))
    error('out and Cost are not consistent on class number.');
end

NumClass=size(Cost,1);
NumIns=size(out,2);
TMout=zeros(size(out));
for i=1:NumClass   
    TMout(i,:)=out(i,:)*sum(Cost(:,i));    
end
TMout=normalize(TMout);

%convert  real-value outputs to predictions
[maxv,id]=max(TMout);
maxv=repmat(maxv,NumClass,1);
maxclass=(abs(maxv-TMout)<1e-6).*out;
[tmp,id]=max(maxclass);
TMprediction=ClassType(id);   

%end
        
