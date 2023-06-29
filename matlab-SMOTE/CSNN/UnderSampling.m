function    [sample,sampleLabel]=undersampling(data,Label,ClassType,C,AttVector)
% Implement under-sampling algorithm.
% It changes the training data distribution by deleting some 
% lower-cost training examples until the appearances of different 
% training examples are proportional to their costs. Here a routine 
% similar to that used in [1] is employed, which removes redundant 
% examples at first and then removes borderline examples, the latter 
% can be detected using Tomek links [2].
%
%Usage:
%  [sample,sampleLabel]=undersampling(data,Label,ClassType,C,attribute)
%
%  sample: new training set after under-sampling to build cost-sensitive NN
%               format - row indexes attributes and column indexes
%               instances                             
%  sampleLabel: class labels for instances in new training set. 
%                       format - row vector
%  data: original training set.
%           format - row indexes attributes and column indexes instances
%  Label: class labels for instances in original training set
%            format - row vector
%  ClassType: class type
%  C: cost vector. the ith entry is the cost of misclassifying ith class
%      instance, without considering the concrete class the instance has
%      been wrongly assigned to.
%  AttVector: attribute vector,1 presents for the corresponding attribute
%                   is nominal and 0 for numeric.
%
% Refer [1]:
% M. Kubat and S. Matwin, ¡°Addressing the curse of imbalanced training
% sets: one-sided selection,¡± in Proceedings of the 14th International 
% Conference on Machine Learning, Nashville, TN, pp.179¨C186, 1997.
% Refer [2]:
% I. Tomek, ¡°Two modifications of CNN,¡± IEEE Transactions on Systems, Man
% and Cybernetics, vol.6, no.6, pp.769¨C772, 1976.


%check parameters
NumClass=length(ClassType);
if(length(C)~=NumClass)
    error('class number does not consistent.')
end
if(size(data,2)~=size(Label))
    error('instance numbers in data and Label do not consistent.')
end
if(size(data,1)~=length(AttVector))
    error('attribute numbers in data and AttVector do not consistent.')
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
[tmp,baseClassID]=max(cn);
newClassD=floor(C/C(baseClassID)*ClassD(baseClassID));

%ascending C
[tmp,ascendC]=sort(C);
%prepare for VDM used in the distance function
attribute=VDM(data,Label,ClassType,AttVector);

%sampling
for i=ascendC    
    if(newClassD(i)<ClassD(i))    
        D=[];
        DL=[];
        K=[];
        % put instances of other classes into D
        for j=1:NumClass
            if(j~=i)
                D=[D ClassData{j}];
                l=ones(1,ClassD(j)).*ClassType(j);
                DL=[DL l];
            end
        end
        %break ClassData{i} into 2 parts
        n=floor(newClassD(i)/2);
        id=randperm(ClassD(i)); 
        id1=id(1:n);
        id2=id(n+1:end);
        % put Ni*/2 class i instances into D 
        D=[D ClassData{i}(:,id1)];
        l=ones(1,n).*ClassType(i);
        DL=[DL l];
        % put the remaining class i instances into K
        K=ClassData{i}(:,id2);
        K_flag=zeros(1,length(id2));% 0 unchanged,1 moved to D, -1 deleted        
        
        diff=ClassD(i)-newClassD(i);        
        NumDelIns=0;
        
        %check instances in K to delete redundant ones     
        while(NumDelIns<diff & length(find(K_flag==0))>0)
            %randomly pick an unchecked instance to check
            id=find(K_flag==0);
            rn=round(rand(1,1)*(length(id)-1))+1;
            id=id(rn);
            instance=K(:,id);
            target=ClassType(i); 
            % calculate distance which used for classification with 1-NN rule
            d=dist_nominal(instance,D,attribute,AttVector);
            % 1-NN
            [tmp,mind]=min(d);
            if(target==DL(mind))%delete
                K_flag(id)= -1;                
                NumDelIns=NumDelIns+1;
            else% move to D
                K_flag(id)= 1;                  
            end     
        end       
       
        %if enough instances have been deleted, merge the remaining into D
        if(NumDelIns==diff)            
             id=find(K_flag~= -1);
             D=[D K(:,id)];            
             l=ones(1,length(id))*ClassType(i);
             DL=[DL l];
             K=[];
             K_flag=[];
        %if not  
        else     
            %merge unredundant instances into D
            id=find(K_flag==1);
            D=[D K(:,id)];
            l=ones(1,length(id))*ClassType(i);
            DL=[DL l];
            K=[];
            K_flag=[];           
            
            %check the i-th class in D to delete borderline examples
            idClassi=find(DL==ClassType(i));              
            while(NumDelIns<diff & ~isempty(idClassi))    
                %randomly pick up an instance from the i-th class
                rn=round(rand(1,1)*(length(idClassi)-1))+1;            
                id=idClassi(rn);
                X=D(:,id);
                target=ClassType(i);
                % calculate distances to identify Tomek links
                d=dist_nominal(X,D,attribute,AttVector);
                d(id)=inf;
                iid=find(isnan(DL)==1);
                d(iid)=nan;
                [tmp,NearX]=min(d);                
                                
                if(target~=DL(NearX))
                    Y=D(:,NearX);                    
                    d=dist_nominal(Y,D,attribute,AttVector);
                    d(NearX)=Inf;
                    iid=find(isnan(DL)==1);
                    d(iid)=nan;                    
                    [tmp,NearY]=min(d);
                    if(NearY==id)%delete  borderline example                      
                        DL(id)=NaN;
                        NumDelIns=NumDelIns+1;                        
                    end                
                end
                idClassi=setdiff(idClassi,id);                
            end%while 
            
            id=find(isnan(DL)==0);
            D=D(:,id);
            DL=DL(id);
            % if it still needs to delete some instances, randomly delete
            % until requirement is meet
            if( isempty(idClassi) & NumDelIns<diff ) 
                idClassi=find(DL==ClassType(i));
                id=randperm(length(idClassi));
                id=idClassi(id(1:diff-NumDelIns));
                id=setdiff(1:length(DL),id);
                D=D(:,id);
                DL=DL(id);                
            end
         
        end%if-elseif
        
        %update the i-th class after under-sampling
        id=find(DL==ClassType(i));
        ClassData{i}=D(:,id);
   
    end%if
end%for

sample=D;
sampleLabel=DL;

