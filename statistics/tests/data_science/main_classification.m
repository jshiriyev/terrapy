close all
clear variables
clear global
clc

hTree = findall(0,'Tag','tree viewer');
close(hTree)

filename = 'unconv_MV_v3.csv';

N = 200;    % number of samples

M = csvread(filename,1,0,[1 0 N 7]);

Y = zeros(N,1);

X = M(:,[2,5]);
% Y = M(:,8);

L1 = and(X(:,1)<15,X(:,2)<50);
L2 = and(X(:,1)>=15,X(:,2)<60);
L3 = ~or(L1,L2);

Y(L1) = 1;
Y(L2) = 2;
Y(L3) = 3;

figure(1)

scatter(X(:,1),X(:,2),40,Y); hold on
% scatter(X(L1,1),X(L1,2),40,Y); hold on
% scatter(X(L2,1),X(L2,2),40,Y); hold on
% scatter(X(L3,1),X(L3,2),40,Y); hold on

% colormap(flipud(gray))
colorbar

xlabel('Porosity [%]')
ylabel('Brittle')

xlim([0,30])
ylim([0,100])

%% Linear and Quadratic Discriminant Analysis

lda = fitcdiscr(X,Y);               % fit discriminant analysis classifier (classificication with the default linear discriminant analysis (LDA)
ldaClass = resubPredict(lda);       % predict ensemble response by resubstitution
ldaResubErr = resubLoss(lda);       % computing the resubstitution error

% QDA can be used when the regions for various classes are not well
% separated by lines

qda = fitcdiscr(X,Y,'DiscrimType','quadratic'); % classificication with the quadratic discriminant analysis (QDA)
qdaClass = resubPredict(qda);
qdaResubErr = resubLoss(qda);

bad_l = ~(ldaClass==Y);       
bad_q = ~(qdaClass==Y);  

plot(X(bad_q,1),X(bad_q,2),'kx');       % x is put on the misclassified points

% [ldaResubCM,grpOrder] = confusionmat(Y,ldaClass);

figure(2)

[x,y] = meshgrid(0:2:30,0:4:100);
x = x(:);
y = y(:);
DAlabel = classify([x y],X,Y,'linear');
scatter(x,y,40,DAlabel)

% colormap(flipud(gray))
colorbar

xlabel('Porosity [%]')
ylabel('Brittle')

xlim([0,30])
ylim([0,100])
% 
% % calculating the test error (generalization error, expected prediction error on an independent set)
% 
% rng(0,'twister');                       % to reproduce exact results (setting initial random seed)
% 
cp = cvpartition(Y,'KFold',2);         % create cross-validation partition for data
% 
% cvlda = crossval(lda,'CVPartition',cp); % loss estimate using cross validation
% ldaCVErr = kfoldLoss(cvlda);            % cross-validation loss of partitioned regression model
% 
% cvqda = crossval(qda,'CVPartition',cp); % loss estimate using cross validation
% qdaCVErr = kfoldLoss(cvqda);            % cross-validation loss of partitioned regression model

%% Naive Bayes Classifier

% nbGau = fitcnb(X,Y);
% nbGauResubErr = resubLoss(nbGau);
% 
% nbGauCV = crossval(nbGau,'CVPartition',cp);
% nbGauCVErr = kfoldLoss(nbGauCV);
% 
% figure(3)
% 
% NBlabel = predict(nbGau,[x y]);
% scatter(x,y,40,NBlabel)
% 
% colorbar
% 
% title('Multivariate Normal Distribution')
% xlabel('Porosity [%]')
% ylabel('Brittle')
% 
% xlim([0,30])
% ylim([0,100])

% Modeling each variable in each class using a kernell density estimation

% nbKD = fitcnb(X,Y,'DistributionNames','kernel','Kernel','box');
% nbKDResubErr = resubLoss(nbKD);
% 
% nbKDCV = crossval(nbKD,'CVPartition',cp);
% nbKDCVErr = kfoldLoss(nbKDCV);
% 
% figure(4)
% 
% KDlabel = predict(nbKD,[x y]);
% scatter(x,y,40,KDlabel)
% 
% colorbar
% 
% title('Kernel Density Estimation')
% xlabel('Porosity [%]')
% ylabel('Brittle')
% 
% xlim([0,30])
% ylim([0,100])

%% Decision Tree Analysis

% tree = fitctree(X,Y);
% dtResubErr = resubLoss(tree);
% 
% cvt = crossval(tree,'CVPartition',cp);
% dtCVErr = kfoldLoss(cvt);
% 
% figure(5)
% 
% [grpname,node] = predict(tree,[x y]);
% scatter(x,y,40,grpname)
% 
% colorbar
% 
% title('Decision Tree')
% xlabel('Porosity [%]')
% ylabel('Brittle')
% 
% xlim([0,30])
% ylim([0,100])
% 
% view(tree,'Mode','graph');
% 
% resubcost = resubLoss(tree,'Subtrees','all');
% [cost,secost,ntermnodes,bestlevel] = cvloss(tree,'Subtrees','all');
% 
% figure(6)
% 
% plot(ntermnodes,cost,'b-'); hold on
% plot(ntermnodes,resubcost,'r--'); hold on
% 
% % xticks([1 2 3 4])
% % xticklabels({'1','2','3','4'})
% 
% xlabel('number of terminal nodes');
% ylabel('misclassification error');
% 
% legend('Cross-validation','Resubstitution')
% 
% pruned_tree = prune(tree,'Level',bestlevel);
% view(pruned_tree,'Mode','graph')