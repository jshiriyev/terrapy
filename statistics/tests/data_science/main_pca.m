close all
clear variables
clear global
clc

filename = 'unconv_MV_v3.csv';

M = csvread(filename,1,0,[1 0 200 7]);

x1 = M(:,2);
x2 = M(:,3);
x3 = M(:,4);
x4 = M(:,5);

%% Scaling Data

X1 = (x1-mean(x1))/std(x1);    % scaling is on !!!
X2 = (x2-mean(x2))/std(x2);    % scaling is on !!!
X3 = (x3-mean(x3))/std(x3);    % scaling is on !!!
X4 = (x4-mean(x4))/std(x4);    % scaling is on !!!

figure(1)

plot(X1,X2,'k.','MarkerSize',10)

xlabel('Porosity [%]')
ylabel('LogPermeability')

% xlim([-5,5])
% ylim([-5,5])

%% PCA analysis

% coeff is the factor loading which simply is the correlation
% of the specific variable on the respective principal component (PC)

[coeff,PC,latent,tsquared,explained,mu] = pca([X1,X2,X3,X4]);

% latent : the variance of PC
% explained : the percentage of the total variance explained by each PC
% mu : the estimated mean of X1, X2, X3 ...

% figure(2)
% 
% plot(PC(:,1),PC(:,2),'k.','MarkerSize',10)
% 
% xlabel('PC1')
% ylabel('PC2')
% 
% xlim([-5,5])
% ylim([-5,5])

%% Dimensional Reduction 

k = 2; % number of principal components to use

Z = PC(:,1:k)*coeff(1:k,1:k)';

figure(3)

plot(Z(:,1),Z(:,2),'k.','MarkerSize',10)

xlabel('Porosity [%]')
ylabel('LogPermeability')

% xlim([-5,5])
% ylim([-5,5])