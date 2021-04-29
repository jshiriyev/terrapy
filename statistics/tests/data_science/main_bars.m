close all
clear variables
clear global
clc

% it is reasonable to have it for the same parameter over groups of data

filename = 'unconv_MV_v3.csv';

N = 200;    % number of samples

M = csvread(filename,1,0,[1 0 N 7]);

figure(1)

boxplot(M(:,1:7),'Labels',{'Well','Por','LogPerm','Al','Brittle','TOC','VR'})
set(gca,'FontSize',12)
% ylim([-5,30])
% xlim([0,8])
ylabel('Variation of each Parameter','FontSize',12)
xlabel('Parameters','FontSize',12)
% grid on