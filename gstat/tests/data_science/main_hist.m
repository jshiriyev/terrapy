close all
clear variables
clear global
clc

% it is reasonable to have it for the same parameter over groups of data

filename = 'unconv_MV_v3.csv';

N = 200;    % number of samples

M = csvread(filename,1,0,[1 0 N 7]);

figure(1)

histogram(M(:,1),15)

set(gca,'FontSize',12)

xlabel('Parameter');
ylabel('percentage of occurence','FontSize',12)

% ylim([-5,30])
% xlim([0,8])

% % grid on