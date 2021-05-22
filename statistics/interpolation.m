clear all;
close all;
clc;

fid=fopen('Data_all.bin');
allData=fread(fid,'float');

fid=fopen('Data_in.bin');
d=fread(fid,'float');

L=length(allData);
idx=(1:L)';

nonZero=allData(d~=0);
nonZeroID=idx(d~=0);

l=length(nonZero);

%% Generation of the G matrix and d vector

epslon=1;

G=spalloc(L,L,(L-2)*(L-1));
G=G+sparse(2:L-1,2:L-1,-2*epslon,L,L);
G=G+sparse(1:L-1,2:L,epslon,L,L);
G=G+sparse(2:L,1:L-1,epslon,L,L);
G=G+sparse(1,1,-epslon,L,L);
G=G+sparse(L,L,-epslon,L,L);

G=[sparse(1:l,nonZeroID,1); G];
d=[nonZero; spalloc(L,1,0)];

m=(G'*G)\(G'*d);

%% Plot of the Results

figure(1)
spy(G)
title('Structure of the G matrix');
ylabel('Dimensions of G is 125x100');

figure(2)
scatter(nonZeroID,nonZero,'r'); hold on
plot(idx,allData,'b',idx,m,'m')
title('Interpolation');
legend('Dumped Data','Full Data','Interpolation',3);
grid on