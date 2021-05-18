clear
close all
clc

% addpath('C:\Users\js68897\repos\greensfrac')
% 
% res = resControl(pwd,'SI');
% frac = fracControl(pwd,'SI');
% well = wellControl(pwd,'SI');
% time = numControl(pwd,'SI');
% 
% diffusivity = frac.permeability/(res.fluidViscosity*res.porosity*res.totCompressibility);
% 

% 
% idx = [2,5,10,20,40,70,100];
% 
% for i = 1:length(idx)
%     plot(x,p(idx(i),:),'k-'); hold on
% end
% 
% ylabel('non-dimensional pressure');
% xlabel('non-dimensional distance');

time = [0.01,0.1,0.2,0.5,1];

figure(1)

for i = 1:length(time)
    
    A = analytical(time(i));
    N = numerical(time(i));

    plot(A.x,A.pressure,'k-'); hold on
    plot(N.x,N.pressure,'ro'); hold on

end
