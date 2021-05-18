clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

resSI = resControl([pwd,'\test_1'],'SI');
timeSI = numControl([pwd,'\test_1'],'SI');
wellSI = wellControl([pwd,'\test_1'],'SI');

resFU = resControl([pwd,'\test_1'],'FU');
timeFU = numControl([pwd,'\test_1'],'FU');
wellFU = wellControl([pwd,'\test_1'],'FU');

% R = linspace(wellSI.radius,resSI.xLength/2)';
R = [2.5;10;50;100];

obs.Xcoord = resSI.xLength/2+R;
obs.Ycoord = resSI.yLength/2;
obs.Zcoord = 0;

src.p1.Xcoord = resSI.xLength/2;
src.p1.Ycoord = resSI.yLength/2;
src.p1.Zcoord = resSI.zLength;

% src.p2.Xcoord = resSI.xLength/2;
% src.p2.Ycoord = resSI.yLength/2;
% src.p2.Zcoord = 0;

% green = green(obs,src,timeSI.tau,resSI,'line');

dynamic = green.line(obs,src,timeSI.tau,resSI);
static = green.volume(obs,timeSI.tau,resSI);

dynamic = permute(dynamic,[1,3,2]);
static = permute(static,[1,3,2]);

gterm = cumsum(dynamic*timeSI.deltaTime,2);

p = resSI.initPressure*static-gterm*wellSI.consFlowrate/resSI.zLength;

pGS = p/inpput.convFactorDetermine('pressure');

R = R/inpput.convFactorDetermine('length');

pLS = EiSolution(R,timeFU.tau*24,resFU,wellFU);

figure('Name','Pressure Comparison','NumberTitle','off');   %,'Position',[0,0,600,600]

idx = 0:250:10000; idx(1) = 1;

for i = 1:length(R)
    plot(timeFU.tau,pLS(i,:),'k','LineWidth',1.12); hold on
    plot(timeFU.tau(idx),pGS(i,idx),'ro'); hold on
end

ax = legend('Line Source','Green Solution');

ax.Location = 'north';
ax.NumColumns = 2;

ylim([2000,3200])

ylabel('pressure [psi]')
xlabel('time [days]')

% [X,Y] = meshgrid(timeFU.tau,R);
% 
% figure(1)
% 
% Z1 = griddata(timeFU.tau,R,pLS,X,Y);
% 
% h = pcolor(X,Y,Z1);
% 
% title('Line Source Solution')
% 
% set(h,'EdgeColor','none');
% shading interp
% 
% colormap(jet)
% colorbar
% 
% hold on
% 
% figure(2)
% 
% Z2 = griddata(timeFU.tau,R,pGS,X,Y);
% 
% h = pcolor(X,Y,Z2);
% 
% title('Green Solution')
% 
% set(h,'EdgeColor','none');
% shading interp
% 
% colormap(jet)
% colorbar
% 
% hold on


