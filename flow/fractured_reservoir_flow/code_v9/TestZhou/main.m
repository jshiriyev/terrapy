clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

res = resControl(pwd,'SI');
frac = fracControl(pwd,'SI');
time = numControl(pwd,'SI');
well = wellControl(pwd,'SI');

greenSol = green.plane(frac.center,frac,time.tau,res);

sol = solver(res,frac,time,well,greenSol);

% % this plot is in SI units
% figure('Name','Fracture Network','NumberTitle','off','visible','on');
% 
% resultPlot.node(frac);
% 
% prop.Color = 'k';
% prop.LineWidth = 1.5;
% resultPlot.frac(frac,prop);
% 
% prop.MarkerSize = 10;
% prop.Color = 'k';
% resultPlot.well(frac,well,prop);
% 
% xlim([0,res.Length(1)])
% ylim([0,res.Length(2)])

% Bottom Hole Pressure

zhou = load('Zhou_et_al_2014.txt');

figure('Name','Bottom Hole Pressure','NumberTitle','off','visible','on')

prop.Marker = 'o';
prop.Color = 'r';
prop.LineStyle = 'none';

idx = 0:250:10000; idx(1) = 1;

ptime = resultPlot.conversion(time.tau,'sec','FU');
pwell = resultPlot.conversion(sol.pn(well.wellID,:),'Pa','FU');

plot(zhou(:,1),zhou(:,2),'k','LineWidth',1.2); hold on
plot(ptime(idx),pwell(idx),prop); hold on

xlim([0,1000])
ylim([2000,4500])

ylabel('BHP [psi]')
xlabel('Time [days]')

ax = legend('Zhou et al. 2014','Current Model');

ax.Location = 'north';
ax.NumColumns = 2;

grid on

% Production rate
% figure('Name','Production Rate','NumberTitle','off','visible','off')
% 
% propPR.Marker = '.';
% propPR.Color = 'k';
% propPR.LineStyle = 'none';
% propPR.MarkerSize = 5;
% 
% resultPlot.flowRate(sol,time,propPR)
% 
% ylabel('Flow Rate [bbl/day]')
% xlabel('Time [days]')

% saveas(gcf,'results/flowrate.png')
% close(gcf)

% % For map plot, map is in SI, pressure is in field units, psi
% obs = resultPlot.map2D(res,10,8);
% greenMap = greenFunc(obs,frac,time.tau,res).array;
% 
% resultPlot.map2Dplot(obs,sol,res,frac,time,greenMap);