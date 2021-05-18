clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

frac = fracControl(pwd,'SI');
res = resControl(pwd,'SI');
time = numControl(pwd,'SI');
well = wellControl(pwd,'SI');

G.cplane = green.plane(frac.center,frac,time.tau,res);
G.volume = green.volume(frac.center,time.tau,res);

sol = solver(res,frac,time,well,G);

% this plot is in SI units
figure('Name','Fracture Network','NumberTitle','off','visible','on');

resultPlot.node(frac);

prop.Color = 'k';
prop.LineWidth = 1.5;
resultPlot.frac(frac,prop);

prop.MarkerSize = 10;
prop.Color = 'k';
resultPlot.well(frac,well,prop);

xlim([0,res.Length(1)])
ylim([0,res.Length(2)])

% saveas(gcf,'results/fractureNetwork.png')
% close(gcf)

% Bottom Hole Pressure
figure('Name','Bottom Hole Pressure','NumberTitle','off','visible','on')

resultPlot.bhp(sol,time,well);

ylabel('BHP [psi]')
xlabel('Time [days]')

% saveas(gcf,'results/bhp.png')
% close(gcf)

% Production rate
figure('Name','Production Rate','NumberTitle','off','visible','on')

propPR.Marker = '.';
propPR.Color = 'k';
propPR.LineStyle = 'none';
propPR.MarkerSize = 5;

resultPlot.flowRate(sol,time,propPR)

ylabel('Flow Rate [bbl/day]')
xlabel('Time [days]')

% % load("rateData.txt")
% plot(rateData(:,1),rateData(:,2),'r.','MarkerSize',5);
% legend('Green Approach','Ashish Model')

% saveas(gcf,'results/flowrate.png')
% close(gcf)

% For map plot, map is in SI, pressure is in field units, psi
obs = resultPlot.map2D(res,10,10);
greenMap = green.plane(obs,frac,time.tau,res);

resultPlot.map2Dplot(obs,sol,res,frac,time,greenMap);