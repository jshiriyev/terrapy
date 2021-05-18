clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

res = resControl(pwd,'SI');
frac = fracControl(pwd,'SI');
time = numControl(pwd,'SI');
well = wellControl(pwd,'SI');

G.cplane = green.plane(frac.center,frac,time.tau,res);
G.volume = green.volume(frac.center,time.tau,res);

sol = solver(res,frac,time,well,G);

% this plot is in SI units
figure('Name','Fracture Network','NumberTitle','off','visible','off');

resultPlot.node(frac);

prop.Color = 'k';
prop.LineWidth = 1.5;
resultPlot.frac(frac,prop);

prop.MarkerSize = 10;
prop.Color = 'k';
resultPlot.well(frac,well,prop);

xlim([0,res.Length(1)])
ylim([0,res.Length(2)])

saveas(gcf,'results/fractureNetwork.png')
close(gcf)

% Bottom Hole Pressure
figure('Name','Bottom Hole Pressure','NumberTitle','off','visible','off')

resultPlot.bhp(sol,time,well);

ylabel('BHP [psi]')
xlabel('Time [days]')

saveas(gcf,'results/bhp.png')
close(gcf)

% Production rate
figure('Name','Production Rate','NumberTitle','off','visible','off')

propPR.Marker = '.';
propPR.Color = 'k';
propPR.LineStyle = 'none';
propPR.MarkerSize = 5;

resultPlot.flowRate(sol,time,propPR)

ylabel('Flow Rate [bbl/day]')
xlabel('Time [days]')

% saveas(gcf,'results/flowrate.png')
% close(gcf)

% % For map plot, map is in SI, pressure is in field units, psi
% obs = resultPlot.map2D(res,10,8);
% greenMap = greenFunc(obs,frac,time.tau,res).array;
% 
% resultPlot.map2Dplot(obs,sol,res,frac,time,greenMap);