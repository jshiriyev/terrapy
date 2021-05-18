clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

frac = fracControl(pwd);
res = resControl(pwd);
time = numControl(pwd);

greenSol = greenFunc(frac.center,frac,time.tau,res).array;

sol = solver(res,frac,time,greenSol);

% this plot is in SI units
figure('Name','Fracture Network','NumberTitle','off','visible','off');

resultPlot.node(frac);

prop.Color = 'k';
prop.LineWidth = 1.5;
resultPlot.frac(frac,prop);

prop.MarkerSize = 10;
prop.Color = 'k';
resultPlot.well(frac,prop);

xlim([0,res.Length.SI(1)])
ylim([0,res.Length.SI(2)])

saveas(gcf,'results/fractureNetwork.png')
close(gcf)

% Bottom Hole Pressure
figure('Name','Bottom Hole Pressure','NumberTitle','off','visible','off')

resultPlot.bhp(sol,frac,time);

ylabel('BHP [psi]')
xlabel('Time [days]')

ylim([2000,4500])

grid on

saveas(gcf,'results/bhp.png')
close(gcf)

%

idx = [1,10,100,1000,10000];

for j=1:5
    plot(sol.pn(:,idx(j)));
end

%

% % Production rate
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
% 
% saveas(gcf,'results/flowrate.png')
% close(gcf)
% 
% % For map plot, map is in SI, pressure is in field units, psi
% obs = resultPlot.map2D(res,25,20);
% greenMap = greenFunc(obs,frac,time.tau,res).array;
% 
% resultPlot.map2Dplot(obs,sol,res,frac,time,greenMap);