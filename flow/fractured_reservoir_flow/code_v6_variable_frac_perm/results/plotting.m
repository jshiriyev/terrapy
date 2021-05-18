clear all %#ok<CLALL>
close all
clc

F1 = load(strcat(pwd,'\','out_no_geomechanics.dat'));
F2 = load(strcat(pwd,'\','out_timedep_fraccond.dat'));

time1 = F1(:,1);                 % [day]
flow_rate1 = F1(:,2);            % [STB/day]
bottom_hole_pressure1 = F1(:,3); % [psi]

time2 = F2(:,1);                 % [day]
flow_rate2 = F2(:,2);            % [STB/day]
bottom_hole_pressure2 = F2(:,3); % [psi]

cum_oil_prod1 = cumsum(flow_rate1*time1(1))/1000;   % [MSTB]
cum_oil_prod2 = cumsum(flow_rate2*time2(1))/1000;   % [MSTB]

figure('Name','Production History','NumberTitle','off')

plot(time1,cum_oil_prod1,'k'); hold on
plot(time2,cum_oil_prod2,'r.'); hold on

legend('without-geomeachanics','with-geomeachanics','location','NorthWest');

% xlim([min(F1(:,1)),max(F1(:,1))])

% ylim([0,200])

ylabel('Cumulative Oil Production [MSTB]')
xlabel('Time [days]')

grid on

% ylim([2000,4500])
% 
% set(gca,'XTickLabels',[])
% 
% ylabel('BHP [psi]')
% 
% subplot(2,1,2)
% 
% plot(time.tau,sol.qw/1.273);
% 
% xlim([0,max(time.tau)])
% ylim([10,50])
    