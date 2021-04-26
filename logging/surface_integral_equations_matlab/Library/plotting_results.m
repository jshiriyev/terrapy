clear all
close all
clc

string = strcat(pwd,'\hfd_1m_0deg\');
% string_est = strcat(pwd,'\hfd_8m_30deg_est\');

Vhfd = load(strcat(string,'out.dat'));
% Vhfd_est = load(strcat(string_est,'out.dat'));

range = load(strcat(string,'sampling.txt'));

RofS = range(:,3);
RofO1 = range(:,6);
RofO2 = range(:,9);

spacing1 = 1:41;
spacing2 = 42:82;

z_axis = mean([range(spacing1,6) range(spacing1,9)],2);

figure(1)

semilogy(z_axis,abs(Vhfd(spacing1,5)),'k-','LineWidth',1.5); hold on
semilogy(z_axis,abs(Vhfd(spacing1,6)),'k-','LineWidth',1.5); hold on

% semilogy(z_axis,abs(Vhfd_est(spacing1,5)),'r--','LineWidth',1.5); hold on
% semilogy(z_axis,abs(Vhfd_est(spacing1,6)),'r--','LineWidth',1.5); hold on

set(gca,'FontSize',14)

title('short & co-axial','FontSize',14)
ylabel(['Differential signal [',char(181),'V]'],'FontSize',14)
xlabel('Distance [m]','FontSize',14)

figure(2)

semilogy(z_axis,abs(Vhfd(spacing2,5)),'k-','LineWidth',1.5); hold on
semilogy(z_axis,abs(Vhfd(spacing2,6)),'k-','LineWidth',1.5); hold on

% semilogy(z_axis,abs(Vhfd_est(spacing2,5)),'r--','LineWidth',1.5); hold on
% semilogy(z_axis,abs(Vhfd_est(spacing2,6)),'r--','LineWidth',1.5); hold on

set(gca,'FontSize',14)

title('long & co-axial','FontSize',14)
ylabel(['Differential signal [',char(181),'V]'],'FontSize',14)
xlabel('Distance [m]','FontSize',14)

% figure(3)
% 
% semilogy(z_axis,abs(Vhfd(spacing1,3)),'k-','LineWidth',1.5); hold on
% semilogy(z_axis,abs(Vhfd(spacing1,4)),'k-','LineWidth',1.5); hold on
% 
% % semilogy(z_axis,abs(Vhfd_est(spacing1,3)),'r--','LineWidth',1.5); hold on
% % semilogy(z_axis,abs(Vhfd_est(spacing1,4)),'r--','LineWidth',1.5); hold on
% 
% set(gca,'FontSize',14)
% 
% title('short & cross-polarized','FontSize',14)
% ylabel(['Differential signal [',char(181),'V]'],'FontSize',14)
% xlabel('Distance [m]','FontSize',14)
% 
% figure(4)
% 
% semilogy(z_axis,abs(Vhfd(spacing2,3)),'k-','LineWidth',1.5); hold on
% semilogy(z_axis,abs(Vhfd(spacing2,4)),'k-','LineWidth',1.5); hold on
% 
% % semilogy(z_axis,abs(Vhfd_est(spacing2,3)),'r--','LineWidth',1.5); hold on
% % semilogy(z_axis,abs(Vhfd_est(spacing2,4)),'r--','LineWidth',1.5); hold on
% 
% set(gca,'FontSize',14)
% 
% title('long & cross-polarized','FontSize',14)
% ylabel(['Differential signal [',char(181),'V]'],'FontSize',14)
% xlabel('Distance [m]','FontSize',14)