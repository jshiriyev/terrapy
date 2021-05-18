% clear all
% close all
% clc

% Fracture Modeling using Green's solution of Diffusivity Equation ----- %
% Structure Dictionary used in the simulation:
%
% SI : international system of units
% FU : field units used in the oil industry
% DL : dimensionless
% ---------------------------------------------------------------------- %

frac = fracControl.read(pwd);
frac = fracControl.property(frac);
fracControl.plot(frac,[-20,20,0,40])

% res = reservoir.property(pwd);
% frc = fracture.property(pwd);
% bhc = wellbore.property(pwd);
% num = numerics.property(pwd);

% [res,frac,time,sol] = processor(res,frac,bhc,time);
% 
% % saving results
% 
% string = strcat(pwd,'\results\');
% 
% out = [time.tau,(sum(sol.qw,1)/1.273)',sol.pw'];
% 
% dlmwrite(strcat(string,'out_timedep_fraccond.dat'),out,'delimiter','\t');
% 
% % plotting(time,sol,'Production History');