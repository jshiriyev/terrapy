clear all %#ok<CLALL>
close all
clc

% Fracture Modeling using Green's solution of Diffusivity Equation

% Structure Dictionary used in the simulation:
%
% inp : input parameter, does not change at any step of simulation
% out : output parameter calculated during the run of simulation
%
% SI : international system of units
% FU : field units used in the oil industry
% DL : dimensionless
% 
% properties to be used as the last field of structures:
% length, porosity, compressibility, viscosity, permeability, pressure,
% flowrate, time, map

% ---------------------------------------------------------------------- %
% input starts here ---------------------------------------------------- %

% res:reservoir dimensions and properties

res.xdir.lenght = assign.property(10000,'length','FU');             % length of reservoir in x-direction [ft]
res.ydir.length = 8000;              % length of reservoir in y-direction [ft]
res.zdir.length = 50;                % length of reservoir in z-direction [ft]

res.rock.porosity = 0.1;             % reservoir rock porosity

res.total.compressibility = 3e-6;    % total compressibility [1/psi]

res.fluid.viscosity = 0.6;           % viscosity of fluid [cp]

res.xdir.permeability = 0.1;         % permeability in x-direction [mD]
res.ydir.permeability = 0.1;         % permeability in y-direction [mD]
res.zdir.permeability = 0.1;         % permeability in z-direction [mD]

res.initial.pressure = 4200;         % initial reservoir pressure at t = 0 [psi]

% frac:fracture dimensions and parameters of fracture

frac.node.xcoord = [5000;5000;5000];	% nodes, x-coordinates [ft]
frac.node.ycoord = [3790;4000;4210];	% nodes, y-coordinates [ft]

frac.node.point1 = [1;2];               % points on one side of segments
frac.node.point2 = [2;3];               % points on the other side of segments

frac.permeability = [42000;42000];   % fracture permeability [mD]
frac.width = [0.01;0.01];     % fracture width [ft]

% bhc:bottom hole conditions

bhc.well.node = 2;               % bottom hole condition applied node points
bhc.constant.pressure = 2000;        % constant bottom hole pressure [psi]
% bhc.FU.constant.flowrate = 25*1.273;  % constant flow rate constraint [bbl/day]

% num:numerical inputs such as total production time and incremental time

num.total.time = 1e3;                % total time calculation carried, days
num.delta.time = 1e-1;               % delta time, days

% Input Ends Here ------------------------------------------------------ %
% ---------------------------------------------------------------------- %

[res,frac,time,sol] = processor(res,frac,bhc,time);

% saving results

string = strcat(pwd,'\results\');

out = [time.tau,(sum(sol.qw,1)/1.273)',sol.pw'];

dlmwrite(strcat(string,'out_timedep_fraccond.dat'),out,'delimiter','\t');

% % plotting results
% 
% plotting(time,sol,'Production History');