clear all %#ok<CLALL>
close all
clc

% modeling of single, rectangular fracture with constant flow rate ----- %
% input starts here ---------------------------------------------------- %

% dimensions and properties of reservoir

res.xlength = 10000;        % length of reservoir in x-direction [ft]
res.ylength = 8000;         % length of reservoir in y-direction [ft]
res.zlength = 50;           % lenght of reservoir in z-direction [ft]

res.porosity = 0.1;         % reservoir rock porosity
res.totcompres = 3e-6;      % total compressibility [1/psi]

res.fluid.viscosity = 0.6;	% viscosity of fluid [cp]

res.xpermeability = 0.1;	% permeability in x-direction [mD]
res.ypermeability = 0.1;	% permeability in y-direction [mD]
res.zpermeability = 0.1;	% permeability in z-direction [mD]

res.initpressure = 4200;    % initial reservoir pressure at t = 0 [psi]

% dimensions and parameters of fracture

frac.nodes(:,1) = [5000;5000;5000;5000;5000;5000;5000];	% nodes, x-coordinates [ft]
frac.nodes(:,2) = [3790;3860;3930;4000;4070;4140;4210];	% nodes, y-coordinates [ft]

% frac.nodes(:,1) = [5000;5000;5000];	% nodes, x-coordinates [ft]
% frac.nodes(:,2) = [3790;4000;4210];	% nodes, y-coordinates [ft]

% frac.map(:,1) = [1;2;4;5];            % p1 points on one side of segments
% frac.map(:,2) = [2;3;5;6];            % p2 points on the other side of segments

frac.map(:,1) = [1;2;3;4;5;6];                  % points on one side of segments
frac.map(:,2) = [2;3;4;5;6;7];                  % points on the other side of segments

frac.permeability = [42000;42000;42000;42000;42000;42000];      % fracture permeability [mD]

frac.width = [0.01;0.01;0.01;0.01;0.01;0.01];               % fracture width [ft]

% wellbore production constraints

bhc.well = 4;           % bottom hole condition applied node points
bhc.type = 1;               % constant p is 0 and constant q is 1
% bhc.cond = 2000;            % constant bottom hole pressure [psi]
bhc.cond = 25*1.273;      % constant flow rate constraint [bbl/day]

% production time and numeric incremental time

time.tt = 1e2;              % total time calculation carried, days
time.dt = 1e-1;             % delta time, days

% Input Ends Here ------------------------------------------------------ %
% ---------------------------------------------------------------------- %

% conversion of inputs from field units to SI units

[res,frac,bhc,time] = unitconvert(res,frac,bhc,time,'Field_to_SI');

% output the solution in Field units

[res,frac,time,sol] = processor(res,frac,bhc,time);

% conversion of inputs from SI units back to Field units

[res,frac,bhc,time] = unitconvert(res,frac,bhc,time,'SI_to_Field');

% saving results

% string = strcat(pwd,'\results\');
% 
% out = [time.tau,(sum(sol.qw,1)/1.273)'];
% 
% dlmwrite(strcat(string,'out_1frac.dat'),out,'delimiter','\t');

% % plotting results

plotting(time,sol,'Production History');