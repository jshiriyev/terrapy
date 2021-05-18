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

frac.p1(:,1) = [5000;5000]; % point 1, x-coordinates [ft]
frac.p1(:,2) = [3790;4000]; % point 1, y-coordinates [ft]
frac.p2(:,1) = [5000;5000]; % point 2, x-coordinates [ft]
frac.p2(:,2) = [4000;4210]; % point 2, y-coordinates [ft]

frac.permeability = [42000;42000];	% fracture permeability [mD]
frac.width = [0.01;0.01];           % fracture width [ft]

% wellbore production constraints

bhc.idx = 2;                % bottom hole condition applied node point
bhc.c = 0;                  % 1 is constant q, 0 is constant p
bhc.q = 25*1.273;           % constant flow rate constraint [bbl/day]
bhc.p = 2000;               % constant bottom hole pressure [psi]

% production time and numeric incremental time

time.tt = 1e3;              % total time calculation carried, days
time.dt = 1e-1;             % delta time, days

% Input Ends Here ------------------------------------------------------ %
% ---------------------------------------------------------------------- %

% conversion of inputs from field units to SI units

[res,frac,bhc,time] = unitconvert(res,frac,bhc,time,'Field_to_SI');

% solving, tau, q_well, p_well is in the Field units

[res,frac,tau,q_well,p_well] = processor(res,frac,bhc,time);

% conversion of inputs from SI units back to Field units

[res,frac,bhc,time] = unitconvert(res,frac,bhc,time,'SI_to_Field');

% plotting results

plotting(tau,p_well,q_well,'Production History');