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

% Kaustubh added this section ------------------------------------------ %

% noOfElementsToTest = 16; %maximum value 16 for this particular case
% 
% load('meshGridData');

meshConverter = meshInterchange(gridInfo.allMeshGrid(1:noOfElementsToTest,1),gridInfo.xElementLength);

allNodeData = meshConverter.productionNodeList;
completeFracMap = meshConverter.productionFracMap;

fileName = strcat(pwd,'\fracture\','coordHead.txt');

fid = fopen(fileName,'w+');

fprintf(fid,'%% x, y, z coordinates of fracture nodes\n');
fprintf(fid,'%% in 2D solution z coordinate needs to be entered as reservoir depth\n');
fprintf(fid,'%% number of rows needs to be equal to number of node points\n');

fprintf(fid,'length, SI\n');

fclose(fid);

fileName = strcat(pwd,'\fracture\','coordBody.txt');

dlmwrite(fileName,allNodeData,'delimiter','\t');

system('type coordHead.txt coordBody.txt > coordinates.txt')

% ---------------------------------------------------------------------- %

frac = fracControl.read(pwd);
frac = fracControl.property(frac);
fracControl.plot(frac,[-10,130,-10,30])

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