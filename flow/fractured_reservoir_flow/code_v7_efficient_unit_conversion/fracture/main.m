clear all
close all
clc

% Kaustubh added this section ------------------------------------------ %

noOfElementsToTest = 16;      % maximum value 16 for this particular case

load('meshGridData');

meshConverter = meshInterchange(gridInfo.allMeshGrid(1:noOfElementsToTest,1),gridInfo.xElementLength);

allNodeData = meshConverter.productionNodeList;
completeFracMap = meshConverter.productionFracMap;

fileName = strcat('coordHead.txt');

fid = fopen(fileName,'w+');

fprintf(fid,'%% x, y, z coordinates of fracture nodes\n');
fprintf(fid,'%% in 2D solution z coordinate needs to be entered as reservoir depth\n');
fprintf(fid,'%% number of rows needs to be equal to number of node points\n');

fprintf(fid,'length, SI\n');

fclose(fid);

fileName = strcat('coordBody.txt');

dlmwrite(fileName,allNodeData,'delimiter','\t');

system('type coordHead.txt coordBody.txt > coordinates.txt')