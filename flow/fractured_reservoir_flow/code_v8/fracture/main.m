% clear all
% close all
% clc

% Kaustubh added this section ------------------------------------------ %

load('meshGridData.mat');

noOfElementsToTest = 246;      % maximum value 16 for this particular case

meshConverter = meshInterchange(gridInfo.allMeshGrid(1:noOfElementsToTest,1),gridInfo.xElementLength);

allNodeData = meshConverter.productionNodeList;
completeFracMap = meshConverter.productionFracMap;

numnode = size(allNodeData,1);
numfrac = size(completeFracMap,1);

inpwrite('coordinates.txt','length, SI',[allNodeData,ones(numnode,1)*20]);
inpwrite('map.txt','-, -',completeFracMap);
inpwrite('permeability.txt','permeability, FU',ones(numfrac,1)*42000);
inpwrite('width.txt','length, FU',ones(numfrac,1)*0.01);