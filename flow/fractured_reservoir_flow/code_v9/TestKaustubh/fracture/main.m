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

frac_perm = 42000; % mD
frac_wdth = 0.01; % ft

inpwrite('coordinates.txt','length, SI',[allNodeData(:,1)+50,allNodeData(:,2)+30,ones(numnode,1)*15.24]);
inpwrite('map.txt','-, -',completeFracMap);
inpwrite('permeability.txt','permeability, FU',ones(numfrac,1)*frac_perm);
inpwrite('width.txt','length, FU',ones(numfrac,1)*frac_wdth);