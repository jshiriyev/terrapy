clear
close all
clc

xfrac = 210;        % fracture half length, feet

coord = (70:70:xfrac)';

% coord = 210;

ycoord = [flipud(4000-coord);4000;4000+coord];

numnode = size(ycoord,1);

xcoord = ones(numnode,1)*5000;
zcoord = ones(numnode,1)*50;

allNodeData = [xcoord,ycoord,zcoord];

completeFracMap = [1:(numnode-1);2:numnode]';

numfrac = size(completeFracMap,1);

wellID = (numnode+1)/2;

frac_perm = 42000; % mD
frac_wdth = 0.01; % ft

inpwrite('coordinates.txt','#length, FU',allNodeData);
dlmwrite('map.txt',completeFracMap,'delimiter','\t','newline', 'pc');
inpwrite('permeability.txt','#permeability, FU',ones(numfrac,1)*frac_perm);
inpwrite('width.txt','#length, FU',ones(numfrac,1)*frac_wdth);
% dlmwrite('well.txt',[2;31.825;wellID],'delimiter','\t','newline');

fid = fopen('well.txt','w+');
fprintf(fid,'2\t#\t\r\n');
fprintf(fid,'31.825\t#bbl/day\t\r\n');
fprintf(fid,[num2str(wellID),'\r\n']);
fclose(fid);