clear
close all
clc

% time = [0.01,0.1,0.2,0.5,1];
% 
% % figure(1)
% 
% for i = 1:length(time)
%     
%     A = analytical(time(i));
%     N = numerical(time(i));
% 
%     plot(A.x,A.pressure,'k-'); hold on
%     plot(N.x,N.pressure,'ro'); hold on
% 
% end

nodeCoord = [0;0.3;0.4;0.6;0.9;1];
map = [1,2;2,3;3,4;4,5;5,6];

Length = nodeCoord(2:end)-nodeCoord(1:end-1);

H = 1;  % reservoir depth

numAfrac = size(map,1);
numAnode = size(nodeCoord,1);

permeability = ones(numAfrac,1)*0.1;
width = 0.01;

areatofracture = width*H;

oilViscosity = 1;
oilFVF = 1;

Tmat = zeros(numAfrac,numAfrac);

for host = 1:numAfrac

    [neighbor,~] = find(sum(permute(map,[1,3,2])==map(host,:),3));
    
    neighbor(neighbor==host)=[];
    
    deltax = (Length(host)+Length(neighbor))/2;
    
    perm = deltax./(Length(host)/permeability(host)/2+...
                    Length(neighbor)./permeability(neighbor)/2);
                
    trans = perm.*areatofracture/oilViscosity/oilFVF./deltax;
    
    Tmat(host,host) = sum(trans);
    Tmat(host,neighbor) = -trans;
    
end