clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

string = [pwd,'\test_3\'];

file2Name = [string,'map.txt'];
file5Name = [string,'well.txt'];
file6Name = [string,'qf.txt'];
file7Name = [string,'result.txt'];

frac.map = inpput.read(file2Name,'SI').list.value;

wellInfo = inpput.read(file5Name,'SI');

if strcmp(wellInfo.prop1.quantity,'pressure')
    well.consPressure = wellInfo.prop1.value;
elseif strcmp(wellInfo.prop1.quantity,'flowrate')
    well.consFlowrate = wellInfo.prop1.value;
end

well.wellID = wellInfo.list.value;

frac.fracID = unique(cumsum(any(frac.map>0,2)));
frac.nodeID = unique(frac.map);

frac.numAfrac = size(frac.fracID,1);
frac.numAnode = size(frac.nodeID,1);
well.numWnode = size(well.wellID,1);

qf = inpput.read(file6Name,'SI').list.value;

xtrue = inpput.read(file7Name,'SI').list.value;

Amat = zeros(frac.numAnode,frac.numAfrac+well.numWnode);

eq1_ID = (1:frac.numAnode)';

q1_ID = (1:frac.numAfrac)';
qw_ID = (1:well.numWnode)'+frac.numAfrac;

mass = massbalance(frac,well);

Amat(eq1_ID,q1_ID) = massbalance.arrayFracQ1(mass);
Amat(eq1_ID,qw_ID) = massbalance.arrayFracQw(mass);

bvec = -double(mass.fracQf)*qf;

xcalc = Amat\bvec;

err = norm(xcalc-xtrue); disp(err);