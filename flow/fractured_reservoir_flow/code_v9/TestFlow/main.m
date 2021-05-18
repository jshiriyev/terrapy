clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

qf = inpput.read([pwd,'\test_1\qf.txt'],'SI').list.value;

res = resControl([pwd,'\test_1\'],'SI');
frac = fracControl([pwd,'\test_1\'],'SI');
well = wellControl([pwd,'\test_1\'],'SI');

Amat = zeros(frac.numAnode+frac.numAfrac+well.numWnode);
bvec = zeros(frac.numAnode+frac.numAfrac+well.numWnode,1);

eq1_ID = (1:frac.numAnode)';
eq2_ID = (1:frac.numAfrac)'+frac.numAnode;
eq3_ID = (1:well.numWnode)'+frac.numAfrac+frac.numAnode;

q1_ID = (1:frac.numAfrac)';
qw_ID = (1:well.numWnode)'+frac.numAfrac;
pn_ID = (1:frac.numAnode)'+well.numWnode+frac.numAfrac;

mass = massbalance(frac,well);
flow = fracflow(frac);

Amat(eq1_ID,q1_ID) = massbalance.arrayFracQ1(mass);
Amat(eq1_ID,qw_ID) = massbalance.arrayFracQw(mass);

Amat(eq2_ID,q1_ID) = fracflow.arrayFlowQ1(flow,res,frac);
Amat(eq2_ID,pn_ID) = fracflow.arrayFlowPn(flow);

if ~isempty(well.consPressure)
    Amat(eq3_ID,pn_ID) = massbalance.arrayWellPn(mass);
    bvec(eq3_ID) = massbalance.vecWellPn(well);
elseif ~isempty(well.consFlowrate)
    Amat(eq3_ID,qw_ID) = massbalance.arrayWellQw(mass);
    bvec(eq3_ID) = massbalance.vecWellQw(res,well);
end

bvec(eq1_ID) = bvec(eq1_ID)-massbalance.arrayFracQf(mass,frac)*qf;
bvec(eq2_ID) = bvec(eq2_ID)-fracflow.arrayFlowQf(flow,res,frac)*qf;

x = Amat\bvec;

q1 = x(q1_ID)*res.zLength*(6.29*24*60*60);
qw = x(qw_ID)*res.zLength*(6.29*24*60*60);
pn = x(pn_ID)/6894.76;

plot(pn); hold on

P = well.consPressure+...
    res.fluidViscosity*(frac.Length(1)*frac.numAfrac/2)^2/2*...
    qf(1)/frac.permeability(1)/frac.width(1);

P = P/6894.76;

plot([1,11],[P,P],'o','MarkerFaceColor','r')