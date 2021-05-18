clear
close all
clc

addpath('C:\Users\js68897\repos\greensfrac')

res = resControl(pwd,'SI');
frac = fracControl(pwd,'SI');
well = wellControl(pwd,'SI');
time = numControl(pwd,'SI');

% prop.Color = 'k';
% resultPlot.frac(frac,prop)
% xlim([0,res.xLength]);
% ylim([0,res.yLength]);

Gplane = green.plane(frac.center,frac,time.tau,res,'xz');

% B = frac.volume.*frac.porosity*res.oilCompressibility/res.oilFVF;

T = frac.permeability.*frac.areatofracture/...
    res.oilViscosity/res.oilFVF./frac.Length;

req = 0.14*sqrt(frac.Length(well.wellID).^2+res.zLength^2);

J = 2*pi*frac.width(well.wellID)*frac.permeability(well.wellID)/...
    (res.oilViscosity*res.oilFVF*log(req/well.radius));

idx = 1:frac.numAfrac;
idy = 1:(frac.numAfrac-1);

I = sparse(idx,idx,1,frac.numAfrac,frac.numAfrac);

% Bmat = sparse(idx,idx,B,frac.numAfrac,frac.numAfrac);

Jmat = sparse(well.wellID,well.wellID,J,frac.numAfrac,frac.numAfrac);

Tmat = sparse(idx,idx,2*T,frac.numAfrac,frac.numAfrac);

Tmat = Tmat+sparse(idy,idy+1,-T(2:end),frac.numAfrac,frac.numAfrac);
Tmat = Tmat+sparse(idy+1,idy,-T(1:end-1),frac.numAfrac,frac.numAfrac);

Tmat(1,1) = T(1);                             % left side no-flow boundary condition
Tmat(frac.numAfrac,frac.numAfrac) = T(end);     % right side no-flow boundary condition

Q = sparse(frac.numAfrac,1);
% Q(1) = 2*T(1)*1500*inpput.convFactorDetermine('pressure');
Q(well.wellID) = J*well.consPressure;

% Qf = zeros(frac.numAfrac,1)+1e-4;

% sol.pressure = (Tmat+Jmat)\(Q+Qf);
Pinit = zeros(frac.numAfrac,1)+res.initPressure;

Af = diag(frac.areatoreservoir);

A = Af+(Tmat+Jmat)*Gplane(:,:,1)*time.deltaTime/2;
b = (Tmat+Jmat)*Pinit-Q;

Qf1 = A\b;

P1 = (Tmat+Jmat)\(Q+Af*Qf1);

pressure = P1/inpput.convFactorDetermine('pressure');
plot(pressure)


sol.fracflux = zeros(frac.numAfrac,time.numTimeStep+1);
sol.pressure = zeros(frac.numAfrac,time.numTimeStep+1);
sol.pressure(:,1) = res.initPressure;

for i = 1:time.numTimeStep
    
    summ1 = zeros(frac.numAfrac,1);
    
    for j = 1:i
        summ1 = summ1+Gplane(:,:,i)*sol.fracflux(:,i-j+1);
    end

    sol.fracflux(:,i+1) = -Bmat/time.deltaTime*sol.pressure(:,i)-Q+...
     (Tmat+Jmat+Bmat/time.deltaTime)*(res.initPressure-summ1*time.deltaTime);

    sol.fracflux(:,i+1) = sol.fracflux(:,i+1)./frac.areatoreservoir;
    
    summ2 = zeros(frac.numAfrac,1);
    
    for j = 1:i
        summ2 = summ2+Gplane(:,:,i)*sol.fracflux(:,i-j+1);
    end
    
    sol.pressure(:,i+1) = res.initPressure-summ2*time.deltaTime;

end

% sol.fracflux(:,1) = -1e-5;
% sol.pressure(:,2) = (Tmat+Jmat+1/time.deltaTime*Bmat)\(1/time.deltaTime*Bmat*sol.pressure(:,1)+Q+sol.fracflux(:,1));

pressure = sol.pressure(:,2)/inpput.convFactorDetermine('pressure');
plot(pressure)


% figure(1)
% 
% plot(frac.center.Xcoord,pressureFU); hold on
% xlim([min(frac.nodeCoord(:,1)),max(frac.nodeCoord(:,1))]);
% 
% % Ntime = round(sol.time/sol.deltatime);
% % 
% % for i = 1:Ntime
% %     sol.pressure = (I+alfa*A)\sol.pressure;
% % end

