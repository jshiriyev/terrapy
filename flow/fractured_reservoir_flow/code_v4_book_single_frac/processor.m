function [res,frac,tau,q_well,p_well] = processor(res,frac,bhc,time)

% ---------------------------------------------------------------------- %
%   
%   solving system of linear equations
%
% ---------------------------------------------------------------------- %
    
    % calculation of hydraulic diffusivity in SI units
    
    res.xdiffusivity = res.xpermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    res.ydiffusivity = res.ypermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    res.zdiffusivity = res.zpermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    
    % calculation of fracture geometry in SI units
    
    Nf = size(frac.p1,1);
    
    frac.length = sqrt(sum((frac.p2-frac.p1).^2,2));
    
    frac.area = res.zlength*frac.length;
    
    frac.center(:,1) = (frac.p1(:,1)+frac.p2(:,1))/2;
    frac.center(:,2) = (frac.p1(:,2)+frac.p2(:,2))/2;
    frac.center(:,3) = res.zlength/2;
    
    % A and b matrix fill, x = A\b
    
    coeff_a = (res.fluid.viscosity)./(frac.permeability.*frac.width);
    coeff_b = 1/(2*res.porosity*res.totcompres*res.xlength);
    
    Nt = round(time.tt/time.dt);
    
    tau = linspace(time.dt,time.tt,Nt)';
    
    Green = greenfunc(frac.center,frac,tau,res);
    Gterm = coeff_b*Green*time.dt;
    
    c1 = 1:Nf-1;
    c2 = 1:Nf;
    c3 = 1:Nf+1;
    
    A11 = sparse([c2,c1],[c2,c1+1],[ones(1,Nf),-ones(1,Nf-1)],Nf,Nf);
    A12 = sparse(c2,c2,frac.length);
    A13 = sparse(Nf,Nf+1);
    
    A21 = sparse(c2,c2,coeff_a.*frac.length,Nf,Nf);
    A22 = sparse(c2,c2,coeff_a.*frac.length.^2/2);
    A23 = sparse([c2,c2],[c2,c2+1],[-ones(1,Nf),ones(1,Nf)],Nf,Nf+1);
    
    A31 = sparse(c2,c2,coeff_a.*frac.length/2,Nf,Nf);
    A32 = sparse(c2,c2,coeff_a.*frac.length.^2/8)-Gterm(:,:,1);
    A33 = sparse(c2,c2,-ones(1,Nf),Nf,Nf+1);
    
    A_4 = sparse(1,3*Nf+1);
    
    A = full([A11,A12,A13;A21,A22,A23;A31,A32,A33;A_4]);
    
    b11 = zeros(Nf,1);
    b21 = zeros(Nf,1);
    b31 = zeros(Nf,1)-res.initpressure;
    b_4 = zeros(1,1);
    
    b = [b11;b21;b31;b_4];
    
    % boundary conditions are applied only on b vector and we have
    % preconditioners
    
    A(:,1) = zeros(3*Nf+1,1);
    
    A(bhc.idx-1,1) = -1;
    
    if bhc.c
        A(3*Nf+1,1) = 1;
        b(3*Nf+1,1) = bhc.q/res.zlength*1e11;     % preconditioner
        A(3*Nf+1,:) = A(3*Nf+1,:)*1e11; % preconditioner
    else
        A(3*Nf+1,2*Nf+bhc.idx) = 1;
        b(3*Nf+1,1) = bhc.p;
    end
    
    A(c2,:) = A(c2,:)*1e11; % preconditioner
    
    % initialization of time dependent vectors
    
    bb = b;
    x = zeros(3*Nf+1,Nt);
    qfj = zeros(Nf,Nt);
    
    for t = 1:Nt
        
        x(:,t) = A\bb;
        
        qfj(:,t) = x(c2+Nf,t);
        
        if t ~= Nt
            qfjisfoft = permute(qfj(:,t:-1:1),[3,1,2]);
            integrand = sum(sum(qfjisfoft.*Gterm(:,:,2:t+1),3),2);
            bb(c2+2*Nf,1) = b(c2+2*Nf,1)+integrand;
        end
        
        disp(['Loading... ',num2str(t/Nt*100),'% is complete'])
        
    end
    
%     qj = x(c1,:);
    pj = x(c3+2*Nf,:);
    
    q_well = sum(qfj.*frac.length,1)'*res.zlength;
    p_well = pj(2,:)';
    
    % conversion of output to field units
    
    tau = tau/24/60/60;                 % conversion to [days]
    
    q_well = q_well*(6.29*24*60*60);    % conversion to [bbl/days]
    p_well = p_well/6894.76;            % conversion to [psi]
    
end