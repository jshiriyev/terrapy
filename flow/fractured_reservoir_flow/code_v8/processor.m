function [res,frac,time,sol] = processor(res,frac,bhc,time)

% ---------------------------------------------------------------------- %
%   
%   filling general matrices A and b
%   solving system of linear equations
%
% ---------------------------------------------------------------------- %       
    
    % calculation of Green's function and the Green's term added to
    % the general matrix
    
    cfB = 1/(2*res.porosity*res.totcompres*res.xlength);
    
    Ntime = round(time.tt/time.dt);                 % number of time steps
    
    time.tau = linspace(time.dt,time.tt,Ntime)';    % array of time steps
    
    green = greenfunc(frac.center,frac,time.tau,res);
    gterm = cfB*green*time.dt;
    
    % determination of indices to fill the matrices
    
    idx_w = 1:Nw;
    idx_d = 1:Nd;
    idx_f = 1:Nf;
    idx_n = 1:Nn;
    
    wn_1 = any(frac.nodeOnJustPnt1==bhc.well',1);   % wells on a node 1
    
    [idy1w,~] = find(frac.map(:,1)==bhc.well(wn_1,:)');
    [idy2w,~] = find(frac.map(:,2)==bhc.well(~wn_1,:)');
    
    [idy1d,~] = find(frac.map(:,1)==frac.nodeOnBothEnds');
    [idy2d,~] = find(frac.map(:,2)==frac.nodeOnBothEnds');
    
    % filling the matrices
    
    % 1) implementation of wellbore conditions
    
    A12 = sparse(Nw,Nd);
    A13 = sparse(Nw,Nf);
    
    if bhc.type == 0            % constant bottom hole pressure
        A11 = sparse(Nw,Nw);
        A14 = sparse(idx_w,bhc.well,ones(Nw,1),Nw,Nn);
        b1 = sparse(idx_w,ones(Nw,1),ones(Nw,1)*bhc.cond,Nw,1);
    elseif bhc.type == 1        % constant flow rate
        A11 = sparse(idx_w,idx_w,ones(Nw,1),Nw,Nw);
        A14 = sparse(Nw,Nn);
        b1 = sparse(idx_w,ones(Nw,1),ones(Nw,1)*bhc.cond/res.zlength,Nw,1);
        A11 = A11*1e11;         % preconditioner
        b1 = b1*1e11;           % preconditioner
    end
    
    % 2) mass balance
    
    A21 = sparse([idy1w;idy2w],idx_w,wn_1-~wn_1,Nf,Nw);
    A22 = sparse([idy1d,idy2d],[idx_d,idx_d],[ones(Nd,1);-ones(Nd,1)],Nf,Nd);
    A23 = sparse(idx_f,idx_f,frac.length,Nf,Nf);
    A24 = sparse(Nf,Nn);
    
    A21 = A21*1e11;             % preconditioner
    A22 = A22*1e11;             % preconditioner
    A23 = A23*1e11;             % preconditioner
    
    b2 = sparse(Nf,1);
    
    % 3) fracture flow
    
    % 4) pressure continuity
    
    A41 = sparse(Nf,Nw);
    A44 = sparse(idx_f,frac.map(:,1),-ones(Nf,1),Nf,Nn);
    b4 = sparse(Nf,1)-res.initpressure;
    
    % solving system of linear equations x = A\b
    
    idx_W = idx_w;
    idx_D = Nw+idx_d;
    idx_F = Nw+Nd+idx_f;
    idx_N = Nw+Nd+Nf+idx_n;
    
    sol.qw = zeros(Nw,Ntime);
    sol.qd = zeros(Nd,Ntime);
    sol.qf = zeros(Nf,Ntime);
    sol.pn = zeros(Nn,Ntime);
    sol.pm = zeros(Nf,Ntime);
    sol.fc = zeros(Nf,Ntime);
    
    b = [b1;b2;b3;b4];
    
    sigma_hormin = 5345;     % Wanrning!!! [psi]
    
    for t = 1:Ntime
        
        cfA = (res.fluid.viscosity)./FC;
        
        cf4 = cfA.*frac.length/2;
        cf5 = cfA.*(frac.length.^2)/8;
        
        A42 = sparse(idy1d,idx_d,cf4(idy1d),Nf,Nd);
        A43 = diag(cf5)-gterm(:,:,1);
        
        A = [A11,A12,A13,A14;A21,A22,A23,A24;...
             A31,A32,A33,A34;A41,A42,A43,A44];
        
        x = A\b;
        
        sol.qw(:,t) = x(idx_W);
        sol.qd(:,t) = x(idx_D);
        sol.qf(:,t) = x(idx_F);
        sol.pn(:,t) = x(idx_N);
        
        qn = zeros(Nn,1);
        qn(frac.nodeOnBothEnds) = sol.qd(:,t);
        
        sol.pm(:,t) = sol.pn(frac.map(:,1),t)-...
                  cfA.*(qn(frac.map(:,1)).*frac.length/2+...
                        sol.qf(:,t).*frac.length.^2/6);
        
        sigma_closure = sigma_hormin-sol.pm(:,t)/6894.76;       % Wanrning!!! [psi]
        
        norm_fraccond = power(10,-0.0004*sigma_closure+0.2191);
        
%         FC = FC.*norm_fraccond;
        
        if t ~= Ntime
            qf_t = permute(sol.qf(:,t:-1:1),[3,1,2]);
            intg = sum(sum(qf_t.*gterm(:,:,2:t+1),3),2);
            b(Nw+2*Nf+idx_f,1) = b4+intg;
        end
        
        disp(['Loading... ',num2str(t/Ntime*100),'% is complete'])
        
        sol.fc(:,t) = FC;
        
    end
    
    sol.pw = sol.pn(bhc.well,:);
    
    % conversion of output to field units
    
    time.tau = time.tau/24/60/60;               % conversion to [days]
    
    sol.qw = qw*(6.29*24*60*60)*res.zlength;    % conversion to [bbl/days]
    sol.qd = qd*(6.29*24*60*60)*res.zlength;    % conversion to [bbl/days]
    sol.qf = qf*(6.29*24*60*60);                % conversion to [bbl/days]
    sol.pn = pn/6894.76;                        % conversion to [psi]
    sol.pw = pw/6894.76;                        % conversion to [psi]
    sol.pm = pm/6894.76;                      
    
end