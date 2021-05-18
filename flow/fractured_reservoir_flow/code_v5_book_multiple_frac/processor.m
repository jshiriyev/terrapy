function [res,frac,time,sol] = processor(res,frac,bhc,time)

% ---------------------------------------------------------------------- %
%   
%   filling general matrices A and b
%   solving system of linear equations
%
% ---------------------------------------------------------------------- %
    
    % finding double neighbor nodes
    
    frac.dnodes = intersect(frac.map(:,1),frac.map(:,2));   % nodes both on p1 and p2
    frac.snodes = setdiff(frac.map(:,1),frac.map(:,2));     % nodes on p1 but not on p2
    
    % determining number of unknowns
    
    Nw = size(bhc.well,1);      % number of hole conditions applied node
    Nd = size(frac.dnodes,1);   % number of double neighbor nodes
    Nf = size(frac.map,1);      % number of fracture segments
    Nn = size(frac.nodes,1);    % number of nodes
    
    % coordinates of point 1 and 2 at each fracture segment
    
    frac.p1(:,1) = frac.nodes(frac.map(:,1),1);
    frac.p1(:,2) = frac.nodes(frac.map(:,1),2);
    frac.p2(:,1) = frac.nodes(frac.map(:,2),1);
    frac.p2(:,2) = frac.nodes(frac.map(:,2),2);
    
    % calculation of hydraulic diffusivity
    
    res.xdiffusivity = res.xpermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    res.ydiffusivity = res.ypermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    res.zdiffusivity = res.zpermeability/...
        (res.porosity*res.totcompres*res.fluid.viscosity);
    
    % calculation of fracture geometry
    
    frac.length = sqrt(sum((frac.p2-frac.p1).^2,2));
    
    frac.area = res.zlength*frac.length;
    
    frac.center(:,1) = (frac.p1(:,1)+frac.p2(:,1))/2;
    frac.center(:,2) = (frac.p1(:,2)+frac.p2(:,2))/2;
    frac.center(:,3) = res.zlength/2;
    
    % calculation of some constants
    
    cfA = (res.fluid.viscosity)./(frac.permeability.*frac.width);
    cfB = 1/(2*res.porosity*res.totcompres*res.xlength);
    
    cf1 = frac.length;
    cf2 = cfA.*frac.length;
    cf3 = cfA.*(frac.length.^2)/2;
    cf4 = cfA.*frac.length/2;
    cf5 = cfA.*(frac.length.^2)/8;
    
    % calculation of Green's function and the Green's term added to
    % the general matrix
    
    Ntime = round(time.tt/time.dt);                 % number of time steps
    
    time.tau = linspace(time.dt,time.tt,Ntime)';    % array of time steps
    
    green = greenfunc(frac.center,frac,time.tau,res);
    gterm = cfB*green*time.dt;
    
    % determination of indices to fill the matrices
    
    idx_w = 1:Nw;
    idx_d = 1:Nd;
    idx_f = 1:Nf;
    idx_n = 1:Nn;
    
    wn_1 = any(frac.snodes==bhc.well',1);   % wells on a node 1
    
    [idy1w,~] = find(frac.map(:,1)==bhc.well(wn_1,:)');
    [idy2w,~] = find(frac.map(:,2)==bhc.well(~wn_1,:)');
    
    [idy1d,~] = find(frac.map(:,1)==frac.dnodes');
    [idy2d,~] = find(frac.map(:,2)==frac.dnodes');
    
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
%         A11 = A11*1e11;         % preconditioner
%         b1 = b1*1e11;           % preconditioner
    end
    
    % 2) mass balance
    
    A21 = sparse([idy1w;idy2w],idx_w,wn_1-~wn_1,Nf,Nw);
    A22 = sparse([idy1d,idy2d],[idx_d,idx_d],[ones(Nd,1);-ones(Nd,1)],Nf,Nd);
    A23 = sparse(idx_f,idx_f,cf1,Nf,Nf);
    A24 = sparse(Nf,Nn);
    
%     A21 = A21*1e11;             % preconditioner
%     A22 = A22*1e11;             % preconditioner
%     A23 = A23*1e11;             % preconditioner
    
    b2 = sparse(Nf,1);
    
    % 3) fracture flow
    
    A31 = sparse(Nf,Nw);
    A32 = sparse(idy1d,idx_d,cf2(idy1d),Nf,Nd);
    A33 = sparse(idx_f,idx_f,cf3,Nf,Nf);
    A34 = sparse([idx_f,idx_f],frac.map,[-ones(Nf,1),ones(Nf,1)],Nf,Nn);
    b3 = sparse(Nf,1);
    
    % 4) pressure continuity
    
    A41 = sparse(Nf,Nw);
    A42 = sparse(idy1d,idx_d,cf4(idy1d),Nf,Nd);
    A43 = diag(cf5)-gterm(:,:,1);
    A44 = sparse(idx_f,frac.map(:,1),-ones(Nf,1),Nf,Nn);
    b4 = sparse(Nf,1)-res.initpressure;
    
    % merging all the matrices to the global matrix
    
    A = [A11,A12,A13,A14;A21,A22,A23,A24;...
         A31,A32,A33,A34;A41,A42,A43,A44];
    
    b = [b1;b2;b3;b4];
    
    % solving system of linear equations x = A\b
    
    idx_W = idx_w;
    idx_D = Nw+idx_d;
    idx_F = Nw+Nd+idx_f;
    idx_N = Nw+Nd+Nf+idx_n;
    
    x = zeros(Nw+Nd+Nf+Nn,Ntime);
    qf = zeros(Nf,Ntime);
    
    for t = 1:Ntime
        
        qf_t = permute(qf(:,t-1:-1:1),[3,1,2]);
        intg = sum(sum(qf_t.*gterm(:,:,2:t),3),2);
        
        b(Nw+2*Nf+idx_f,1) = b4+intg;
        sol.bvector(:,t) = b;
        x(:,t) = A\b;
        
        qf(:,t) = x(idx_F,t);
        
%         if t ~= Ntime
%             qf_t = permute(qf(:,t:-1:1),[3,1,2]);
%             intg = sum(sum(qf_t.*gterm(:,:,2:t+1),3),2);
%             b(Nw+2*Nf+idx_f,1) = b4+intg;
%         end
        
        disp(['Loading... ',num2str(t/Ntime*100),'% is complete'])
        
    end
    
    qw = x(idx_W,:)*res.zlength;
    qd = x(idx_D,:)*res.zlength;
    pn = x(idx_N,:);
    pw = pn(bhc.well,:);
    
    % conversion of output to field units
    
    time.tau = time.tau/24/60/60;           % conversion to [days]
    
    sol.qw = qw*(6.29*24*60*60);            % conversion to [bbl/days]
    sol.qd = qd*(6.29*24*60*60);            % conversion to [bbl/days]
    sol.qf = qf*(6.29*24*60*60);            % conversion to [bbl/days]
    sol.pn = pn/6894.76;                    % conversion to [psi]
    sol.pw = pw/6894.76;                    % conversion to [psi]
    
end