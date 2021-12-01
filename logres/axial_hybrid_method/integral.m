function GG = integral(cx,qx1,qx2,flag)

%     Calculates Integrals Analytically
%
%     g1 = -2*L1^3+3*L1^2;
%     g2 = qx1*dz*L1^2*L2;
%     g3 = -2*L2^3+3*L2^2;
%     g4 = -qx1*dz*L2^2*L1;
%
%     g1d = 1/dz*(6*L1^2-6*L1);
%     g2d = qx2*(-2*L1*L2+L1^2);
%     g3d = 1/dz*(-6*L2^2+6*L2);
%     g4d = -qx2*(2*L2*L1-L2^2);
    
    global Ne

    gg = zeros(Ne,4,4);
    
    if flag == 1
        gg = gxgx(cx,qx1,qx2);
    elseif flag == 2
        gg = gxgxd(cx,qx1,qx2);
    elseif flag == 3
        gg = gxdgxd(cx,qx1,qx2);
    end
    
    GG = assembly(gg);

end

function gg = gxgx(cx,qx1,qx2)
    
    global dz

    gg(:,1,1) = 13/35*dz.*cx;
    gg(:,1,2) = 11/210*qx2.*dz.*dz.*cx;
    gg(:,1,3) = 9/70*dz.*cx;
    gg(:,1,4) = -13/420*qx2.*dz.*dz.*cx;
    
    gg(:,2,1) = 11/210*qx1.*dz.*dz.*cx;
    gg(:,2,2) = 1/105*qx1.*dz.*qx2.*dz.*dz.*cx;
    gg(:,2,3) = 13/420*qx1.*dz.*dz.*cx;
    gg(:,2,4) = -1/140*qx1.*dz.*qx2.*dz.*dz.*cx;
    
    gg(:,3,1) = 9/70*dz.*cx;
    gg(:,3,2) = 13/420*qx2.*dz.*dz.*cx;
    gg(:,3,3) = 13/35*dz.*cx;
    gg(:,3,4) = -11/210*qx2.*dz.*dz.*cx;
    
    gg(:,4,1) = -13/420*qx1.*dz.*dz.*cx;
    gg(:,4,2) = -1/140*qx1.*dz.*qx2.*dz.*dz.*cx;
    gg(:,4,3) = -11/210*qx1.*dz.*dz.*cx;
    gg(:,4,4) = 1/105*qx1.*dz.*qx2.*dz.*dz.*cx;
    
end

function gg = gxgxd(cx,qx1,qx2)
    
    global dz Ne
    
    gg(:,1,1) = -1/2*cx;
    gg(:,1,2) = 1/10*qx2.*dz.*cx;
    gg(:,1,3) = 1/2*cx;
    gg(:,1,4) = -1/10*qx2.*dz.*cx;
    
    gg(:,2,1) = -1/10*qx1.*dz.*cx;
    gg(:,2,2) = zeros(Ne,1);
    gg(:,2,3) = 1/10*qx1.*dz.*cx;
    gg(:,2,4) = -1/60*qx1.*qx2.*dz.*dz.*cx;
    
    gg(:,3,1) = -1/2*cx;
    gg(:,3,2) = -1/10*qx2.*dz.*cx;
    gg(:,3,3) = 1/2*cx;
    gg(:,3,4) = 1/10*qx2.*dz.*cx;
    
    gg(:,4,1) = 1/10*qx1.*dz.*cx;
    gg(:,4,2) = 1/60*qx1.*qx2.*dz.*dz.*cx;
    gg(:,4,3) = -1/10*qx1.*dz.*cx;
    gg(:,4,4) = zeros(Ne,1);
    
end

function gg = gxdgxd(cx,qx1,qx2)
    
    global dz
    
    gg(:,1,1) = 6/5*cx./dz;
    gg(:,1,2) = 1/10*qx2.*cx;
    gg(:,1,3) = -6/5*cx./dz;
    gg(:,1,4) = 1/10*qx2.*cx;
    
    gg(:,2,1) = 1/10*qx1.*cx;
    gg(:,2,2) = 2/15*qx1.*qx2.*dz.*cx;
    gg(:,2,3) = -1/10*qx1.*cx;
    gg(:,2,4) = -1/30*qx1.*qx2.*dz.*cx;
    
    gg(:,3,1) = -6/5*cx./dz;
    gg(:,3,2) = -1/10*qx2.*cx;
    gg(:,3,3) = 6/5*cx./dz;
    gg(:,3,4) = -1/10*qx2.*cx;
    
    gg(:,4,1) = 1/10*qx1.*cx;
    gg(:,4,2) = -1/30*qx1.*qx2.*dz.*cx;
    gg(:,4,3) = -1/10*qx1.*cx;
    gg(:,4,4) = 2/15*qx1.*qx2.*dz.*cx;
    
end

function GG = assembly(gg)
    
    global Ne Nb

    GG = sparse(Nb,Nb);
    
    idx1 = 1:2:Nb;
    idx2 = 2:2:Nb;
    
    GG = GG+sparse(idx1,idx1,gg(1:Ne-1,3,3)+gg(2:Ne,1,1),Nb,Nb);
    GG = GG+sparse(idx1,idx2,gg(1:Ne-1,3,4)+gg(2:Ne,1,2),Nb,Nb);
    GG = GG+sparse(idx2,idx1,gg(1:Ne-1,4,3)+gg(2:Ne,2,1),Nb,Nb);
    GG = GG+sparse(idx2,idx2,gg(1:Ne-1,4,4)+gg(2:Ne,2,2),Nb,Nb);
    
    GG = GG+sparse(idx1(2:Ne-1),idx1(1:Ne-2),gg(2:Ne-1,3,1),Nb,Nb);
    GG = GG+sparse(idx1(2:Ne-1),idx2(1:Ne-2),gg(2:Ne-1,3,2),Nb,Nb);
    
    GG = GG+sparse(idx1(1:Ne-2),idx1(2:Ne-1),gg(2:Ne-1,1,3),Nb,Nb);
    GG = GG+sparse(idx1(1:Ne-2),idx2(2:Ne-1),gg(2:Ne-1,1,4),Nb,Nb);
    
    GG = GG+sparse(idx2(2:Ne-1),idx1(1:Ne-2),gg(2:Ne-1,4,1),Nb,Nb);
    GG = GG+sparse(idx2(2:Ne-1),idx2(1:Ne-2),gg(2:Ne-1,4,2),Nb,Nb);
    
    GG = GG+sparse(idx2(1:Ne-2),idx1(2:Ne-1),gg(2:Ne-1,2,3),Nb,Nb);
    GG = GG+sparse(idx2(1:Ne-2),idx2(2:Ne-1),gg(2:Ne-1,2,4),Nb,Nb);
    
    GG = full(GG);
    
end