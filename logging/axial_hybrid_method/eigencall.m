function [Cmat,Lambda] = eigencall(stat)
    
    global Nr Nb B1 B2
    
    Cmat = zeros(2*Nb,2*Nb,Nr);

    Lambda = zeros(2*Nb,Nr);

    for l = 1:Nr

        A1e = integral(-1./stat.qe(:,l),stat.qe(:,l),stat.qe(:,l),3);
        A1h = integral(-1./stat.qh(:,l),stat.qh(:,l),stat.qh(:,l),3);

        A2e = integral(stat.k2e(:,l)./stat.pe(:,l),stat.qe(:,l),stat.qe(:,l),1);
        A2h = integral(stat.k2h(:,l)./stat.ph(:,l),stat.qh(:,l),stat.qh(:,l),1);

        Be = integral(1./stat.pe(:,l),stat.qe(:,l),stat.qe(:,l),1);
        Bh = integral(1./stat.ph(:,l),stat.qh(:,l),stat.qh(:,l),1);

        Ae = A1e+A2e;
        Ah = A1h+A2h;

        [CCe,DDe] = eig(Ae,Be,'vector');
        [CCh,DDh] = eig(Ah,Bh,'vector');

        Cmat(B1,B1,l) = orthog(CCe,Be);
        Cmat(B2,B2,l) = orthog(CCh,Bh);

        Lambda(B1,l) = sqrt(DDe);
        Lambda(B2,l) = sqrt(DDh);

    end
    
end

function Ceta = orthog(Ceta_old,Beta)
    
    %   Correction for orthogonality relationship
    
    global Nb
    
    Ceta = Ceta_old;
    
    noneI = transpose(Ceta_old)*Beta*Ceta_old;
    noneI = sqrt(1./diag(noneI));
    
    for i = 1:Nb
        Ceta(:,i) = Ceta_old(:,i)*noneI(i);
    end
    
end