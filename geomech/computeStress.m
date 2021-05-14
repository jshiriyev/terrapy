function [snp1,de,dep] = computeStress(sn,dd,B)
    
    global m Cmat Nld Ey nu
    
    H=10;            % H is from Y=sY+H*ep
    sY=15;           % sY is from Y=sY+H*ep
    beta=0.5;        % beta is from f=sqrt(3/2)*S-beta*p-Y
    
    mu=Ey/(2*(1+nu));
    lame=Ey*nu/(1+nu)/(1-2*nu);
    K=lame+2/3*mu;
    
    de=B*transpose(reshape(transpose(dd),1,2*Nld));	% delta strain
    ded=de-1/3*(de(1)+de(2))*m;                     % delta dev. strain
    
    str=sn+Cmat*de;                                 % stress
    p=-1/3*(str(1)+str(2));                         % pressure
    Str=str+p*m;                                    % deviatoric stress
    
    S=sqrt((Str(1))^2+(Str(2))^2+2*(Str(3))^2);     % scalar
    
    T=Str/S;
    Qij=sqrt(6)/2*T+1/3*beta*m;
    Q=Qij/sqrt((Qij(1))^2+(Qij(2))^2+2*(Qij(3))^2);
    
    QT=Q(1)*T(1)+Q(2)*T(2)+2*Q(3)*T(3);
    dlambdaQ=(3*mu)/(H+3*mu*QT)*(ded*QT-beta*Q/(sqrt(6)*mu)*K*(de(1)+de(2)));
    dlambda=sqrt((dlambdaQ(1))^2+(dlambdaQ(2))^2+2*(dlambdaQ(3))^2);
    
    f=sqrt(3/2)*real(S)-beta*real(p)-sY;
    
    if f>=0
        snp1=sn+2*mu*(ded-dlambdaQ)+K*(de(1)+de(2))*m;
        dep=sqrt(2/3)*dlambda;
    else
        snp1=sn+2*mu*ded+K*(de(1)+de(2))*m;
        dep=0;
    end

end