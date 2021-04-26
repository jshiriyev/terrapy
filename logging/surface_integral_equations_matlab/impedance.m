function [Zmn,Bmn] = impedance(k1,order)
    
    global VtoR DtoT TtoD TtoD1 TtoV NofD NofT RofC rhocp rhocm lofD AofT GofT
    
    R1 = VtoR(TtoV(:,1),:);
    R2 = VtoR(TtoV(:,2),:);
    R3 = VtoR(TtoV(:,3),:);
    
    [Rn,Wn,Pn] = gauss(R1,R2,R3,order);
    
    Lpn = zeros(NofD,3,Pn);
    Lmn = zeros(NofD,3,Pn);
    Gmn = zeros(NofT,NofT,Pn);
    
    for k = 1:Pn
        rhop = Rn(DtoT(:,1),:,k)-VtoR(DtoT(:,3),:);
        rhom = VtoR(DtoT(:,4),:)-Rn(DtoT(:,2),:,k);
        Lpn(:,1,k) = lofD.*rhop(:,1)/2;
        Lpn(:,2,k) = lofD.*rhop(:,2)/2;
        Lpn(:,3,k) = lofD.*rhop(:,3)/2;
        Lmn(:,1,k) = lofD.*rhom(:,1)/2;
        Lmn(:,2,k) = lofD.*rhom(:,2)/2;
        Lmn(:,3,k) = lofD.*rhom(:,3)/2;
        Gmn(:,:,k) = green1(k1,RofC,Rn(:,:,k));
    end
    
    Zmn = zeros(NofD,NofD);
    
    for n = 1:NofD
        Pp = zeros(NofD,1);
        Pm = zeros(NofD,1);
        Ap = zeros(NofD,3);
        Am = zeros(NofD,3);
        for k = 1:Pn
            Gpp = Gmn(DtoT(:,1),DtoT(n,1),k);
            Gmp = Gmn(DtoT(:,2),DtoT(n,1),k);
            Gpm = Gmn(DtoT(:,1),DtoT(n,2),k);
            Gmm = Gmn(DtoT(:,2),DtoT(n,2),k);
            Pp = Pp+lofD(n)*(Gpp-Gpm)*Wn(k);
            Pm = Pm+lofD(n)*(Gmp-Gmm)*Wn(k);
            Ap(:,1) = Ap(:,1)+(Lpn(n,1,k)*Gpp+Lmn(n,1,k)*Gpm)*Wn(k);
            Ap(:,2) = Ap(:,2)+(Lpn(n,2,k)*Gpp+Lmn(n,2,k)*Gpm)*Wn(k);
            Ap(:,3) = Ap(:,3)+(Lpn(n,3,k)*Gpp+Lmn(n,3,k)*Gpm)*Wn(k);
            Am(:,1) = Am(:,1)+(Lpn(n,1,k)*Gmp+Lmn(n,1,k)*Gmm)*Wn(k);
            Am(:,2) = Am(:,2)+(Lpn(n,2,k)*Gmp+Lmn(n,2,k)*Gmm)*Wn(k);
            Am(:,3) = Am(:,3)+(Lpn(n,3,k)*Gmp+Lmn(n,3,k)*Gmm)*Wn(k);
        end
        Zmn(:,n) = (sum(Ap.*rhocp+Am.*rhocm,2))/2+(Pm-Pp)/k1^2;
    end
    
    Lcpn = zeros(NofD+1,3);
    Lcmn = zeros(NofD+1,3);

    Lcpn(1:NofD,1) = lofD.*rhocp(:,1)/2./AofT(DtoT(:,1))./GofT(DtoT(:,1));
    Lcpn(1:NofD,2) = lofD.*rhocp(:,2)/2./AofT(DtoT(:,1))./GofT(DtoT(:,1));
    Lcpn(1:NofD,3) = lofD.*rhocp(:,3)/2./AofT(DtoT(:,1))./GofT(DtoT(:,1));
    Lcmn(1:NofD,1) = lofD.*rhocm(:,1)/2./AofT(DtoT(:,2))./GofT(DtoT(:,2));
    Lcmn(1:NofD,2) = lofD.*rhocm(:,2)/2./AofT(DtoT(:,2))./GofT(DtoT(:,2));
    Lcmn(1:NofD,3) = lofD.*rhocm(:,3)/2./AofT(DtoT(:,2))./GofT(DtoT(:,2));
    
    Lrp1 = rhocp.*(Lcpn(TtoD1(DtoT(:,1),1),:)+Lcmn(TtoD1(DtoT(:,1),4),:));
    Lrp2 = rhocp.*(Lcpn(TtoD1(DtoT(:,1),2),:)+Lcmn(TtoD1(DtoT(:,1),5),:));
    Lrp3 = rhocp.*(Lcpn(TtoD1(DtoT(:,1),3),:)+Lcmn(TtoD1(DtoT(:,1),6),:));
    Lrm1 = rhocm.*(Lcpn(TtoD1(DtoT(:,2),1),:)+Lcmn(TtoD1(DtoT(:,2),4),:));
    Lrm2 = rhocm.*(Lcpn(TtoD1(DtoT(:,2),2),:)+Lcmn(TtoD1(DtoT(:,2),5),:));
    Lrm3 = rhocm.*(Lcpn(TtoD1(DtoT(:,2),3),:)+Lcmn(TtoD1(DtoT(:,2),6),:));
    
    Bmn = spalloc(NofD,NofD+1,5*NofD);
    
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,1),1),sum(Lrp1,2)/2,NofD,NofD+1);
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,1),2),sum(Lrp2,2)/2,NofD,NofD+1);
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,1),3),sum(Lrp3,2)/2,NofD,NofD+1);
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,2),1),sum(Lrm1,2)/2,NofD,NofD+1);
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,2),2),sum(Lrm2,2)/2,NofD,NofD+1);
    Bmn = Bmn+sparse(1:NofD,TtoD(DtoT(:,2),3),sum(Lrm3,2)/2,NofD,NofD+1);
    
    Bmn(:,NofD+1) = [];
    
    display('impedance and boundary matrix is constructed')
    
end

function G = green1(k1,R1,R2)
    
    % R1 is the observer
    % R2 is the source point
    % R is the distance between R1 and R2
    
    global NofT
    
    R = zeros(NofT);
    
    for j = 1:NofT
        r(:,1) = R1(:,1)-R2(j,1);
        r(:,2) = R1(:,2)-R2(j,2);
        r(:,3) = R1(:,3)-R2(j,3);
        R(:,j) = sqrt(sum(r.*r,2));
    end
    
    G = exp(-1j*k1*R)./(4*pi*R);
    
end