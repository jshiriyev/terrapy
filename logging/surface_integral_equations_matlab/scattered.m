function [Hsca] = scattered(k1,order,Tmn,string)
    
    global VtoR DtoT TtoD1 TtoV NofD NofT RofCp RofCm rhocp rhocm lofD
    
    R1 = VtoR(TtoV(:,1),:);
    R2 = VtoR(TtoV(:,2),:);
    R3 = VtoR(TtoV(:,3),:);
    
    [Rn,Wn,Pn] = gauss(R1,R2,R3,order);
    
    Lpn = zeros(NofD,3,Pn);
    Lmn = zeros(NofD,3,Pn);
    
    for k = 1:Pn
        rhop = Rn(DtoT(:,1),:,k)-VtoR(DtoT(:,3),:);
        rhom = VtoR(DtoT(:,4),:)-Rn(DtoT(:,2),:,k);
        Lpn(:,1,k) = lofD.*rhop(:,1)/2;
        Lpn(:,2,k) = lofD.*rhop(:,2)/2;
        Lpn(:,3,k) = lofD.*rhop(:,3)/2;
        Lmn(:,1,k) = lofD.*rhom(:,1)/2;
        Lmn(:,2,k) = lofD.*rhom(:,2)/2;
        Lmn(:,3,k) = lofD.*rhom(:,3)/2;
    end
    
    sampling = load(strcat(string,'sampling.txt'));
    
    RofS = sampling(:,1:3);
    RofO1 = sampling(:,4:6);
    RofO2 = sampling(:,7:9);
    
    alfa = (sum((RofO1-RofS).^2,2)./sum((RofO2-RofS).^2,2)).^(3/2);
    
    Nosp = size(RofS,1);
    Hsca = zeros(Nosp,3);
    
    [TLmn,TUmn] = lu(Tmn);
    
    for i = 1:Nosp
        
        Ep = Einc(k1,RofCp,RofS(i,:));
        Em = Einc(k1,RofCm,RofS(i,:));
        Vm = sum(rhocp.*Ep+rhocm.*Em,2)/2;

        In = TUmn\(TLmn\Vm);
        
        Jd = zeros(NofT,3,Pn);
        
        LIpn = zeros(NofD+1,3,Pn);
        LImn = zeros(NofD+1,3,Pn);
        
        for k = 1:Pn
            LIpn(1:NofD,1,k) = Lpn(:,1,k).*In;
            LIpn(1:NofD,2,k) = Lpn(:,2,k).*In;
            LIpn(1:NofD,3,k) = Lpn(:,3,k).*In;
            LImn(1:NofD,1,k) = Lmn(:,1,k).*In;
            LImn(1:NofD,2,k) = Lmn(:,2,k).*In;
            LImn(1:NofD,3,k) = Lmn(:,3,k).*In;
        end
        
        Jd = Jd+LIpn(TtoD1(:,1),:,:)+LImn(TtoD1(:,4),:,:);
        Jd = Jd+LIpn(TtoD1(:,2),:,:)+LImn(TtoD1(:,5),:,:);
        Jd = Jd+LIpn(TtoD1(:,3),:,:)+LImn(TtoD1(:,6),:,:);
        
        GG1 = green2(k1,RofO1(i,:),Rn);
        GG2 = green2(k1,RofO2(i,:),Rn);
        
        G1J = permute(sum(cross(GG1,Jd),1),[3 2 1]);
        G2J = permute(sum(cross(GG2,Jd),1),[3 2 1]);
        
        Hsca1(1,1) = sum(G1J(:,1).*Wn,1);
        Hsca1(1,2) = sum(G1J(:,2).*Wn,1);
        Hsca1(1,3) = sum(G1J(:,3).*Wn,1);
        Hsca2(1,1) = sum(G2J(:,1).*Wn,1);
        Hsca2(1,2) = sum(G2J(:,2).*Wn,1);
        Hsca2(1,3) = sum(G2J(:,3).*Wn,1);
        
        Hsca(i,:) = Hsca2-Hsca1*alfa(i);
        
        display(['Rx is at ' num2str(mean([RofO1(i,3),RofO2(i,3)])) 'm'])

    end
    
end

function GG = green2(k1,R1,R2)
    
    % R1 is the observer
    % R2 is the source point
    % R is the distance between R1 and R2
    % GG is the gradient of Green's function
    
    global NofT
    
    Pn = size(R2,3);
    
    GG = zeros(NofT,3,Pn);
    
    for k = 1:Pn
        
        r(:,1) = R1(1,1)-R2(:,1,k);
        r(:,2) = R1(1,2)-R2(:,2,k);
        r(:,3) = R1(1,3)-R2(:,3,k);
        
        R = sqrt(sum(r.*r,2));
    
        G = exp(-1j*k1*R)./(4*pi*R);

        const = -(1+1j*k1*R).*G./R.^2;

        GG(:,1,k) = r(:,1).*const;
        GG(:,2,k) = r(:,2).*const;
        GG(:,3,k) = r(:,3).*const;
        
    end
    
end
