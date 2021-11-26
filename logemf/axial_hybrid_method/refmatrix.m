function [Q,Hc0p,Jc0p] = refmatrix(stat,Cmat,Lambda)
    
    global Nr Nb B1 B2

    P1p = zeros(2*Nb,2*Nb,Nr-1);
    P1m = zeros(2*Nb,2*Nb,Nr-1);
    P2p = zeros(2*Nb,2*Nb,Nr-1);
    P2m = zeros(2*Nb,2*Nb,Nr-1);

    Dhep = zeros(Nb,Nb,Nr-1);
    Dehp = zeros(Nb,Nb,Nr-1);

    Dhem = zeros(Nb,Nb,Nr-1);
    Dehm = zeros(Nb,Nb,Nr-1);
    
    for l = 1:Nr-1
        
        pe1p = integral(1./stat.pe(:,l+1),stat.qe(:,l),stat.qe(:,l+1),1);
        P1p(B1,B1,l) = transpose(Cmat(B1,B1,l))*pe1p*Cmat(B1,B1,l+1);
        ph1p = integral(1./stat.ph(:,l+1),stat.qh(:,l),stat.qh(:,l+1),1);
        P1p(B2,B2,l) = transpose(Cmat(B2,B2,l))*ph1p*Cmat(B2,B2,l+1);

        pe1m = integral(1./stat.pe(:,l),stat.qe(:,l+1),stat.qe(:,l),1);
        P1m(B1,B1,l) = transpose(Cmat(B1,B1,l+1))*pe1m*Cmat(B1,B1,l);
        ph1m = integral(1./stat.ph(:,l),stat.qh(:,l+1),stat.qh(:,l),1);
        P1m(B2,B2,l) = transpose(Cmat(B2,B2,l+1))*ph1m*Cmat(B2,B2,l);

        pe2p = integral(1./stat.pe(:,l),stat.qe(:,l),stat.qe(:,l+1),1);
        P2p(B1,B1,l) = transpose(Cmat(B1,B1,l))*pe2p*Cmat(B1,B1,l+1);
        ph2p = integral(1./stat.ph(:,l),stat.qh(:,l),stat.qh(:,l+1),1);
        P2p(B2,B2,l) = transpose(Cmat(B2,B2,l))*ph2p*Cmat(B2,B2,l+1);

        pe2m = integral(1./stat.pe(:,l+1),stat.qe(:,l+1),stat.qe(:,l),1);
        P2m(B1,B1,l) = transpose(Cmat(B1,B1,l+1))*pe2m*Cmat(B1,B1,l);
        ph2m = integral(1./stat.ph(:,l+1),stat.qh(:,l+1),stat.qh(:,l),1);
        P2m(B2,B2,l) = transpose(Cmat(B2,B2,l+1))*ph2m*Cmat(B2,B2,l);

        dhep = integral(1./stat.ph(:,l)./stat.qe(:,l+1),stat.qh(:,l),stat.qe(:,l+1),2);
        Dhep(:,:,l) = transpose(Cmat(B2,B2,l))*dhep*Cmat(B1,B1,l+1);
        dehp = integral(1./stat.pe(:,l)./stat.qh(:,l+1),stat.qe(:,l),stat.qh(:,l+1),2);
        Dehp(:,:,l) = transpose(Cmat(B1,B1,l))*dehp*Cmat(B2,B2,l+1);

        dhem = integral(1./stat.ph(:,l+1)./stat.qe(:,l),stat.qh(:,l+1),stat.qe(:,l),2);
        Dhem(:,:,l) = transpose(Cmat(B2,B2,l+1))*dhem*Cmat(B1,B1,l);
        dehm = integral(1./stat.pe(:,l+1)./stat.qh(:,l),stat.qe(:,l+1),stat.qh(:,l),2);
        Dehm(:,:,l) = transpose(Cmat(B1,B1,l+1))*dehm*Cmat(B2,B2,l);
        
    end
    
    Dhe = zeros(Nb,Nb,Nr);
    Deh = zeros(Nb,Nb,Nr);

    for l = 1:Nr

        dhe = integral(1./stat.ph(:,l)./stat.qe(:,l),stat.qh(:,l),stat.qe(:,l),2);
        Dhe(:,:,l) = transpose(Cmat(B2,B2,l))*dhe*Cmat(B1,B1,l);
        deh = integral(1./stat.pe(:,l)./stat.qh(:,l),stat.qe(:,l),stat.qh(:,l),2);
        Deh(:,:,l) = transpose(Cmat(B1,B1,l))*deh*Cmat(B2,B2,l);

    end
    
    display('done with static part')
    
    global muo bnd_r omega nord Nord
    
    Hc0m = zeros(2*Nb,Nr-1,Nord);
    Jc0m = zeros(2*Nb,Nr-1,Nord);
    Hc0p = zeros(2*Nb,Nr-1,Nord);
    Jc0p = zeros(2*Nb,Nr-1,Nord);

    Hc1m = zeros(2*Nb,Nr-1,Nord);
    Jc1m = zeros(2*Nb,Nr-1,Nord);
    Hc1p = zeros(2*Nb,Nr-1,Nord);
    Jc1p = zeros(2*Nb,Nr-1,Nord);

    for l = 1:Nr-1

        Lm = Lambda(:,l+1)*bnd_r(l);
        Lp = Lambda(:,l)*bnd_r(l);
        
        Am = imag(Lm)<0;
        Ap = imag(Lp)<0;

        for i = 1:Nord
            Hc0m(:,l,i) = besselh(nord(i),1,Lm,1);
            Jc0m(:,l,i) = besselj(nord(i),Lm,1).*exp(1j*real(Lm));
            Jc0m(Am,l,i) = Jc0m(Am,l,i).*exp(-2*imag(Lm(Am)));
            Hc0p(:,l,i) = besselh(nord(i),1,Lp,1);
            Jc0p(:,l,i) = besselj(nord(i),Lp,1).*exp(1j*real(Lp));
            Jc0p(Ap,l,i) = Jc0p(Ap,l,i).*exp(-2*imag(Lp(Ap)));
        end

        for i = 1:Nord
            if i == 1
                Left1 = besselh(nord(i)-1,1,Lm,1);
                Left2 = besselj(nord(i)-1,Lm,1).*exp(1j*real(Lm));
                Left2(Am) = Left2(Am).*exp(-2*imag(Lm(Am)));
                Left3 = besselh(nord(i)-1,1,Lp,1);
                Left4 = besselj(nord(i)-1,Lp,1).*exp(1j*real(Lp));
                Left4(Ap) = Left4(Ap).*exp(-2*imag(Lp(Ap)));
            else
                Left1 = Hc0m(:,l,i-1);
                Left2 = Jc0m(:,l,i-1);
                Left3 = Hc0p(:,l,i-1);
                Left4 = Jc0p(:,l,i-1);
            end
            if i == Nord
                Right1 = besselh(nord(i)+1,1,Lm,1);
                Right2 = besselj(nord(i)+1,Lm,1).*exp(1j*real(Lm));
                Right2(Am) = Right2(Am).*exp(-2*imag(Lm(Am)));
                Right3 = besselh(nord(i)+1,1,Lp,1);
                Right4 = besselj(nord(i)+1,Lp,1).*exp(1j*real(Lp));
                Right4(Ap) = Right4(Ap).*exp(-2*imag(Lp(Ap)));
            else
                Right1 = Hc0m(:,l,i+1);
                Right2 = Jc0m(:,l,i+1);
                Right3 = Hc0p(:,l,i+1);
                Right4 = Jc0p(:,l,i+1);
            end
            Hc1m(:,l,i) = (Left1-Right1)/2;
            Jc1m(:,l,i) = (Left2-Right2)/2;
            Hc1p(:,l,i) = (Left3-Right3)/2;
            Jc1p(:,l,i) = (Left4-Right4)/2;
        end

    end
    
    display('done with bessel')
    
    Chipm = Hc1m./Hc0m;
    Chimm = Jc1m./Jc0m;
    Chipp = Hc1p./Hc0p;
    Chimp = Jc1p./Jc0p;
    
    display('done with Chi')
    
    Yp = zeros(2*Nb,Nr-2,Nord);
    Ym = zeros(2*Nb,Nr-2,Nord);
    
    for l = 1:Nr-2

        L_Drho = Lambda(:,l+1)*(bnd_r(l+1)-bnd_r(l));

        for i = 1:Nord
            Yp(:,l,i) = exp(1j*L_Drho).*(Hc0p(:,l+1,i)./Hc0m(:,l,i));
            Ym(:,l,i) = exp(1j*L_Drho).*(Jc0m(:,l,i)./Jc0p(:,l+1,i));
        end

    end
    
    display('done with gamma')
    
    Q = zeros(2*Nb,2*Nb,Nr-1,Nord);

    for i = 1:Nord

        Tp = zeros(2*Nb,2*Nb,Nr-1);
        Tm = zeros(2*Nb,2*Nb,Nr-1);

        Rp = zeros(2*Nb,2*Nb,Nr-1);
        Rm = zeros(2*Nb,2*Nb,Nr-1);

        for l = 1:Nr-1

            beta1pp = zeros(2*Nb,2*Nb);
            beta1mp = zeros(2*Nb,2*Nb);
            beta2pm = zeros(2*Nb,2*Nb);

            beta1mm = zeros(2*Nb,2*Nb);
            beta1pm = zeros(2*Nb,2*Nb);
            beta2mp = zeros(2*Nb,2*Nb);

            beta1pp(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhe(:,:,l);
            beta1pp(B2,B1) = -diag(Chipp(B1,l,i).*Lambda(B1,l));
            beta1pp(B1,B2) = -1j*omega*muo*diag(Chipp(B2,l,i).*Lambda(B2,l));
            beta1pp(B2,B2) = 1j*nord(i)/bnd_r(l)*Deh(:,:,l);

            beta1mp(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhe(:,:,l);
            beta1mp(B2,B1) = -diag(Chimp(B1,l,i).*Lambda(B1,l));
            beta1mp(B1,B2) = -1j*omega*muo*diag(Chimp(B2,l,i).*Lambda(B2,l));
            beta1mp(B2,B2) = 1j*nord(i)/bnd_r(l)*Deh(:,:,l);

            beta2mp(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhem(:,:,l);
            beta2mp(B2,B1) = -P2m(B1,B1,l)*diag(Chimp(B1,l,i).*Lambda(B1,l));
            beta2mp(B1,B2) = -1j*omega*muo*P2m(B2,B2,l)*diag(Chimp(B2,l,i).*Lambda(B2,l));
            beta2mp(B2,B2) = 1j*nord(i)/bnd_r(l)*Dehm(:,:,l);

            beta1pm(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhe(:,:,l+1);
            beta1pm(B2,B1) = -diag(Chipm(B1,l,i).*Lambda(B1,l+1));
            beta1pm(B1,B2) = -1j*omega*muo*diag(Chipm(B2,l,i).*Lambda(B2,l+1));
            beta1pm(B2,B2) = 1j*nord(i)/bnd_r(l)*Deh(:,:,l+1);

            beta1mm(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhe(:,:,l+1);
            beta1mm(B2,B1) = -diag(Chimm(B1,l,i).*Lambda(B1,l+1));
            beta1mm(B1,B2) = -1j*omega*muo*diag(Chimm(B2,l,i).*Lambda(B2,l+1));
            beta1mm(B2,B2) = 1j*nord(i)/bnd_r(l)*Deh(:,:,l+1);

            beta2pm(B1,B1) = 1j*nord(i)/bnd_r(l)*Dhep(:,:,l);
            beta2pm(B2,B1) = -P2p(B1,B1,l)*diag(Chipm(B1,l,i).*Lambda(B1,l+1));
            beta2pm(B1,B2) = -1j*omega*muo*P2p(B2,B2,l)*diag(Chipm(B2,l,i).*Lambda(B2,l+1));
            beta2pm(B2,B2) = 1j*nord(i)/bnd_r(l)*Dehp(:,:,l);

            beta1pp = beta1pp*diag(power(Lambda(:,l),-2));
            beta1mp = beta1mp*diag(power(Lambda(:,l),-2));
            beta2mp = beta2mp*diag(power(Lambda(:,l),-2));
            beta1pm = beta1pm*diag(power(Lambda(:,l+1),-2));
            beta1mm = beta1mm*diag(power(Lambda(:,l+1),-2));
            beta2pm = beta2pm*diag(power(Lambda(:,l+1),-2));

            Tp(:,:,l) = (beta2pm-beta1mp*P1p(:,:,l))\(beta1pp-beta1mp);
            Tm(:,:,l) = (beta2mp-beta1pm*P1m(:,:,l))\(beta1mm-beta1pm);

            Rp(:,:,l) = P1p(:,:,l)*Tp(:,:,l)-eye(2*Nb);
            Rm(:,:,l) = P1m(:,:,l)*Tm(:,:,l)-eye(2*Nb);
            
        end
        
        display('done with local matrixes')
        
        for l = (Nr-1):-1:1
            if l == Nr-1
                Q(:,:,l,i) = Rp(:,:,l);
            else
                Q1 = diag(Ym(:,l,i))*Q(:,:,l+1,i)*diag(Yp(:,l,i));
                S = (eye(2*Nb)-Rm(:,:,l)*Q1)\Tp(:,:,l);
                Q(:,:,l,i) = Rp(:,:,l)+Tm(:,:,l)*Q1*S;
            end
        end
        
        display('done with generalized matrix')
        
    end
end
