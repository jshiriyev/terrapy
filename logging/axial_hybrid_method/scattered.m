function [z_obsrvr,Hsca] = scattered(string,Cmat,Lambda,Q,Hc0p,Jc0p)
    
    global bnd_r mur_s mur_z nord Nord Nb B2

    fid = fopen(strcat(string,'sampling.txt'));

    Zstart = str2num(strtok(fgetl(fid),'%'));
    Zend = str2num(strtok(fgetl(fid),'%'));
    Nsamples = str2num(strtok(fgetl(fid),'%'));
    Ltr = str2num(strtok(fgetl(fid),'%'));

    fclose(fid);

    rhoa = 1e-6;
    
    Lb = Lambda(:,1)*bnd_r(1);
    La = Lambda(:,1)*rhoa;
    
    Aa = imag(La)<0;

    Hcap_a = zeros(2*Nb,Nord);
    Jcap_a = zeros(2*Nb,Nord);

    Q_a = zeros(2*Nb,2*Nb,Nord);

    for i = 1:Nord
        
        Hcap_a(:,i) = besselh(nord(i),1,La,1);
        Jcap_a(:,i) = besselj(nord(i),La,1).*exp(1j*real(La));
        Jcap_a(Aa,i) = Jcap_a(Aa,i).*exp(-2*imag(La(Aa)));
        
        Yp_a = exp(1j*(Lb-La)).*(Hc0p(:,1,i)./Hcap_a(:,i));
        Ym_a = exp(1j*(Lb-La)).*(Jcap_a(:,i)./Jc0p(:,1,i));
        
        Q_a(:,:,i) = diag(Ym_a)*Q(:,:,1,i)*diag(Yp_a);
        
    end
    
    display('all done before sampling')
    
    z_source = linspace(Zstart,Zend,Nsamples)';
    z_obsrvr = z_source+Ltr;
    
    Hsca = zeros(Nsamples,1);
    
    for k = 1:Nsamples
        gh_source = sampling(z_source(k),mur_s(1,1));
        gh_obsrvr = sampling(z_obsrvr(k),mur_s(1,1));
        for i = 1:Nord
            bph = 1/4j/mur_z(1,1)*diag(Hcap_a(B2,i).*Jcap_a(B2,i).*Lambda(B2,1).^2)*...
                  transpose(Cmat(B2,B2,1))*gh_source;
            Hsca_n = 1/mur_z(1,1)*gh_obsrvr'*Cmat(B2,B2,1)*Q_a(B2,B2,i)*bph;
            if i == 1
                Hsca(k,1) = Hsca(k,1)+Hsca_n;
            else
                Hsca(k,1) = Hsca(k,1)+2*Hsca_n;
            end
        end
    end
    
end

function geta = sampling(z_dash,qeta)
    
    % wellbore must be homogeneous
    
    global Zglobal dz Ne Nb
    
    geta = zeros(Nb,1);
    
    for i = 1:Ne
        if and(z_dash>=Zglobal(i),z_dash<Zglobal(i+1))
            L1 = (Zglobal(i+1)-z_dash)/dz(i);
            L2 = (z_dash-Zglobal(i))/dz(i);
            geta(2*i-3) = -2*L1^3+3*L1^2;
            geta(2*i-2) = qeta*dz(i)*L1^2*L2;
            geta(2*i-1) = -2*L2^3+3*L2^2;
            geta(2*i) = -qeta*dz(i)*L2^2*L1;
        end
    end
    
end