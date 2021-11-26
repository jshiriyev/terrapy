function [stat] = meshing(string)
    
    global Zglobal dz Nz Nr bnd_z Ne Nb B1 B2
    
    fid = fopen(strcat(string,'meshing.txt'));

    zlog1 = str2num(strtok(fgetl(fid),'%'));
    zlogN = str2num(strtok(fgetl(fid),'%'));
    Dz = str2num(strtok(fgetl(fid),'%'));
    Qexp = str2num(strtok(fgetl(fid),'%'));
    zmax = str2num(strtok(fgetl(fid),'%'));
    
    fclose(fid);
    
    zlog1 = zlog1-1;
    zlogN = zlogN+1;
    
    Zmiddle = (zlog1:Dz:zlogN)';
    
    Nright = ceil(log((zmax-zlogN)/Dz*(1-1/Qexp)+1)/log(Qexp))+2;
    
    Zright = zeros(Nright,1);
    
    for i = 1:Nright
        Zright(i) = zlogN+Dz*sum(power(Qexp,(1:i)));
    end
    
    Nleft = ceil(log((zmax+zlog1)/Dz*(1-1/Qexp)+1)/log(Qexp))+2;
    
    Zleft = zeros(Nleft,1);
    
    for i = 1:Nleft
        Zleft(i) = zlog1-Dz*sum(power(Qexp,(1:i)));
    end
    
    Zglobal = [flipud(Zleft)',Zmiddle',Zright']';
    
    idx = zeros(Nz-1,1);

    for i = 1:Nz-1
        if sum(bnd_z(i)==Zglobal)
            idx(i) = sum(bnd_z(i)>Zglobal)+1;
        else
            idx(i) = sum(bnd_z(i)>Zglobal);
            Zglobal = [Zglobal(1:idx(i));bnd_z(i);Zglobal(idx(i)+1:end)];
            idx(i) = idx(i)+1;
        end
    end
    
    dz = Zglobal(2:end)-Zglobal(1:end-1);
    
    Ng = length(Zglobal);       % number of grids
    Ne = Ng-1;                  % number of elements
    Nb = 2*(Ne-1);              % number of basis functions
    
    idx = [1;idx;Ng];
    
    B1 = 1:Nb;
    B2 = B1+Nb;
    
    global muo omega sigma_s sigma_z mur_s mur_z

    stat.qe = zeros(Ne,Nr);
    stat.qh = zeros(Ne,Nr);
    stat.pe = zeros(Ne,Nr);
    stat.ph = zeros(Ne,Nr);
    
    for j = 1:Nr
        for i = 1:Nz
            stat.qe(idx(i):idx(i+1)-1,j) = sigma_s(i,j);
            stat.qh(idx(i):idx(i+1)-1,j) = mur_s(i,j);
            stat.pe(idx(i):idx(i+1)-1,j) = sigma_z(i,j);
            stat.ph(idx(i):idx(i+1)-1,j) = mur_z(i,j);
        end
    end

    stat.k2e = 1j*omega*muo*stat.qh.*stat.pe;
    stat.k2h = 1j*omega*muo*stat.qe.*stat.ph;
    
end