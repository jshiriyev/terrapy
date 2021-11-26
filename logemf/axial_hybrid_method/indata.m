function [Mtr] = indata(string)
    
    global Nz Nr bnd_z bnd_r sigma_s sigma_z mur_s mur_z omega nord Nord
    
    fid = fopen(strcat(string,'formation.txt'));
    
    Nzr = str2num(strtok(fgetl(fid),'%'));
    
    Nz = Nzr(1);        % number of layers in z (wellbore) direction
    Nr = Nzr(2);        % number of layers in radial direction
    
    bnd_z = str2num(strtok(fgetl(fid),'%'));
    bnd_r = str2num(strtok(fgetl(fid),'%'));
    
    sigma_s = zeros(Nz,Nr);
    sigma_z = zeros(Nz,Nr);
    
    mur_s = zeros(Nz,Nr);
    mur_z = zeros(Nz,Nr);
    
    for i = 1:Nz
        sigma_s(i,:) = str2num(strtok(fgetl(fid),'%'));
        sigma_z(i,:) = str2num(strtok(fgetl(fid),'%'));
    end
    
    for i = 1:Nz
        mur_s(i,:) = str2num(strtok(fgetl(fid),'%'));
        mur_z(i,:) = str2num(strtok(fgetl(fid),'%'));
    end
    
    fclose(fid);
    
    fid = fopen(strcat(string,'operation.txt'));
    
    freq = str2num(strtok(fgetl(fid),'%'));
    Mtx = str2num(strtok(fgetl(fid),'%'));
    Nrx = str2num(strtok(fgetl(fid),'%'));
    Arx = str2num(strtok(fgetl(fid),'%'));
    
    fclose(fid);
    
    Mtr = Mtx*Nrx*Arx;
    omega = 2*pi*freq;
    
    % ---------------------- Fourier series ------------------------ %
    
    nordmin = 0;
    nordmax = 1;
    
    nord = (nordmin:nordmax)';
    Nord = length(nord);
    
end