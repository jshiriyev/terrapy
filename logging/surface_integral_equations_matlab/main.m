clear all
close all
clc

string = strcat(pwd,'\Library\hfd_1m_0deg\');

timerVal = tic;

muo = 4*pi*1E-7;                % free space electrical permeability, [H/m]
epso = 8.854187817*1E-12;       % free space permittivity, [F/m]

opcon = load(strcat(string,'operation.txt'));

omega = 2*pi*opcon(1);          % angular frequency, [rad.Hz]

k1 = sqrt(muo*epso*omega^2-1j*muo*opcon(5)*omega);

geometry(string);      % conductance of the fracture, [S]

tic;
[Zmn,Bmn] = impedance(k1,opcon(6));
T1 = toc;

Tmn = 1j*omega*muo*Zmn+Bmn;

tic;
Hsca = scattered(k1,opcon(7),Tmn,string);
T2 = toc;

Mtr = prod(opcon(2:4));

Vxz = -1j*(muo*omega)^2*Mtr*(Hsca(:,1))*1e6;
Vyz = -1j*(muo*omega)^2*Mtr*(Hsca(:,2))*1e6;
Vzz = -1j*(muo*omega)^2*Mtr*(Hsca(:,3))*1e6;

out = [real(Vxz) imag(Vxz) real(Vyz) imag(Vyz) real(Vzz) imag(Vzz)];

RD = rand(82,6);                % adding noise
out = out+out*0.01.*RD;         % adding noise

dlmwrite(strcat(string,'out.dat'),out,'delimiter','\t');

T3 = toc(timerVal);

fid = fopen(strcat(string,'info.dat'),'w+');

fprintf(fid,'%f \t matrix fill time\n',T1);
fprintf(fid,'%d \t\t number of unknowns\n',size(Bmn,1));
fprintf(fid,'%f \t matrix solution time for all points\n',T2);
fprintf(fid,'%d \t\t number of source points\n',size(Vzz,1));
fprintf(fid,'%f \t total run time\n',T3);

fclose(fid);