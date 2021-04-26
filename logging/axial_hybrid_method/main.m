clear all %#ok<CLALL>
close all
clc

string = strcat(pwd,'\Library\run_MM_short\');

timerVal = tic;

global muo omega Nb

muo = 4*pi*1E-7;	% free space electrical permeability, [H/m]

MMtr = indata(string);
stat = meshing(string);

tic;
[Cmat,Lambda] = eigencall(stat);
T1 = toc;

tic;
[Q,Hc0p,Jc0p] = refmatrix(stat,Cmat,Lambda);
T2 = toc;

tic;
[z_obsr,Hsca] = scattered(string,Cmat,Lambda,Q,Hc0p,Jc0p);
T3 = toc;

Vzz_sca = 1j*muo*omega*MMtr*Hsca*1e6;

out = [real(Vzz_sca) imag(Vzz_sca)];

dlmwrite(strcat(string,'out.dat'),out,'delimiter','\t');

T4 = toc(timerVal);

fid = fopen(strcat(string,'info.dat'),'w+');

fprintf(fid,'%f \t %% the solution of generalized eigenvalue problem\n',T1);
fprintf(fid,'%d \t %% number of basis functions\n',Nb);
fprintf(fid,'%f \t %% calculation of generalized refraction matrix\n',T2);
fprintf(fid,'%f \t %% solution time for all points\n',T3);
fprintf(fid,'%d \t %% number of source points\n',size(Vzz_sca,1));
fprintf(fid,'%f \t %% total run time\n',T4);

fclose(fid);

% figure(1)
% 
% semilogy(z_obsr,abs(real(Vzz_sca)),'r'); hold on
% semilogy(z_obsr,abs(imag(Vzz_sca)),'k');
% 
% legend('real','imag')
% 
% ylabel(['scattered voltage [',char(181),'V]'],'FontSize',14)
% xlabel('distance [m]','FontSize',14)