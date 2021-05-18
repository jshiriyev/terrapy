function plotting(tau,p_well,q_well,name)
    
%     dp = abs(p_well(1:end-1)-p_well(2:end))./abs(tau(1:end-1)-tau(2:end));
%     loglog(tau(1:end-1),dp);

    figure('Name',name,'NumberTitle','off')

    subplot(2,1,1)

    plot(tau,p_well);

    xlim([0,max(tau)])
%     ylim([2000,4500])

    set(gca,'XTickLabels',[])

    ylabel('BHP [psi]')

    subplot(2,1,2)

    plot(tau,q_well/1.273);

    xlim([0,max(tau)])
%     ylim([10,50])

    ylabel('Flow Rate [STB/day]')
    xlabel('Time [days]')
    
% %     mapping(res,frac,qfj,time)
% %     mapping(res,frac,permute(qfj,[3,1,2]),time)
    
%     coeff_b = 1/(2*res.porosity*res.totcompres*res.xlength);
%     
%     [X,Y,Z] = meshgrid(0:res.xlength/25:res.xlength,...
%                           0:res.ylength/20:res.ylength,res.zlength/2);
% 
%     coord = [reshape(X,[],1),reshape(Y,[],1),reshape(Z,[],1)];
%     
%     Nt = round(time.tt/time.dt);
% 
%     tau = linspace(time.dt,time.tt,Nt)';
%     
%     Green = greenfunc(coord,frac,tau,res);
%     Gterm = coeff_b*Green*time.dt;
%     
%     pressure = res.initpressure-sum(sum(qfj.*Gterm,3),2);
%     
%     % conversion back to field units ----------------------------------- %
%     
%     coord = coord/0.3048;                   % [m] to [ft]
%     pressure = pressure/6894.76;            % [Pa] to [psi]
%     
%     X = X/0.3048;                           % [m] to [ft]
%     Y = Y/0.3048;                           % [m] to [ft]
%     
%     res.xlength = res.xlength/0.3048;               % [m] to [ft]
%     res.ylength = res.ylength/0.3048;               % [m] to [ft]
%     
%     frac.xnodes = frac.xnodes/0.3048;
%     frac.ynodes = frac.ynodes/0.3048;
%     
%     % conversion ends here --------------------------------------------- %
% 
%     figure('Name','Reservoir Domain','NumberTitle','off')
%     
%     Z = griddata(coord(:,1),coord(:,2),pressure,X,Y);
%     
%     h = pcolor(X,Y,Z);
%     
%     set(h,'EdgeColor','none');
%     shading interp
%     
%     colormap(jet)
%     colorbar
%     
%     hold on
%     
% %     plot(frac.xnodes,frac.ynodes,'k.','MarkerSize',7); hold on
% %     plot(frac.xnodes,frac.ynodes,'k-'); hold on
%     plot(frac.xnodes,frac.ynodes,'w','LineWidth',3); hold on
%     pbaspect([res.xlength/res.ylength 1 1])
%     
%     set(gca,'FontSize',14)
% 
%     xlim([0,res.xlength])
%     ylim([0,res.ylength])
% 
%     xlabel('x-axis [ft]');
%     ylabel('y-axis [ft]');

end