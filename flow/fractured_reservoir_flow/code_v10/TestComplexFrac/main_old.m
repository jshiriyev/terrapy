clear; close all; clc

addpath('C:\Users\js68897\repos\greensfrac')

res = resControl(pwd,'SI');
frac = fracControl(pwd,'SI');
well = wellControl(pwd,'SI');
time = numControl(pwd,'SI');

Gsol = green.plane(frac.center,frac,time.tau-time.deltaTime/2,res);

sol = solver(res,frac,well,time,Gsol);

% vtkwrite('results\green.vtk',res,frac,well,time,sol);

geometry(res,frac,well);
% flowrateBH(sol,time);
% pressureBH(sol,time);
% pressure1D(sol,res,frac,time);
% pressure2D(sol,res,frac,time);

function geometry(res,frac,well)

    figname = 'Fracture Map';
    
    figure('Name',figname,'NumberTitle','off')

    prop1.MarkerSize = 10;
    prop1.MarkerEdgeColor = 'r';
    prop1.MarkerFaceColor = 'r';
    
    prop2.Color = 'k';
    
    prop3.Color = 'k';
    
    plotAll.node(frac,prop1); hold on
    plotAll.fracture(frac,prop2); hold on
    plotAll.well(frac,well,prop3);
    
    xlim([0,res.xLength]);
    ylim([0,res.yLength]);
    
    xlabel('x-axis [m]')
    ylabel('y-axis [m]')
    
    savefig(gcf,['results/',figname])
    close(gcf)
    
end

function flowrateBH(sol,time)
    
    figname = 'Well Flow Rate';
    
    figure('Name',figname,'NumberTitle','off')

    Q = sol.wellflowrate/inpput.convFactorDetermine('flowrate');
    T = time.tau/inpput.convFactorDetermine('time');

    plot(T,Q); hold on
    
    xlabel('time [days]')
    ylabel('flow rate [bbl/day]')
    
    savefig(gcf,['results/',figname])
    close(gcf)

end

function pressureBH(sol,time)

    figname = 'Wellbore Pressure';

    figure('Name',figname,'NumberTitle','off')
    
    T = time.tau/inpput.convFactorDetermine('time');
    P = sol.wellpressure/inpput.convFactorDetermine('pressure');
    
    plot(T,P); hold on

    xlabel('time [days]')
    ylabel('bottom hole pressure [psi]')
    
    savefig(gcf,['results/',figname])
    close(gcf)
    
end

function pressure1D(sol,res,frac,time)

    obs1D = plotAll.obsNodes(0,res.xLength/2,1,res.yLength/3,2*res.yLength/3,400);
    
    Gplt1D = green.plane(obs1D,frac,time.tau,res);
    
    assignin('base','obs1D',obs1D)
    assignin('base','Gplt1D',Gplt1D)
    
    figure('Name','Reservoir Pressure','NumberTitle','off')
    
    time.snapTime = time.snapTime/inpput.convFactorDetermine('time');
    
    plotAll.obsPressure(obs1D,sol,res,frac,time,Gplt1D,1);
    
    legend('0.1 day','1 day','10 day','100 day','Location','SouthEast')
    
    xlim([res.yLength/3,2*res.yLength/3]);
    ylim([2500,4500]);
    
    xlabel('distance [m]');
    ylabel('pressure [psi]');
            
    savefig(gcf,'results/pressure1D')
    close(gcf)
            
end

function pressure2D(sol,res,frac,time)

    obs2D = plotAll.calc2Dnodes([0,res.xLength,40],[0,res.yLength,40]);
    
    Gplt2D = green.plane(obs2D,frac,time.tau-time.deltaTime/2,res);
    
    assignin('base','obs2D',obs2D)
    assignin('base','Gplt2D',Gplt2D)
    
    pressure = plotAll.calcPressure(sol,res,time,Gplt2D);
    
    plotAll.pressure2D(obs2D,pressure,frac,time,[200,200]);

end