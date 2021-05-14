function [] = plottingresults(xu,yu,xp,yp,xs,ys,ux,uy,Sxx,Syy,Sxy,p)

    xld=linspace(0,max(xu),500);
    yld=linspace(0,max(yu),500);
    [X,Y]=meshgrid(xld,yld);

    ux=griddata(xu,yu,ux,X,Y);
    uy=griddata(xu,yu,uy,X,Y);
    Sxx=griddata(xs,ys,Sxx,X,Y);
    Syy=griddata(xs,ys,Syy,X,Y);
    Sxy=griddata(xs,ys,Sxy,X,Y);
    p=griddata(xp,yp,p,X,Y);

    figure(1)
    contourf(X,Y,ux)
    title('u_{x}');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])

    figure(2)
    contourf(X,Y,uy)
    title('u_{y}');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])

    figure(3)
    contourf(X,Y,Sxx)
    title('\sigma_{xx}');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])

    figure(4)
    contourf(X,Y,Syy)
    title('\sigma_{yy}');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])

    figure(5)
    contourf(X,Y,Sxy)
    title('\sigma_{xy}');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])

    figure(6)
    contourf(X,Y,p)
    title('Pressure');
    xlabel('x');
    ylabel('y');
    axis equal tight
    colorbar
    hold on
    rectangle('Position',[-0.8 -1 1.6 2],'FaceColor',[1 1 1],...
        'EdgeColor','w','Curvature',[1 1])
    
end

