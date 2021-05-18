function sol = numerical(time)
    
    sol.time = time;

    sol.numofgrid = 50;

    sol.deltatime = 0.0001;
    sol.deltax = 1/sol.numofgrid;

    sol.pressure = ones(sol.numofgrid,1);   % initial condition
    alfa = (sol.deltatime)/(sol.deltax)^2;

    idx = 1:sol.numofgrid;
    idy = 1:(sol.numofgrid-1);

    I = sparse(idx,idx,1,sol.numofgrid,sol.numofgrid);

    A = sparse(sol.numofgrid,sol.numofgrid);
    A = A+sparse(idy,idy+1,-1,sol.numofgrid,sol.numofgrid);
    A = A+sparse(idy+1,idy,-1,sol.numofgrid,sol.numofgrid);
    A = A+sparse(idx,idx,2,sol.numofgrid,sol.numofgrid);

    A(1,1) = 1;                             % left side no-flow boundary condition
    A(sol.numofgrid,sol.numofgrid) = 3;     % right side dirichlet boundary condition
    
    Ntime = round(sol.time/sol.deltatime);
    
    for i = 1:Ntime
        sol.pressure = (I+alfa*A)\sol.pressure;
    end

    sol.x = linspace(sol.deltax/2,1-sol.deltax/2,sol.numofgrid)';
    
end