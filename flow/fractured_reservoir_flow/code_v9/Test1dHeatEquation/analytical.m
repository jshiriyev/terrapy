function sol = analytical(time)
    
    Niter = 10000;

    n = 0:Niter;
    n = permute(n,[1,3,2]);

    sol.x = linspace(0,1);
    sol.time = time;

    T = exp(-(2*n+1).^2*pi^2/4.*sol.time);
    X = cos((2*n+1)*pi/2.*sol.x);

    sol.pressure = 4/pi*sum((-1).^n./(2*n+1).*T.*X,3);

end