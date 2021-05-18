classdef green

% ---------------------------------------------------------------------- %
%  ref: Thambynayagam 2011 The Diffusion Handbook.
%
%  1. Green array is structured as: [observers]x[sources]x[time_steps]
%  2. Elliptic Theta Function of Third Kind, ref. page 6.
%  3. Integral of Elliptic Theta Function of Third Kind, ref. page 7.
%
%  Two forms of theta functions are given. Both forms are valid over 
%  the whole range of time, but convergence is more rapid in the 
%  specified regions of the argument exp(-pi^2*t).
% ---------------------------------------------------------------------- %
  
  methods (Static)
    
    function point(obs,src,tau,res)
        
        cons = 1/(8*res.porosity*res.totCompressibility*...
                    res.xLength*res.yLength*res.zLength);
                
        Hx = green.thetaAssembly('x',obs,src,tau,res,'');
        Hy = green.thetaAssembly('y',obs,src,tau,res,'');
        Hz = green.thetaAssembly('z',obs,src,tau,res,'');
        
        Garray = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p+Hz.m);

        assignin('base','Gpoint',Garray)
        
    end
      
    function line(obs,src,tau,res)
        
        cons = 1/(4*res.porosity*res.totCompressibility*...
                    res.xLength*res.yLength);
        
        Hx = green.thetaAssembly('x',obs,src,tau,res,'');
        Hy = green.thetaAssembly('y',obs,src,tau,res,'');
        Hz = green.thetaAssembly('z',obs,src,tau,res,'integral');
        
        Garray = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p-Hz.m);

        assignin('base','Gline',Garray)

    end
    
    function plane(res,frac,time,str)
        
        tau = time.tau-time.deltaTime/2;
        
        cons = 1/(4*res.porosity*res.totCompressibility*...
                    res.xLength*res.yLength);
        
        switch nargin
            
            case 4
            
                if strcmp(str,'xz')

                    cons = cons*(2*res.xLength);

                    H1x = green.thetaAssembly('x',frac.center,frac.point1,tau,res,'integral');
                    H2x = green.thetaAssembly('x',frac.center,frac.point2,tau,res,'integral');
                    H1y = green.thetaAssembly('y',frac.center,frac.point1,tau,res,'');
                    H1z = green.thetaAssembly('z',frac.center,frac.point1,tau,res,'integral');

                    Garray = cons*(H2x.p-H2x.m-H1x.p+H1x.m).*(H1y.p+H1y.m).*(H1z.p-H1z.m);

                    disp('Green loading... 100% is complete')

                elseif strcmp(str,'yz')

                    cons = cons*(2*res.yLength);

                    H1x = green.thetaAssembly('x',frac.center,frac.point1,tau,res,'');
                    H1y = green.thetaAssembly('y',frac.center,frac.point1,tau,res,'integral');
                    H2y = green.thetaAssembly('y',frac.center,frac.point2,tau,res,'integral');
                    H1z = green.thetaAssembly('z',frac.center,frac.point1,tau,res,'integral');

                    Garray = cons*(H1x.p+H1x.m).*(H2y.p-H2y.m-H1y.p+H1y.m).*(H1z.p-H1z.m);

                    disp('Green loading... 100% is complete')

                end
            
            case 3
                
                N = 20;

                dl = frac.Length.*linspace(0,1,N);
                ds = (dl(:,2:end)-dl(:,1:end-1))/2;

                var.Xcoord = frac.point1.Xcoord+frac.signX.*dl.*cos(frac.azimuth);
                var.Ycoord = frac.point1.Ycoord+frac.signY.*dl.*sin(frac.azimuth);
                var.Zcoord = res.zLength;

                [Xx,Tx] = green.thetaAssembly('x',frac.center,var,tau,res);
                [Xy,Ty] = green.thetaAssembly('y',frac.center,var,tau,res);

                Hz = green.thetaAssembly('z',frac.center,var,tau,res,'integral');

                Garray = zeros(frac.numAfrac,frac.numAfrac,time.numTimeStep);

                for i = 1:frac.numAfrac

                    Hx.p = green.ellipticTheta(Xx.p(:,:,i),Tx);
                    Hx.m = green.ellipticTheta(Xx.m(:,:,i),Tx);

                    Hy.p = green.ellipticTheta(Xy.p(:,:,i),Ty);
                    Hy.m = green.ellipticTheta(Xy.m(:,:,i),Ty);

                    line = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p-Hz.m);

                    Garray(:,i,:) = sum(ds(i,:).*(line(:,2:end,:)+line(:,1:end-1,:)),2);

                    disp(['Green loading... ',num2str(i/frac.numAfrac*100),'% is complete'])

                end
            
        end
        
        assignin('base','Gplane',Garray)
        
    end
    
    function volume(obs,tau,res)
        
        src.Xcoord = res.xLength;
        src.Ycoord = res.yLength;
        src.Zcoord = res.zLength;
        
        Hx = green.thetaAssembly('x',obs,src,tau,res,'integral');
        Hy = green.thetaAssembly('y',obs,src,tau,res,'integral');
        Hz = green.thetaAssembly('z',obs,src,tau,res,'integral');
        
        Garray = (Hx.p-Hx.m).*(Hy.p-Hy.m).*(Hz.p-Hz.m);
    
        assignin('base','Gvolume',Garray)
        
    end
    
    function [varargout] = thetaAssembly(str1,obs,src,tau,res,str2)
        
        if strcmpi(str1,'x')
            Tloc = 'Xcoord';
            Tdim = 'xLength';
            Teta = 'xDiffusivity';
        elseif strcmpi(str1,'y')
            Tloc = 'Ycoord';
            Tdim = 'yLength';
            Teta = 'yDiffusivity';
        elseif strcmpi(str1,'z')
            Tloc = 'Zcoord';
            Tdim = 'zLength';
            Teta = 'zDiffusivity';
        end
        
        [~,Ncol] = size(src.(Tloc));
        
        if Ncol == 1
            src.(Tloc) = src.(Tloc)';
        elseif Ncol > 1
            src.(Tloc) = permute(src.(Tloc),[3,2,1]);
        end
        
        X.p = (obs.(Tloc)+src.(Tloc))/(2*res.(Tdim));
        X.m = (obs.(Tloc)-src.(Tloc))/(2*res.(Tdim));
        
        T = permute((res.(Teta)*tau)/(res.(Tdim)^2),[3 2 1]);
        
        switch nargin
            case 5
                varargout{1} = X;
                varargout{2} = T;
            case 6
                if strcmp(str2,'')
                    H.p = green.ellipticTheta(X.p,T);
                    H.m = green.ellipticTheta(X.m,T);
                elseif strcmp(str2,'integral')
                    H.p = green.ellipticTheta(X.p,T,'integral');
                    H.m = green.ellipticTheta(X.m,T,'integral');
                end
                varargout{1} = H;
        end
        
    end
    
    function theta = ellipticTheta(X,T,str)

      % There is a need to add convergence check for the following N

        N = 20;

        Nx = size(X,1);
        Ny = size(X,2);
        Nt = size(T,3);

        theta = zeros(Nx,Ny,Nt);

      % t0 = T<1e-5;                    % early time
        t1 = exp(-pi^2*T)>=1/pi;        % intermediate time
        t2 = ~t1;                       % later time

      % theta(:,:,t0) = 1./sqrt(pi*T(t0)).*exp(-X.^2./T(t0));
        
        switch nargin
            case 2
                
                if sum(t1)~=0
                    coef1 = theta(:,:,t1);
                    for n = -N:N
                        coef1 = coef1+exp(-(X+n).^2./T(t1));
                    end
                    theta(:,:,t1) = 1./sqrt(pi*T(t1)).*coef1;
                end
                
                if sum(t2)~=0
                    coef2 = theta(:,:,t2);
                    for n = 1:N
                        coef2 = coef2+exp(-n^2*pi^2*T(t2)).*cos(2*n*pi*X);
                    end
                    theta(:,:,t2) = 1+2*coef2;
                end
                
            case 3
                if strcmp(str,'integral')
                    
                    if sum(t1)~=0
                        coef1 = theta(:,:,t1);
                        for n = -N:N
                            coef1 = coef1+erf((X+n)./sqrt(T(t1)))-erf(n./sqrt(T(t1)));
                        end
                        theta(:,:,t1) = 1/2*coef1;
                    end
                    
                    if sum(t2)~=0
                        coef2 = theta(:,:,t2);
                        for n = 1:N
                            coef2 = coef2+exp(-n^2*pi^2*T(t2))/n.*sin(2*n*pi*X);
                        end
                        theta(:,:,t2) = X+1/pi*coef2;
                    end
                    
                else
                    error('integral is the only available option')
                end
                
        end

    end
    
  end
    
end