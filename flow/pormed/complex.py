import numpy as np 

if __name__ == "__main__":
    import setup

from mathbox.elliptictheta import kind3

class rectangular_parallelepiped():

    def __init__(self,pi,a,b,d,phi,ct,mu,kx,ky,kz,q,time,time_steps=20):

        self.pi = pi

        self.a = a
        self.b = b
        self.d = d

        self.phi = phi

        self.ct = ct
        self.mu = mu

        self.kx = kx
        self.ky = ky
        self.kz = kz

        self.etax = self.kx/(self.phi*self.ct*self.mu)
        self.etay = self.ky/(self.phi*self.ct*self.mu)
        self.etaz = self.kz/(self.phi*self.ct*self.mu)

        self.q = q

        self.time = time

        self.taus = np.linspace(0,self.time,time_steps,dtype=np.float64)

    def set_observers(self):

        self.numobs = 50

        self.x = np.linspace(self.a/2,self.a,self.numobs,dtype=np.float64)
        # self.y = np.linspace(0,self.b,self.numobs,dtype=np.float64)
        self.y = np.full(self.numobs,self.b/2,dtype=np.float64)
        self.z = np.full(self.numobs,self.d,dtype=np.float64)

    def point(self):

        pass

    def line(self,xo,yo):

        X1 = np.pi*(self.x+xo)/(2*self.a)
        X2 = np.pi*(self.x-xo)/(2*self.a)

        Y1 = np.pi*(self.y+yo)/(2*self.b)
        Y2 = np.pi*(self.y-yo)/(2*self.b)

        Z1 = np.pi*(self.z+self.d)/(2*self.d)
        Z2 = np.pi*(self.z-self.d)/(2*self.d)

        Tx = np.exp(-(np.pi/self.a)**2*self.etax*self.taus[1:])
        Ty = np.exp(-(np.pi/self.b)**2*self.etay*self.taus[1:])
        Tz = np.exp(-(np.pi/self.d)**2*self.etaz*self.taus[1:])

        thetax1 = kind3(X1,Tx,Nmax=1000)
        thetax2 = kind3(X2,Tx,Nmax=1000)
        thetay1 = kind3(Y1,Ty,Nmax=1000)
        thetay2 = kind3(Y2,Ty,Nmax=1000)
        thetaz1 = kind3(Z1,Tz,Nmax=1000)
        thetaz2 = kind3(Z2,Tz,Nmax=1000)

        itx1 = thetax1.function()
        itx2 = thetax2.function()
        ity1 = thetay1.function()
        ity2 = thetay2.function()
        itz1 = thetaz1.integral()
        itz2 = thetaz2.integral()

        cons = 1/(4*self.phi*self.ct*self.a*self.b);

        deltap = np.zeros(self.numobs)

        for index,tau in enumerate(self.taus[1:]):

            if index == 0:
                deltat = self.taus[index]
            else:
                deltat = self.taus[index]-self.taus[index-1]

            # print("Current time step is {}".format(tau))

            green = (next(itx1)+next(itx2))*(next(ity1)+next(ity2))*(next(itz1)-next(itz2))

            # print(green.shape)

            deltap += green*deltat

        return self.pi-self.q*cons*deltap

    def plane(self):

        pass

# classdef green

# % ---------------------------------------------------------------------- %
# %  ref: Thambynayagam 2011 The Diffusion Handbook.
# %
# %  1. Green array is structured as: [observers]x[sources]x[time_steps]
# %  2. Elliptic Theta Function of Third Kind, ref. page 6.
# %  3. Integral of Elliptic Theta Function of Third Kind, ref. page 7.
# %
# %  Two forms of theta functions are given. Both forms are valid over 
# %  the whole range of time, but convergence is more rapid in the 
# %  specified regions of the argument exp(-pi^2*t).
# % ---------------------------------------------------------------------- %
  
#   methods (Static)
    
#     function array = point(obs,src,tau,res)
        
#         cons = 1/(8*res.porosity*res.totCompressibility*...
#                     res.xLength*res.yLength*res.zLength);
                
#         Hx = green.thetaAssembly('x',obs,src,tau,res,'');
#         Hy = green.thetaAssembly('y',obs,src,tau,res,'');
#         Hz = green.thetaAssembly('z',obs,src,tau,res,'');
        
#         array = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p+Hz.m);
              
#     end
      
#     function array = line(obs,src,tau,res)
        
#         cons = 1/(4*res.porosity*res.totCompressibility*...
#                     res.xLength*res.yLength);
        
#         Hx = green.thetaAssembly('x',obs,src,tau,res,'');
#         Hy = green.thetaAssembly('y',obs,src,tau,res,'');
#         Hz = green.thetaAssembly('z',obs,src,tau,res,'integral');
        
#         array = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p-Hz.m);

#     end
    
#     function array = plane(obs,src,tau,res,str)
        
#         cons = 1/(4*res.porosity*res.totCompressibility*...
#                     res.xLength*res.yLength);
        
#         switch nargin
            
#             case 5
            
#             if strcmp(str,'xz')

#                 cons = cons*(2*res.xLength);
                
#                 H1x = green.thetaAssembly('x',obs,src.point1,tau,res,'integral');
#                 H2x = green.thetaAssembly('x',obs,src.point2,tau,res,'integral');
#                 H1y = green.thetaAssembly('y',obs,src.point1,tau,res,'');
#                 H1z = green.thetaAssembly('z',obs,src.point1,tau,res,'integral');
                
#                 array = cons*(H2x.p-H2x.m-H1x.p+H1x.m).*(H1y.p+H1y.m).*(H1z.p-H1z.m);
                
#                 disp('Green loading... 100% is complete')

#             elseif strcmp(str,'yz')

#                 cons = cons*(2*res.yLength);
                
#                 H1x = green.thetaAssembly('x',obs,src.point1,tau,res,'');
#                 H1y = green.thetaAssembly('y',obs,src.point1,tau,res,'integral');
#                 H2y = green.thetaAssembly('y',obs,src.point2,tau,res,'integral');
#                 H1z = green.thetaAssembly('z',obs,src.point1,tau,res,'integral');
                
#                 array = cons*(H1x.p+H1x.m).*(H2y.p-H2y.m-H1y.p+H1y.m).*(H1z.p-H1z.m);
                
#                 disp('Green loading... 100% is complete')
                
#             end
            
#             case 4
                
#             N = 20;
            
#             dl = src.Length.*linspace(0,1,N);
#             ds = (dl(:,2:end)-dl(:,1:end-1))/2;

#             var.Xcoord = src.point1.Xcoord+src.signX.*dl.*cos(src.azimuth);
#             var.Ycoord = src.point1.Ycoord+src.signY.*dl.*sin(src.azimuth);
#             var.Zcoord = res.zLength;
            
#             [Xx,Tx] = green.thetaAssembly('x',obs,var,tau,res);
#             [Xy,Ty] = green.thetaAssembly('y',obs,var,tau,res);
            
#             Hz = green.thetaAssembly('z',obs,var,tau,res,'integral');
            
#             for i = 1:src.numAfrac

#                 Hx.p = green.ellipticTheta(Xx.p(:,:,i),Tx);
#                 Hx.m = green.ellipticTheta(Xx.m(:,:,i),Tx);

#                 Hy.p = green.ellipticTheta(Xy.p(:,:,i),Ty);
#                 Hy.m = green.ellipticTheta(Xy.m(:,:,i),Ty);

#                 line = cons*(Hx.p+Hx.m).*(Hy.p+Hy.m).*(Hz.p-Hz.m);

#                 array(:,i,:) = sum(ds(i,:).*(line(:,2:end,:)+line(:,1:end-1,:)),2); %#ok<AGROW>

#                 disp(['Green loading... ',num2str(i/src.numAfrac*100),'% is complete'])

#             end
            
#         end
        
#     end
    
#     function array = volume(obs,tau,res)
        
#         src.Xcoord = res.xLength;
#         src.Ycoord = res.yLength;
#         src.Zcoord = res.zLength;
        
#         Hx = green.thetaAssembly('x',obs,src,tau,res,'integral');
#         Hy = green.thetaAssembly('y',obs,src,tau,res,'integral');
#         Hz = green.thetaAssembly('z',obs,src,tau,res,'integral');
        
#         array = (Hx.p-Hx.m).*(Hy.p-Hy.m).*(Hz.p-Hz.m);
        
#     end
    
#     function [varargout] = thetaAssembly(str1,obs,src,tau,res,str2)
        
#         if strcmpi(str1,'x')
#             Tloc = 'Xcoord';
#             Tdim = 'xLength';
#             Teta = 'xDiffusivity';
#         elseif strcmpi(str1,'y')
#             Tloc = 'Ycoord';
#             Tdim = 'yLength';
#             Teta = 'yDiffusivity';
#         elseif strcmpi(str1,'z')
#             Tloc = 'Zcoord';
#             Tdim = 'zLength';
#             Teta = 'zDiffusivity';
#         end
        
#         [~,Ncol] = size(src.(Tloc));
        
#         if Ncol == 1
#             src.(Tloc) = src.(Tloc)';
#         elseif Ncol > 1
#             src.(Tloc) = permute(src.(Tloc),[3,2,1]);
#         end
        
#         X.p = (obs.(Tloc)+src.(Tloc))/(2*res.(Tdim));
#         X.m = (obs.(Tloc)-src.(Tloc))/(2*res.(Tdim));
        
#         T = permute((res.(Teta)*tau)/(res.(Tdim)^2),[3 2 1]);
        
#         switch nargin
#             case 5
#                 varargout{1} = X;
#                 varargout{2} = T;
#             case 6
#                 if strcmp(str2,'')
#                     H.p = green.ellipticTheta(X.p,T);
#                     H.m = green.ellipticTheta(X.m,T);
#                 elseif strcmp(str2,'integral')
#                     H.p = green.ellipticTheta(X.p,T,'integral');
#                     H.m = green.ellipticTheta(X.m,T,'integral');
#                 end
#                 varargout{1} = H;
#         end
        
#     end
    
#     function theta = ellipticTheta(X,T,str)

#       % There is a need to add convergence check for the following N

#         N = 20;

#         Nx = size(X,1);
#         Ny = size(X,2);
#         Nt = size(T,3);

#         theta = zeros(Nx,Ny,Nt);

#       % t0 = T<1e-5;                    % early time
#         t1 = exp(-pi^2*T)>=1/pi;        % intermediate time
#         t2 = ~t1;                       % later time

#       % theta(:,:,t0) = 1./sqrt(pi*T(t0)).*exp(-X.^2./T(t0));
        
#         switch nargin
#             case 2
                
#                 if sum(t1)~=0
#                     coef1 = theta(:,:,t1);
#                     for n = -N:N
#                         coef1 = coef1+exp(-(X+n).^2./T(t1));
#                     end
#                     theta(:,:,t1) = 1./sqrt(pi*T(t1)).*coef1;
#                 end
                
#                 if sum(t2)~=0
#                     coef2 = theta(:,:,t2);
#                     for n = 1:N
#                         coef2 = coef2+exp(-n^2*pi^2*T(t2)).*cos(2*n*pi*X);
#                     end
#                     theta(:,:,t2) = 1+2*coef2;
#                 end
                
#             case 3
#                 if strcmp(str,'integral')
                    
#                     if sum(t1)~=0
#                         coef1 = theta(:,:,t1);
#                         for n = -N:N
#                             coef1 = coef1+erf((X+n)./sqrt(T(t1)))-erf(n./sqrt(T(t1)));
#                         end
#                         theta(:,:,t1) = 1/2*coef1;
#                     end
                    
#                     if sum(t2)~=0
#                         coef2 = theta(:,:,t2);
#                         for n = 1:N
#                             coef2 = coef2+exp(-n^2*pi^2*T(t2))/n.*sin(2*n*pi*X);
#                         end
#                         theta(:,:,t2) = X+1/pi*coef2;
#                     end
                    
#                 else
#                     error('integral is the available option')
#                 end
#         end

#     end
    
#   end
    
# end

if __name__ == "__main__":

    import matplotlib.pyplot as plt

    mu = 0.72   # cp

    kx = 0.1    # md
    ky = 0.1    # md
    kz = 0.1    # md

    ct = 1.5e-5 # 1/psi

    pi = 3000   # psi

    re = 3000   # ft
    rw = 0.5    # ft

    h = 150     # ft

    q = 20      # STB/day

    Bo = 1.475  # bbl/STB

    phi = 0.23

    time = 3    # hours

    # UNIT CONVERSION TO SI UNITS

    mu = mu*0.001

    kx = kx*0.986923E-15
    ky = ky*0.986923E-15
    kz = kz*0.986923E-15

    ct = ct/6894.76

    pi = pi*6894.76

    re = re*0.3048
    rw = rw*0.3048

    h = h*0.3048
    q = q*Bo*1.85185E-6

    time = time*3600

    rpgreen = rectangular_parallelepiped(pi,2*re,2*re,h,phi,ct,mu,kx,ky,kz,q,time)

    rpgreen.set_observers()

    pressure = rpgreen.line(re,re)

    rpgreen.x = rpgreen.x/0.3048
    pressure = pressure/6894.76

    plt.plot(rpgreen.x,pressure)

    plt.show()

