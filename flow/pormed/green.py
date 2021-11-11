import numpy as np 

if __name__ == "__main__":
    import setup

from flow.pormed.elliptictheta import kind3

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

