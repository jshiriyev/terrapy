import numpy as np

import matplotlib.pyplot as plt

from scipy import optimize

class item():

    def __init__(self):

        pass

    def set_pipe(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):
        
        self.ID = inner_diameter
        self.OD = outer_diameter
        self.length = length
        self.roughness = roughness

        """cross sectional area"""
        self.csa = np.pi*self.ID**2/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius = self.ID/4

        self.roughness_relative = self.roughness/self.ID

    def set_pipe2(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):

        self.ID2 = inner_diameter
        self.OD2 = outer_diameter
        self.length2 = length
        self.roughness2 = roughness

        """cross sectional area"""
        self.csa2 = np.pi*(self.ID2**2-self.OD**2)/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius2 = (self.ID2-self.OD)/4

        self.roughness_relative2 = self.roughness2/self.ID2

    def set_nodes(self,zloc=None,elevation=[0,0]):

        """
        Nodes are the locations where the measurements are available, and
        coordinates are selected in such a way that:
        - r-axis shows radial direction
        - \theta-axis shows angular direction
        - z-axis shows lengthwise direction
        """

        if zloc is None:
            self.zloc = [0,self.length]

        self.elevation = elevation
        

class single_phase(item):

    def __init__(self):

        pass

    def incompressible(self):

        pass

    def DarcyWeisbach(self,NReynolds,tol=1e-5):

        if NReynolds<1000:
            
            f = 64/NReynolds
            
        elif NReynolds>2000:
            
            f0 = 64/NReynolds
            
            converged = False
            
            while not converged:
                
                Lp = (self.epsilon)/(3.7*self.diameter)
                Rp = (2.51)/(NReynolds*np.sqrt(f0))
                
                f1 = 1/(-2*np.log10(Lp+Rp))**2
                
                if np.abs(f1-f0)>tol:
                    
                    f0 = f1
                    
                else:
                    
                    converged = True

    def HazenWilliams(self,C=120):

        k = 0.849

        Rhydraulic = self.diameter/4

        f = ((self.average_velocity)/(k*C*Rhydraulic**0.63))**(1/0.54)

    def compressible(self,P2,P1,L,D,M,T):

        """G is mass flow rate"""

        rho1 = (P1*M)/(8.314*(T+273.15))
        rho2 = (P2*M)/(8.314*(T+273.15))

        nu1 = 1/rho1
        nu2 = 1/rho2

        Dp = P1**2-P2**2
        Rp = np.log(P1/P2)
        Ft = 4*phi*L/D

        A = np.pi*D**2/4
        
        G = A*np.sqrt(Dp/(2*P1*nu1*(Rp+Ft)))

        return G

    def compressible_critical(self,wc,D,phi):
        # it returns length of pipe
        return ((1/wc)**2-(np.log(1/wc))**2-1)*D/(8*phi)

class multiphase(item):

    """liquid-liquid flow"""
    
    def obj(phi,Re):
        x1 = np.power(phi,0.5)
        x2 = np.power(phi,-0.5)
        return x2-(2.5*np.log(Re*x1)+0.3)

    Qo = 10/60          # meter^3/sec
    Qw = 5/60           # meter^3/sec

    rhoo = 890         # kg/meter^3
    rhow = 1020        # kg/meter^3

    muo = 3.5e-3        # Pa.s
    muw = 1.1e-3        # Pa.s

    L = 10              # meter
    D = 0.2032          # meter

    C1 = 60.
    C2 = 1.28

    vo = (4*Qo)/(np.pi*D**2)   # meter/second
    vw = (4*Qw)/(np.pi*D**2)   # meter/second

    Reo = (rhoo*vo*D)/muo
    Rew = (rhow*vw*D)/muw

    phio0 = 8/Reo
    phiw0 = 8/Rew

    phio = optimize.newton(obj,phio0,args=(Reo,))
    phiw = optimize.newton(obj,phiw0,args=(Rew,))

    deltaPo = 4*phio*(L/D)*rhoo*vo**2
    deltaPw = 4*phiw*(L/D)*rhow*vw**2

    Xto2 = deltaPo/deltaPw

    phio2 = C2+C1/Xto2
    phiw2 = C1+C2*Xto2

    deltaP1 = phio2*deltaPo
    deltaP2 = phiw2*deltaPw

    print(deltaP1)
    print(deltaP2)

def objective1(wc,L,D,phi):

    Lc = critical(wc,D,phi)

    return (L-Lc)**2

def objective1(phi,Re):
##    return phi**(-0.5)+2*np.log10(2.51*phi**(-0.5)/Re)
    return phi**(-0.5)-2.5*np.log(Re*phi**(0.5))-3

def objective2(P2,G,P1,L,D,M,T):

    Gc = massflow(P2,P1,L,D,M,T)

    return (G-Gc)**2

def objective3(wc,L,D,phi):
    LHS = (8*phi)*L/D
    RHS = (1/wc)**2-(np.log(1/wc))**2-1
    return (LHS-RHS)**2


if __name__ == "__main__":

    """Class Exercise 1"""

    L = 10*1000         # meters
    D = 12*0.0254       # meters

    P1 = 200*6894.76    # Pascal

    T = 20              # Celsius

    G = 10              # kilogram per seconds

    M = 0.016           # kilogram per moles
    mu = 1.03e-5        # Pascal-second

    A = np.pi*D**2/4

    Re = (G*D)/(mu*A)

    phi = 0.0396/(Re)**0.25

    wc = optimize.minimize_scalar(
                        objective1,
                        args=(L,D,phi),
                        bounds=((1e-5,1)),
                        method='bounded').x

    P2c = P1*wc

    print("P2 is %5.1f psi when G is maximum" %(P2c/6894.76))

    ##G = massflow(161.5*6894.76,P1,L,D,M,T)

    P2 = optimize.minimize_scalar(
                        objective2,
                        args=(G,P1,L,D,M,T),
                        bounds=((P2c,P1)),
                        method='bounded').x

    print("P2 is %5.1f psi when G is 10 kg/sec" %(P2/6894.76))

    rho1 = (P1*M)/(8.314*(T+273.15))
    rho2 = (P2*M)/(8.314*(T+273.15))

    u1 = G/(rho1*A)
    u2 = G/(rho2*A)

    """ Class Exercise 2 """

    Pg = 101325         # pressure in Pa
    Tg = 288            # Kelvin
    Mg = 0.016          # mole mass in kg/mole

    Qg = 50             # m3/second

    Rc = 8.314          # universal gas constant in SI units

    mu = 0.01e-3        # Pascal-second

    P2 = 170e3          # Pascal

    L = 3*1000          # meters
    D = 0.6             # meters

    Tamb = 293          # ambient temperature, Kelvin

    rhog = (Pg*Mg)/(Rc*Tg)

    G = Qg*rhog         # kilogram per seconds

    A = np.pi*D**2/4

    Re = (G*D)/(mu*A)

    phi = optimize.newton(objective1,8/Re,args=(Re,))

    P1 = optimize.minimize(objective2,P2,
                           args=(P2,L,D,phi,Mg,Tamb,G),
                           bounds=[(P2,None)],
                           method='Powell').x[0]

    wc = optimize.minimize_scalar(objective3,args=(L,D,phi),bounds=(1e-5,1),method='bounded').x
