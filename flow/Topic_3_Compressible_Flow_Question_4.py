import numpy as np

import matplotlib.pyplot as plt

from scipy import optimize

def objective1(phi,Re):
##    return phi**(-0.5)+2*np.log10(2.51*phi**(-0.5)/Re)
    return phi**(-0.5)-2.5*np.log(Re*phi**(0.5))-3

def objective2(P1,P2,L,D,phi,M,T,G):
    Gc = massflow(P1,P2,L,D,phi,M,T)
    return (G-Gc)**2

def objective3(wc,L,D,phi):
    LHS = (8*phi)*L/D
    RHS = (1/wc)**2-(np.log(1/wc))**2-1
    return (LHS-RHS)**2

def massflow(P1,P2,L,D,phi,M,T):

    rho1 = (P1*M)/(8.314*T)
    rho2 = (P2*M)/(8.314*T)

    nu1 = 1/rho1
    nu2 = 1/rho2

    Dp = P1**2-P2**2
    Rp = np.log(P1/P2)
    Ft = 4*phi*L/D

    A = np.pi*D**2/4
    
    G = A*np.sqrt(Dp/(2*P1*nu1*(Rp+Ft)))

    return G

"""
## Gas properties
"""

Pg = 101325         # pressure in Pa
Tg = 288            # Kelvin
Mg = 0.016          # mole mass in kg/mole

Qg = 50             # m3/second

Rc = 8.314          # universal gas constant in SI units

mu = 0.01e-3        # Pascal-second

"""
## Pipeline Properties
"""

P2 = 170e3          # Pascal

L = 3*1000          # meters
D = 0.6             # meters

Tamb = 293          # ambient temperature, Kelvin

"""
## END OF INPUTS
"""

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
