import numpy as np

import matplotlib.pyplot as plt

from scipy import optimize

def objective1(wc,L,D,phi):

    Lc = critical(wc,D,phi)

    return (L-Lc)**2

def critical(wc,D,phi):
    # it returns length of pipe
    return ((1/wc)**2-(np.log(1/wc))**2-1)*D/(8*phi)

def objective2(P2,G,P1,L,D,M,T):

    Gc = massflow(P2,P1,L,D,M,T)

    return (G-Gc)**2

def massflow(P2,P1,L,D,M,T):

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
