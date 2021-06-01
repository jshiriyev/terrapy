import numpy as np

from scipy import optimize

from scipy.optimize import Bounds

import matplotlib.pyplot as plt
"""tank flow through noozle"""

class single_phase():
    
    def compressible(self,P2,P1,v1,k,Cd,A):

        Pr = P2/P1
        kr = (k-1)/k

        Sq = 2/kr*P1*v1*(1-Pr**kr)

        G = Cd*A/v1*Pr**(1/k)*np.sqrt(Sq)

        return G #mass flow

def obj(P2,P1,v1,k,G,Cd,A):

    sp = single_phase()

    Gc = sp.compressible(P2,P1,v1,k,Cd,A)

    return np.abs(G-Gc)

P1 = 500*1e3    # Pa

v1 = 16.18      # m^3/kg

k = 2

Cd = 1

##ID = 2.54*1e-3                       # m

ID = np.array([2.4,2.0,2.13,2.26,2.39,2.52,2.66,2.79])*1e-3  # mm

A = (np.pi*ID**2)/4 # m^2

##P2 = np.linspace(0,500000,50000)

##G = massflow(P2,P1,v1,k,Cd,A)

##plt.semilogx(P2,G)
##plt.show()

wc = (2/(k+1))**(k/(k-1))

G = 0.0004      # kg/s

##P2 = optimize.minimize(obj,P1,
##              bounds=[(wc*P1,P1)],
##              method="Powell",
##              args=(P1,v1,k,G,Cd,A)).x
##
##print(P2)

P2 = np.empty_like(A)

for i,Ai in enumerate(A):
    P2[i] = optimize.minimize(obj,P1,
              bounds=[(wc*P1,P1)],
              method='Powell',
              args=(P1,v1,k,G,Cd,Ai)).x

x = np.linspace(1,8,8)

plt.plot(x,P2)
plt.show()
