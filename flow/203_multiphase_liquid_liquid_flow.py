import numpy as np

from scipy import optimize

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

