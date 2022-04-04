import matplotlib.pyplot as plt

import numpy as np

if __name__ == "__main__":
    import setup

from stream.items import SpatProp

from geomodel.connectivity import variogram

TOC = np.ndarray((8,8))

TOC[:,0] = np.array([15,13,11,10,17,16,15,11])
TOC[:,1] = np.array([17,14,10,13,13,15,14,17])
TOC[:,2] = np.array([18,16,10,16,14,18,20,18])
TOC[:,3] = np.array([19,18,15,15,18,23,22,20])
TOC[:,4] = np.array([21,15,20,18,20,20,18,13])
TOC[:,5] = np.array([22,17,18,19,18,25,20,19])
TOC[:,6] = np.array([23,20,17,20,14,23,21,17])
TOC[:,7] = np.array([26,22,19,14,16,19,16,14])

# variogram()

prop = SpatProp(TOC,dX=1,dY=1)

prop.set_connection()

Var = variogram(prop)

bEEWW,vEEWW = Var.set_experimental(
    lagmax=4,azimuth=0,azimuthtol=10,bandwidth=1,returnFlag=True)
bNNSS,vNNSS = Var.set_experimental(
    lagmax=4,azimuth=90,azimuthtol=10,bandwidth=1,returnFlag=True)
bNESW,vNESW = Var.set_experimental(
    lagmax=6,azimuth=45,azimuthtol=10,bandwidth=1,returnFlag=True)
bNWSE,vNWSE = Var.set_experimental(
    lagmax=6,azimuth=135,azimuthtol=10,bandwidth=1,returnFlag=True)

plt.plot(bEEWW,vEEWW,label="E-W")
plt.scatter(bEEWW,vEEWW)

plt.plot(bNNSS,vNNSS,label="N-S")
plt.scatter(bNNSS,vNNSS)

plt.plot(bNESW,vNESW,label="NE-SW")
plt.scatter(bNESW,vNESW)

plt.plot(bNWSE,vNWSE,label="NW-SE")
plt.scatter(bNWSE,vNWSE)

plt.xlabel("lag distance")
plt.ylabel("semi-variance")

plt.legend()

plt.show()
