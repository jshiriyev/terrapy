import numpy as np

if __name__ == "__main__":
    import setup

from stream.items import SpatProp

from geomodel.connectivity import variogram

TOC = np.ndarray((4,4))

TOC[:,0] = np.array([32,28,12,18])
TOC[:,1] = np.array([24,20,16,12])
TOC[:,2] = np.array([20,17,10,7])
TOC[:,3] = np.array([10,12,9,8])

# variogram()

prop = SpatProp(TOC,dX=10,dY=10)

prop.set_connection()

Var = variogram(prop)

# Var.set_experimental()
# Var.set_experimental(azimuth=90,azimuthtol=20,bandwidth=2)
Var.set_experimental(azimuth=45,azimuthtol=22.5,bandwidth=5)

print(Var.lag)
print(Var.lagtol)
print(Var.lagmax)
print(Var.outbound)

print(Var.azimuth)
print(Var.azimuthtol)
print(Var.bandwidth)

print(Var.bins_experimental)
print(Var.experimental)