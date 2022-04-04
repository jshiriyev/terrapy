import numpy as np

if __name__ == "__main__":
    import setup

from geomodel.connectivity import SpatProp

TOC = np.ndarray((4,4))

TOC[:,0] = np.array([32,28,12,18])
TOC[:,1] = np.array([24,20,16,12])
TOC[:,2] = np.array([20,17,10,7])
TOC[:,3] = np.array([10,12,9,8])

# variogram()

prop = SpatProp(TOC,dX=10,dY=10)

prop.set_connection()

# Var.set_experimental()
# Var.set_experimental(azimuth=90,azimuthtol=20,bandwidth=2)
prop.set_experimentalVariogram(azimuth=45,azimuthtol=22.5,bandwidth=5)

print(prop.lag)
print(prop.lagtol)
print(prop.lagmax)
print(prop.outbound)

print(prop.azimuth)
print(prop.azimuthtol)
print(prop.bandwidth)

print(prop.bins_experimental)
print(prop.experimental)
