import numpy as np

if __name__ == "__main__":
    import setup

from stream.items import SpatProp

from geomodel.connectivity import variogram

from geomodel.spatial import kriging

obsprop = SpatProp(np.array([0.25,0.43,0.56]),X=np.array([600,400,800]),Y=np.array([800,700,100]))
obsprop.set_connection()
estprop = SpatProp(shape=(1,),X=np.array([500]),Y=np.array([500]))
estprop.set_connection()

Var = variogram(obsprop,estprop)

Var.set_theoretical()

kriging(Var)


##Var.set_experimental(azimuth=45,azimuthtol=22.5,bandwidth=5)
##
##print(Var.lag)
##print(Var.lagtol)
##print(Var.lagmax)
##print(Var.outbound)
##
##print(Var.azimuth)
##print(Var.azimuthtol)
##print(Var.bandwidth)
##
##print(Var.bins_experimental)
##print(Var.experimental)
