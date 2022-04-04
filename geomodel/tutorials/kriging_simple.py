import numpy as np

if __name__ == "__main__":
    import setup

from geomodel.connectivity import SpatProp

from geomodel.estimation import kriging

obsprop = SpatProp(np.array([0.25,0.43,0.56]),X=np.array([600,400,800]),Y=np.array([800,700,100]))
obsprop.set_connection()

obsprop.set_theoreticalVariogram(vsill=0.0025,vrange=700)

estprop = SpatProp(shape=(1,),X=np.array([500]),Y=np.array([500]))
estprop.set_connection()

K = kriging(obsprop,estprop)

K.simple(0.38)

print(K.get_percentile(0.1))
print(K.get_percentile(0.5))
print(K.get_percentile(0.9))


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
