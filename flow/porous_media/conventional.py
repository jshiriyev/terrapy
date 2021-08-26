import io
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from scipy.special import expi

class radial():

    """
    line source solution based on exponential integral
    """

    def __init__(self,permeability,porosity,viscosity,compressibility,well_radius,reservoir_radius,thickness):

        self.permeability       = permeability
        self.porosity           = porosity
        self.viscosity          = viscosity
        self.compressibility    = compressibility

        self.eta                = self.permeability/(self.porosity*self.viscosity*self.compressibility)

        self.rw                 = well_radius
        self.re                 = reservoir_radius

        self.thickness          = thickness

    def transient(self,radius,time,flow_rate):

        radius = radius.reshape((-1,1))
        time = time.reshape((1,-1))

        Pd = (flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)
        Ei = expi(-(radius**2)/(4*self.eta*time))

        self.deltap = -1/2*Pd*Ei

        early_time_period = 100.*self.rw**2/self.eta
        late_time_period = 0.25*self.re**2/self.eta

        finiteWellEffect = (time>early_time_period)[0]
        finiteBoundaryEffect = (time<late_time_period)[0]

        self.invalidTimeInterval = ~np.logical_and(finiteWellEffect,finiteBoundaryEffect)

        self.deltap[:,self.invalidTimeInterval] = np.nan

    def steady(self,radius,flow_rate):

        Pd = (flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        self.deltap = Pd*np.log(re/radius)

    def pseudosteady(self,radius,time,flow_rate,CA=31.6):

        Area = np.pi*self.re**2/4

        gamma = 1.781

        Pd = (flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        Cd = 1/2*np.log((4*Area)/(gamma*CA*self.rw**2))

        Td = 2*np.pi*self.eta*time/Area

        self.deltap = Pd*(Cd+Td)
        
if __name__ == "__main__":

    q = -350*1.13/6.2898/86400

    pi = 3000*6894.76

    radius = np.logspace(-1,3.5,1000)*0.3048

    # radius = np.array([0.333,1,10,100,1000,3160])*0.3048

    well_radius = 0.333*0.3048

    reservoir_radius = 10000*0.3048

    time = np.array([0.1,1,10])*86400

    permeability = 25*9.869233e-16

    porosity = 0.16

    thickness = 50*0.3048

    viscosity = 0.5e-3

    totalCompressibility = 2e-5/6894.76

    ls = radial(permeability,porosity,viscosity,totalCompressibility,well_radius,reservoir_radius,thickness)

    ls.transient(radius,time,q)

    plt.plot(radius,(pi+ls.deltap[:,0])/6894.76)
    plt.plot(radius,(pi+ls.deltap[:,1])/6894.76)
    plt.plot(radius,(pi+ls.deltap[:,2])/6894.76)

    plt.show()

    # For the unittest of transient

    # radius = np.array([0.333,1,10,100,1000,3160])*0.3048
    # time = np.array([0.1,1,10])*86400

    # (pi+ls.deltap[0,:])/6894.76 must be np.array([2863.65131155 2837.93292432 2812.21451173])
    # (pi+ls.deltap[1,:])/6894.76 must be np.array([2888.21501563 2862.49685438 2836.77846438])
    # (pi+ls.deltap[2,:])/6894.76 must be np.array([2939.62390686 2913.93088964 2888.21501563])
    # (pi+ls.deltap[3,:])/6894.76 must be np.array([2988.43390321 2965.08992094 2939.62390686])
    # (pi+ls.deltap[4,:])/6894.76 must be np.array([3000.         2999.73190234 2988.43390321])
    # (pi+ls.deltap[5,:])/6894.76 must be np.array([3000.         3000.         2999.7306157 ])

    # import unittest

    # from tests import test_porous_media

    # unittest.main(test_porous_media)
