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

        self.permeability = permeability
        self.porosity = porosity
        self.viscosity = viscosity
        self.compressibility = compressibility

        self.eta = self.permeability/(self.porosity*self.viscosity*self.compressibility)

        self.rw = well_radius
        self.re = reservoir_radius

        self.thickness = thickness

        self.time_limit_rw = 100.*self.rw**2/self.eta
        self.time_limit_tr = 0.25*self.re**2/self.eta
        self.time_limit_ps = 0.1*np.pi*self.re**2/self.eta

    def solve(self,radius,time,flow_rate):

        self.radius = radius.reshape((-1,1))

        self.time = time.reshape((1,-1))

        self.flow_rate = flow_rate

        self.pdim = (self.flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        self.deltap = np.zeros((self.radius.size,self.time.size))

        self.transient()
        self.steady()
        self.pseudosteady()

    def transient(self):

        finite_rw = (self.time>self.time_limit_rw)[0]
        finite_re = (self.time<self.time_limit_tr)[0]

        valid_tr = np.logical_and(finite_rw,finite_re)

        Ei = expi(-(self.radius**2)/(4*self.eta*self.time[0,valid_tr]))

        self.deltap[:,valid_tr] = -1/2*self.pdim*Ei

    def steady(self):

        self.deltap_steady = self.pdim*np.log(self.re/self.radius)

    def pseudosteady(self):

        valid_ps = (self.time>self.time_limit_ps)[0]

        Sd = np.log(self.re/self.radius)

        Td = (self.radius**2+4*self.eta*self.time[0,valid_ps])/(2*self.re**2)

        self.deltap[:,valid_ps] = self.pdim*(Sd+Td-3/4)
        
if __name__ == "__main__":

    q = -350*1.13/6.2898/86400

    pi = 3000*6894.76

    radius = np.linspace(0.333,3500,1000)*0.3048

    well_radius = 0.333*0.3048

    reservoir_radius = 3500*0.3048
    
    time = np.array([0.1,1,40,100])*86400

    permeability = 25*9.869233e-16

    porosity = 0.16

    thickness = 50*0.3048

    viscosity = 0.5e-3

    totalCompressibility = 2e-5/6894.76

    ls = radial(permeability,porosity,viscosity,totalCompressibility,well_radius,reservoir_radius,thickness)

    ls.solve(radius,time,q)

    plt.plot(radius,(pi+ls.deltap[:,0])/6894.76,label="at {} days".format(time[0]/86400))
    plt.plot(radius,(pi+ls.deltap[:,1])/6894.76,label="at {} days".format(time[1]/86400))
    plt.plot(radius,(pi+ls.deltap[:,2])/6894.76,label="at {} days".format(time[2]/86400))
    plt.plot(radius,(pi+ls.deltap[:,3])/6894.76,label="at {} days".format(time[3]/86400))

    plt.xlabel("radius [m]")
    plt.ylabel("pressure [psi]")

    plt.legend()

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
