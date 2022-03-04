import matplotlib.pyplot as plt
import numpy as np

from scipy.special import expi

from scipy.sparse import csr_matrix as csr

from scipy.special import j0 as bessel_j0
from scipy.special import j1 as bessel_j1
from scipy.special import y0 as bessel_y0
from scipy.special import y1 as bessel_y1

from scipy.special import jvp as bessel_jvp
from scipy.special import yvp as bessel_yvp

from scipy.optimize import root_scalar

if __name__ == "__main__":
    import setup

from stream.items import get_PorRock
from stream.items import get_Fluids
from stream.items import get_Wells

class transient():

    # Line source solution based on exponential integral

    def __init__(self,flow_rate):

        self.PorRock = get_PorRock("ellipse")()
        
        # There can be two slightly compressible fluids where the
        # second one is at irreducible saturation, not mobile

        self.Fluids = get_Fluids()(number=2)

        self.Well = get_Wells()(number=1)

        self.Well.set_flowconds("rate",flow_rate,"oil")

    def initialize(self,pressure0,Swirr=0):

        self.pressure0 = pressure0

        self.Sw = Swirr

        self.So = 1-self.Sw

        coSo = self.Fluids.compressibility[0]*self.So
        cwSw = self.Fluids.compressibility[1]*self.Sw

        self.compressibility = coSo+cwSw+self.PorRock.compressibility

        cons = self.PorRock.porosity*self.Fluids.viscosity[0]*self.compressibility

        self.diffusivity = (self.PorRock.permeability)/(cons)

    def set_tmin(self):

        # setting mimimum time limit because of the wellbore size

        self.tmin = 100*self.Well.radii**2/self.diffusivity

        return self.tmin

    def set_tmax(self,tmax=None):

        # setting maximum time limit because of the external flow radius
        # if the tmax is provided, new external radius will be calculated 

        if tmax is None:
            tmax = 0.25*self.PorRock.radii[0]**2/self.diffusivity
        else:
            tmax_radius = float(np.sqrt(tmax*self.diffusivity/0.25))
            self.PorRock.set_radii(tmax_radius)

        self.tmax = tmax

        return self.tmax

    def set_times(self,times):

        self.times = times.reshape((1,-1))

    def set_observers(self,observers=None,number=50):

        if observers is not None:
            self.observers = observers
        else:
            self.observers = np.linspace(
                self.Well.radii[0],
                self.PorRock.radii[0],
                number)

        self.observers = self.observers.reshape((-1,1))

    def solve(self):

        rateNumer = self.Well.limits*self.Fluids.viscosity[0]
        rateDenom = self.PorRock.permeability*self.PorRock.thickness

        constRate = (rateNumer)/(2*np.pi*rateDenom)

        bound_int = (self.times>=self.tmin)[0]
        bound_ext = (self.times<=self.tmax)[0]

        validTime = np.logical_and(bound_int,bound_ext)

        if np.any(~bound_int):
            raise Warning("Not all times satisfy the early time limits!")

        if np.any(~bound_ext):
            raise Warning("Not all times satisfy the late time limits!")

        Ei = expi(-(self.observers**2)/(4*self.diffusivity*self.times))

        self.deltap = -1/2*constRate*Ei

        self.pressure = self.pressure0-self.deltap

class steady():

    def __init__(self,PorRock,fluid,well):

        self.permeability       = PorRock.permeability
        self.porosity           = PorRock.porosity
        self.viscosity          = fluid.viscosity
        self.compressibility    = PorRock.compressibility+fluid.compressibility

        self.hdiffusivity       = self.permeability/(self.porosity*self.viscosity*self.compressibility)

        self.flowrate          = well.flowrate

        self.radius_int         = well.radii
        self.radius_ext         = PorRock.lengths[0]

        self.thickness          = PorRock.lengths[2]

    def set_observers(self,point_num):

        if self.geometry == "rectangular":

            pass

        elif self.geometry == "cylindrical":

            self.spacepoints = np.linspace(self.radius_int,self.radius_ext)
            self.spacepoints = self.spacepoints.reshape((-1,1))

        elif self.geometry == "unstructured":

            pass

    def solve(self,radius,time,flow_rate):

        dimensionless = (self.flowrate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        self.deltap = dimensionless*np.log(self.radius_ext/self.spacepoints)

class pseudosteady():

    def __init__(self,PorRock,fluid,well):

        self.permeability       = PorRock.permeability
        self.porosity           = PorRock.porosity
        self.viscosity          = fluid.viscosity
        self.compressibility    = PorRock.compressibility+fluid.compressibility

        self.hdiffusivity       = self.permeability/(self.porosity*self.viscosity*self.compressibility)

        self.flowrate           = well.flowrate

        self.radius_int         = well.radii
        self.radius_ext         = PorRock.lengths[0]

        self.thickness          = PorRock.lengths[2]

        self.timelimit          = 0.1*np.pi*self.radius_ext**2/self.hdiffusivity

    def discretize(self,point_num):

        if self.geometry == "rectangular":

            pass

        elif self.geometry == "cylindrical":

            self.spacepoints = np.linspace(self.radius_int,self.radius_ext)
            self.spacepoints = self.spacepoints.reshape((-1,1))

        elif self.geometry == "unstructured":

            pass

    def set_time(self,timesteps):

        self.timesteps = timesteps.reshape((1,-1))

    def solve(self,radius,time,flow_rate):

        self.radius = radius.reshape((-1,1))

        self.time = time.reshape((1,-1))

        self.flow_rate = flow_rate

        self.pdim = (self.flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        self.deltap = np.zeros((self.radius.size,self.time.size))

        valid_ps = (self.time>self.time_limit_ps)[0]

        Sd = np.log(self.radius_ext/self.radius)

        Td = (self.radius**2+4*self.hdiffusivity*self.time[0,valid_ps])/(2*self.radius_ext**2)

        self.deltap[:,valid_ps] = self.pdim*(Sd+Td-3/4)

class everdingen():

    def __init__(self,rr,tt,RR,num_of_terms=2):

        self.rr = toarray(rr)
        self.tt = toarray(tt)

        self.RR = RR

        self.num = num_of_terms

        self.find_roots()

    def find_roots(self):

        roots = np.empty(self.num)

        for idx in range(self.num):

            lower_bound = ((2*idx+1)*np.pi)/(2*self.RR-2)
            upper_bound = ((2*idx+3)*np.pi)/(2*self.RR-2)

            bracket = (lower_bound,upper_bound)

            solver = root_scalar(self.root_function,method="brentq",bracket=bracket)

            roots[idx] = solver.root

        self.beta_n = roots

    def root_function(self,beta):

        """
        This is the function that outputs values of root function 
        defined in Everdingen solution. At singularity point of \beta = 0,
        it outputs its value at limit.
        """

        beta = toarray(beta)

        res = np.empty(beta.shape)

        res[beta==0] = -self.RR/np.pi+1/(np.pi*self.RR)

        J1B = bessel_j1(beta[beta>0])
        Y1B = bessel_y1(beta[beta>0])
        
        J1BR = bessel_j1(beta[beta>0]*self.RR)
        Y1BR = bessel_y1(beta[beta>0]*self.RR)
        
        res[beta>0] = J1BR*Y1B-J1B*Y1BR

        return res

    def root_function_first_derivative(self,beta):

        """
        it needs a treshold value of beta and two functions to calculate analytical values,
        one close to singularity \beta=0, the other at larger values of \betta
        """

        J1B = bessel_j1(beta)
        Y1B = bessel_y1(beta)
        
        J1BR = bessel_j1(beta*self.RR)
        Y1BR = bessel_y1(beta*self.RR)

        J1B_prime = bessel_jvp(1,beta)
        Y1B_prime = bessel_yvp(1,beta)

        J1BR_prime = self.RR*bessel_jvp(1,beta*self.RR)
        Y1BR_prime = self.RR*bessel_yvp(1,beta*self.RR)

        return J1BR_prime*Y1B+J1BR*Y1B_prime-J1B_prime*Y1BR-J1B*Y1BR_prime

    def root_function_first_derivative_numerical(self,beta):
       
        num = beta.shape[0]

        idx = list(range(num))

        idx_diag_ends = np.array([0,num-1])

        Amatrix = csr((num,num))

        Amatrix += csr((np.ones(num-1),(idx[:-1],idx[1:])),shape=(num,num))
        Amatrix -= csr((np.ones(num-1),(idx[1:],idx[:-1])),shape=(num,num))
        Amatrix += csr((np.array([-1,1]),(idx_diag_ends,idx_diag_ends)),shape=(num,num))

        y_prime = Amatrix*root_function(beta,self.RR)
        x_prime = Amatrix*beta

        return y_prime/x_prime

    def solve(self):

        dist = self.rr.reshape((-1,1,1))
        time = self.tt.reshape((1,-1,1))
        beta = self.beta_n.reshape((1,1,-1))

        term1 = 2/(self.RR**2-1)*((dist[:,:,0]**2)/4.+time[:,:,0])
        term2 = self.RR**2/(self.RR**2-1)*np.log(dist[:,:,0])
        term3 = 3*self.RR**4-4*self.RR**4*np.log(self.RR)-2*self.RR**2-1
        term4 = 4*(self.RR**2-1)**2

        term5 = (bessel_j1(beta*self.RR))**2*np.exp(-(beta**2)*time)
        term6 = bessel_j1(beta)*bessel_y0(beta*dist)-bessel_y1(beta)*bessel_j0(beta*dist)
        term7 = beta*((bessel_j1(beta*self.RR))**2-(bessel_j1(beta))**2)

        term8 = term5*term6/term7

        self.PP = term1-term2-term3/term4+np.pi*term8.sum(axis=2)
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)
