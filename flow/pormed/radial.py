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

from interfaces.items import formation

def toarray(x):

    if isinstance(x,int) or isinstance(x,float):
        x = np.array([x])
    elif isinstance(x,list) or isinstance(x,tuple):
        x = np.array(x)

    return x

class transient(self):

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

    def set_total_compressibility(self):

        self.ct = formation.compressibility+fluid.compressibility

    def set_diffusivity(self,viscosity,total_compressibility):
        
        self.eta = (self.k)/(self.phi*self.mu*self.ct)

    def solve(self,radius,time,flow_rate):

        self.radius = radius.reshape((-1,1))

        self.time = time.reshape((1,-1))

        self.flow_rate = flow_rate

        self.pdim = (self.flow_rate*self.viscosity)/(2*np.pi*self.permeability*self.thickness)

        self.deltap = np.zeros((self.radius.size,self.time.size))

        finite_rw = (self.time>self.time_limit_rw)[0]
        finite_re = (self.time<self.time_limit_tr)[0]

        valid_tr = np.logical_and(finite_rw,finite_re)

        Ei = expi(-(self.radius**2)/(4*self.eta*self.time[0,valid_tr]))

        self.deltap[:,valid_tr] = -1/2*self.pdim*Ei

class steady(self):

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

        self.deltap_steady = self.pdim*np.log(self.re/self.radius)

class pseudosteady(self):

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

        valid_ps = (self.time>self.time_limit_ps)[0]

        Sd = np.log(self.re/self.radius)

        Td = (self.radius**2+4*self.eta*self.time[0,valid_ps])/(2*self.re**2)

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