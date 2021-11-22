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

def toarray(x):

    if isinstance(x,int) or isinstance(x,float):
        x = np.array([x])
    elif isinstance(x,list) or isinstance(x,tuple):
        x = np.array(x)

    return x

class singlephase():

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

    ls = singlephase(permeability,porosity,viscosity,totalCompressibility,well_radius,reservoir_radius,thickness)

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


# EVERDINGEN SOLUTION

    # # beta = np.logspace(-4,3,10000)
    # # u = 1e-2
    # # print(bessel_y0(u))
    # # print(2/np.pi*(np.log(u)-0.11593))
    # # beta = np.linspace(0,100,100000)

    # # tt = np.linspace(8,40)
    # # tt = [0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.35,0.4,0.45,0.5,0.55,0.6]
    # # tt = np.linspace(0.22,0.5,15)
    # # tt = np.linspace(0.4,0.6,11)
    # # tt = np.linspace(1,2,11)
    # # tt = np.linspace(1e-2,50,9)

    # tt = [1e-2,1e-1,0.5,3,5,7,9,11,16]

    # # print(tt)

    # sol = everdingen(rr=np.linspace(1,10,10000),tt=tt,RR=10,num_of_terms=100) #np.linspace(1,10)

    # sol.solve()

    # # print(sol.beta_n)
    # # print(sol.PP[0,:])

    # plt.plot(sol.rr,sol.PP[:,0])
    # plt.plot(sol.rr,sol.PP[:,1])
    # plt.plot(sol.rr,sol.PP[:,2])
    # plt.plot(sol.rr,sol.PP[:,3])
    # plt.plot(sol.rr,sol.PP[:,4])
    # plt.plot(sol.rr,sol.PP[:,5])
    # plt.plot(sol.rr,sol.PP[:,6])
    # plt.plot(sol.rr,sol.PP[:,7])
    # plt.plot(sol.rr,sol.PP[:,8])

    # plt.gca().invert_yaxis()

    # plt.show()

    # # J0B = bessel_j0(sol.beta_n)
    # # Y0B = bessel_y0(sol.beta_n)

    # # J1B = bessel_j1(sol.beta_n)
    # # Y1B = bessel_y1(sol.beta_n)

    # # print(np.pi*sol.beta_n*(J1B*Y0B-Y1B*J0B))

    # # print(sol.beta_n)

    # # # P = everdingen(10,1000)

    # # # f0 = P.root_function(R)
    # # # f1A = P.root_function_first_derivative(R)
    # # # f1N = P.root_function_first_derivative_numerical(R)

    # # # f0 = root_function(1e-20)

    # # # x, fsol, XE, FE, IE = odelay(root_function_first_derivative,f0,beta, events=[e1])

    # # # print(C[0]+D[0])

    # # # plt.plot(beta,root_function(beta,R),label="root function")
    # # # plt.plot(beta,root_function_first_derivative_numerical(beta,R),label="root function derivative")

    # # plt.plot(beta,sol.root_function(beta),label="J1BR*Y1B-J1B*Y1BR")
    # # plt.plot(sol.beta_n,np.zeros(sol.beta_n.shape),'.')
    # # # plt.semilogy(beta,np.abs(B),label="J1BR*Y1B_prime")
    # # # plt.semilogy(beta,np.abs(C),label="J1B_prime*Y1BR")
    # # # plt.semilogy(beta,np.abs(D),label="J1B*Y1BR_prime")

    # # # plt.axhline(y=0,xmin=beta[0],xmax=beta[-1],color='k')
    # # # plt.axhline(y=0,xmin=np.log10(beta[0]),xmax=np.log10(beta[-1]),color='k')

    # # # plt.ylim([-10,10])

    # # # plt.legend()

    # plt.show()