import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr

## from PIL import Imag

if __name__ == "__main__":
    import setup

class singlephase():

    """
    cartesian_1D_laplace
    cartesian_1D_poisson
    cartesian_1D
    cartesian_2D
    cartesian_3D
    """

    def __init__(self,length,radius,porosity,permeability,compressibility):

        self.length = length
        self.radius = radius

        self.porosity = porosity
        self.permeability = permeability
        self.compressibility = compressibility

    def set_hydraulic_diffusivity(self,hydraulic_diffusivity=None):

        if hydraulic_diffusivity is not None:
            self.eta = hydraulic_diffusivity

    def cartesian_laplace_1D(self):
        pass

    def cartesian_poisson_1D(self,boundary_points,boundary_conditions,beta):
        
        """
        lower_boundary is a list of boundary conditions specified for xL
        upper_boundary is a list of boundary conditions specified for xU

        they contain following entries

        x  : x-value of boundary line
        d  : Dirichlet boundary condition coefficient
        n  : Neumann boundary condition coefficient
        f  : scalar function known for the boundary    
        """

        xL = boundary_points[0]
        dL = boundary_conditions[0][0]
        nL = boundary_conditions[0][1]
        fL = boundary_conditions[0][2]

        xU = boundary_points[1]    
        dU = boundary_conditions[1][0]
        nU = boundary_conditions[1][1]
        fU = boundary_conditions[1][2]

        rhs = np.zeros((2,1))

        rhs[0,0] = fL-dL*beta/2*xL**2-nL*beta*xL
        rhs[1,0] = fU-dU*beta/2*xU**2-nU*beta*xU

        mat = np.zeros((2,2))

        mat[0,0] = dL*xL+nL
        mat[0,1] = dL
        mat[1,0] = dU*xU+nU
        mat[1,1] = dU

        coeff = np.linalg.solve(mat,rhs)

        c1 = coeff[0]
        c2 = coeff[1]

        dx = (xU-xL)/1000
        
        self.x = np.arange(xL,xU+dx/2,dx)

        self.u = beta/2*self.x**2+c1*self.x+c2

    def cartesian_1D(self,boundary_points,boundary_pressures,time,x=None,N=50):

        xL = boundary_points[0]
        xU = boundary_points[1]

        PL = boundary_pressures[0][2]
        PU = boundary_pressures[1][2]

        L = xU-xL

        self.time = time.reshape((-1,1,1))

        if x is None:
            self.x = np.linspace(xL,xU).reshape((1,-1,1))
        else:
            self.x = x.reshape((1,-1,1))

        n = np.arange(1,N).reshape((1,1,-1))

        sin_ = np.sin(n*np.pi*self.x/L)
        exp_ = np.exp(-n**2*np.pi**2/L**2*self.eta*self.time)

        sum_ = np.sum(1/n*exp_*sin_,axis=2)

        self.pressure = PL+(PU-PL)*((self.x/L).reshape((1,-1))+2/np.pi*sum_)

class multicomponent():

    def obj(XX,v,L):

        data = np.loadtxt('Lecture_08_dispersion.txt',skiprows=1)

        time = data[:,0]*3600   # seconds

        cc0_c = concentration(XX,v,L,time)

        cc0 = data[:,1]         # dimensionless

        return np.sum((cc0-cc0_c)**2)

    def concentration(XX,v,L,time):

        DL = 10**(-XX)

        term1 = erfc((L-v*time)/(2*np.sqrt(DL*time)))
        term2 = erfc((L+v*time)/(2*np.sqrt(DL*time)))

        if not np.any(term2):
            return 1/2*(term1)
        else:
            term3 = np.exp(v*L/DL)
            return 1/2*(term1+term2*term3)
        
class multiphase():
    
    """
    based on Buckley-Leverett solution
    """

    def __init__(self,Sor,Swr,muo,muw):

        self.Sor = Sor
        self.Swr = Swr
        self.muo = muo
        self.muw = muw

    def k_model(self):

        N = 1000

        self.Sw = np.linspace(self.Swr,1-self.Sor,N)

        self.kro = 2*(1-self.Sw-self.Sor)**2
        self.krw = (self.Sw-self.Swr)**3

    def coreymodel(self,koro,korw,m,n):

        N = 1000

        self.Sw = np.linspace(self.Swr,1-self.Sor,N)

        S = (self.Sw-self.Swr)/(1-self.Swr-self.Sor)

        self.kro = koro*(1-S)**m
        self.krw = korw*S**n

        ## end-point mobility ratio calculation
        self.Mo = (korw/self.muw)/(koro/self.muo)

    def fractionalflow(self):

        self.fw = (self.krw*self.muo)/(self.krw*self.muo+self.kro*self.muw)
        
        N = self.fw.size

        one = np.ones(N-1)

        idx = np.array(list(range(N)))

        row = np.concatenate(((idx[0],idx[-1]),idx[:-1],idx[1:]))
        col = np.concatenate(((idx[0],idx[-1]),idx[1:],idx[:-1]))

        val = np.concatenate(((-1,1),one,-one))

        G = csr((val,(row,col)),shape=(N,N))

        fw_diff = G*self.fw
        Sw_diff = G*self.Sw

        self.fw_der = fw_diff/Sw_diff
        
    def shockfront(self,Swi):

        self.Swi = Swi

        IC = self.Sw>=self.Swi

        ## loosing some data in fw, Sw and fw_der for the saturations below the initial value
        self.fw_IC = self.fw[IC]
        self.Sw_IC = self.Sw[IC]
        ## fw_der_IC is the fw_der corrected for the shock front as well
        self.fw_der_IC = self.fw_der[IC]
        
        self.fwi = self.fw_IC[0]
        
        idx = np.argmax(self.fw_der_IC)

        fw_dh = self.fw_IC[idx:]
        Sw_dh = self.Sw_IC[idx:]

        pseudo_fw_der = (fw_dh-self.fwi)/(Sw_dh-self.Swi)

        self.fwf = fw_dh[np.argmin(np.abs(self.fw_der_IC[idx:]-pseudo_fw_der))]
        self.Swf = Sw_dh[np.argmin(np.abs(self.fw_der_IC[idx:]-pseudo_fw_der))]

        self.Sw_avg = self.Swi+(1-self.fwi)/(self.fwf-self.fwi)*(self.Swf-self.Swi)

        fw_der_corrected = np.empty_like(self.fw_der_IC)

        fw_der_corrected[self.Sw_IC>=self.Swf] = self.fw_der_IC[self.Sw_IC>=self.Swf]
        fw_der_corrected[self.Sw_IC<self.Swf] = self.fw_der_IC[self.Sw_IC==self.Swf]

        self.fw_der_IC = fw_der_corrected

        self.fwf_der = self.fw_der_IC[0]

    def production(self,q,A,L,phi):

        """
        given that L is "ft", A is "ft2", and q is "ft3/day", tp will be in days.
        phi is porosity and is a dimensionless value.
        calculated volumes are normalized with respect to pore volume Vp,
        both produced oil Np and injected water Wi
        """

        v = q/(phi*A)

        self.v = v

        self.Vp = A*L*phi
        
        self.tbt = L/(v*self.fwf_der)

        self.Nbt = v/L*(1-self.fwi)*self.tbt

        self.tp = L/(v*self.fw_der_IC)
        self.Np = (1-self.fw_IC)/(self.fw_der_IC)+self.Sw_IC-self.Swi

        idx = self.tp<=self.tbt

        N = np.count_nonzero(idx)

        self.tp[idx] = np.linspace(0,self.tbt,N)
        self.Np[idx] = v/L*(1-self.fwi)*self.tp[idx]
        
        self.Wi = v/L*self.tp

    def profile(self,q,A,phi,t):

        """
        time is in days assuming that L is "ft", A is "ft2", and q is "ft3/day".
        phi is porosity and is a dimensionless value.
        calculated x_Sw for a saturation profile will be in "ft".
        """

        v = q/(phi*A)        

        self.x_Sw = v*t*self.fw_der_IC

if __name__ == "__main__":

    import unittest

    from flow.pormed.tests import linear

    unittest.main(linear)