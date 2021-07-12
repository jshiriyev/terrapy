import io
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr

from scipy.sparse.linalg import spsolve as sps

from scipy.special import j0
from scipy.special import j1
from scipy.special import y0
from scipy.special import y1

## from PIL import Image

class relative_permeability():

    """

    This Model Provides IMBIBITION Relative Permeability MODELS for a system provided.

    So      = oil saturation
    Sw      = water saturation
    Sg      = gas saturation
    
    MODEL: OIL-WATER system

    Sorow   = residual oil saturation in oil-water system
    Swc     = connate water saturation
    krowc   = oil relative permeability at connate water saturaton
    krwor   = water relative permeability at the residual oil saturation
    no      = oil exponent on relative permeability curve
    nw      = water exponent on relative permeability curve

    MODEL: GAS-OIL system

    Sorgo   = residual oil saturation in gas-oil system
    Swc     = connate water saturation
    Slc     = critical liquid saturation = Swc+Sor
    Sgc     = critical gas saturation
    krogc   = oil relative permeability at critical gas saturation
    krglc   = gas relative permeability at critical liquid saturation
    no      = oil exponent on relative permeability curve
    ng      = gas exponent on relative permeability curve

    MODEL: THREE-PHASE system

    Som     = minimum oil saturation in three-phase system
    Sorow   = residual oil saturtaion in oil-water system
    Sorgo   = residual oil saturation in gas-oil system
    Swc     = connate water saturation
    Sgc     = criticial gas saturation
    krowc   = oil relative permeability at connate water saturaton in oil-water system
    krwor   = water relative permeability at the residual oil saturation in oil-water system
    krogc   = oil relative permeability at critical gas saturation in gas-oil system
    krglc   = gas relative permeability at critical liquid saturation in gas-oil system
    no      = oil exponent on relative permeability curve
    nw      = water exponent on relative permeability curve
    ng      = gas exponent on relative permeability curve
    
    """

    def __init__(self,
                 Sw,
                 Sg=0,
                 Sorow=0.4,
                 Sorgo=0.4,
                 Swc=0.1,
                 Sgc=0.05,
                 krowc=0.8,
                 krwor=0.3,
                 krogc=0.8,
                 krglc=0.3,
                 no=2,
                 nw=2,
                 ng=2,
                 Som=None):

        self.So = 1-Sw-Sg
        self.Sw = Sw
        self.Sg = Sg
        self.Sorow = Sorow
        self.Sorgo = Sorgo
        self.Swc = Swc
        self.Sgc = Sgc
        self.krowc = krowc
        self.krwor = krwor
        self.krogc = krogc
        self.krglc = krglc
        self.no = no
        self.nw = nw
        self.ng = ng

        if Som is None:
            self.Som = self._estimate_Som()
        else:
            self.Som = Som

    def system2phase(self,model="oil-water"):

        if model == "oil-water":
            self.kro,self.krw = self._oil_water()
        elif model == "gas-oil":
            self.kro,self.krg = self._gas_oil()
        
    def _oil_water(self):

        movable_o = self.So-self.Sorow
        movable_w = self.Sw-self.Swc
        movable_l = 1-self.Sorow-self.Swc
        
        kro = self.krowc*(movable_o/movable_l)**self.no
        krw = self.krwor*(movable_w/movable_l)**self.nw

        return kro,krw

    def _gas_oil(self):

        Slc = self.Sorgo+self.Swc
        
        movable_o = 1-Slc-self.Sg
        movable_g = self.Sg-self.Sgc
        movable_f = 1-Slc-self.Sgc

        kro = self.krogc*(movable_o/movable_f)**self.no
        krg = self.krglc*(movable_g/movable_f)**self.ng

        return kro,krg

    def system3phase(self,model="Stone's Model I",n=None):

        if model=="Stone's Model I":
            self.kro,self.krw,self.krg = self._stones_model_I()
        elif model=="Aziz and Settari":
            self.kro,self.krw,self.krg = self._aziz_settari()
        elif model=="Stone's Model II":
            self.kro,self.krw,self.krg = self._stones_model_II()
        elif model=="Hustad-Holt Correlation":
            self.kro,self.krw,self.krg = self._hustad_holt(n)

    def _stones_model_I(self):

        movable_o = self.So-self.Som
        movable_w = self.Sw-self.Swc
        movable_g = self.Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water()
        krogo,krg = self._gas_oil()

        beta_w = (kroow)/(1-Sw_star)
        beta_g = (krogo)/(1-Sg_star)

        kro = So_star*beta_w*beta_g

        return kro,krw,krg

    def _estimate_Som(self):

        alpha = 1-(self.Sg)/(1-self.Swc-self.Sorgo)

        Som = alpha*self.Sorow+(1-alpha)*self.Sorgo

        return Som

    def _aziz_settari(self):
        
        movable_o = self.So-self.Som
        movable_w = self.Sw-self.Swc
        movable_g = self.Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water()
        krogo,krg = self._gas_oil()

        beta = (So_star)/(1-Sw_star)/(1-Sg_star)

        kro = (kroow*krogo)/(self.krowc)*beta

        return kro,krw,krg

    def _stones_model_II(self):

        movable_o = self.So-self.Som
        movable_w = self.Sw-self.Swc
        movable_g = self.Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water()
        krogo,krg = self._gas_oil()

        kro = self.krowc*((kroow/self.krowc+krw)*(krogo/self.krowc+krg)-(krw+krg))

        return kro,krw,krg

    def _hustad_holt(self,n):

        movable_o = self.So-self.Som
        movable_w = self.Sw-self.Swc
        movable_g = self.Sg-self.Sgc

        movable_f = 1-self.Som-self.Swc-self.Sgc

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water()
        krogo,krg = self._gas_oil()

        beta = (So_star)/(1-Sw_star)/(1-Sg_star)

        kro = (kroow*krogo)/(self.krowc)*beta**n

        return kro,krw,krg

class fluid():

    def __init__(self,viscosity):

        self.viscosity = viscosity

    def compressible(self,compressibility):

        self.compressibility = compressibility

    def slightly_compressible(self,compressibility):

        self.compressibility = compressibility

    def incompressible(self,density):

        self.density = density

class core_singlephase():

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
        
class core_multiphase():
    
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

class formation_singlephase():

    def __init__(self,height,radius,porosity,permeability,compressibility):

        self.height = height
        self.radius = radius

        self.porosity = porosity
        self.permeability = permeability
        self.compressibility = compressibility

    def set_total_compressibility(self,fluid):

        self.ct = self.cr+fluid.cf
        
    def set_diffusivity(self,viscosity,total_compressibility):
        
        self.eta = (self.k)/(self.phi*self.mu*self.ct)

    def set_well(self,radius,location,true_vertical_depth,measured_depth):

        self.radius = radius
        
    def set_fracture_nodes(self,thickness,nodes):

        self.thickness = thickness

    def set_hydraulic_diffusivity(self,hydraulic_diffusivity=None):

        if hydraulic_diffusivity is not None:
            self.eta = hydraulic_diffusivity

    def transient_flow(self,time):
        """line source solution"""

        well.radius = radius

        t0 = 100.*well_radius**2/self.eta
        te = 0.25*self.radius**2/self.eta

        if time>t0 and time<te:
            p = pi-5

    def steady_state_flow(self):
        pass

    def pseudo_steady_state_flow(self):
        """solution with shape factor"""
        pass

    def cartesian_2D(self):
        """green's solution"""
        pass

    def cartesian_3D(self):
        """green's solution"""
        pass

class computational_singlephase():
    
    """
    This class is supposed to generate regular grids in cartesian and
    radial coordinates and perform the given first or second order central
    derivative operation by generating A matrix and implementing boundary
    condition and solving for the given right hand side.
    """
    
    def __init__(self):
        
        pass

    def cartesian(self,length,grid_num):

        """
        length is a tuple with three entries for size in x,y,z direction
        for rectangular parallelepiped:
        """

        self.length_x = length[0]
        self.length_y = length[1]
        self.length_z = length[2]

        """
        grid_num is a tuple with three entries for discretization in x,y,z direction
        for rectangular parallelepiped:
        """

        self.num_x = grid_num[0]
        self.num_y = grid_num[1]
        self.num_z = grid_num[2]

        """
        self.num is a total number of grids for rectangular parallelepiped
        """
        self.num = self.num_x*self.num_y*self.num_z

        """
        self.id is a connectivity map and contain index of all grids
        their neighbours.
        """

        idx = np.arange(self.num)
        
        self.id = np.tile(idx,(7,1)).T

        self.id[idx.reshape(-1,self.num_x)[:,1:].ravel(),1] -= 1
        self.id[idx.reshape(-1,self.num_x)[:,:-1].ravel(),2] += 1
        self.id[idx.reshape(self.num_z,-1)[:,self.num_x:],3] -= self.num_x
        self.id[idx.reshape(self.num_z,-1)[:,:-self.num_x],4] += self.num_x
        self.id[idx.reshape(self.num_z,-1)[1:,:],5] -= self.num_x*self.num_y
        self.id[idx.reshape(self.num_z,-1)[:-1,:],6] += self.num_x*self.num_y

        """
        self.size is the size of grids in x,y,z direction
        """

        node_x = np.linspace(0,self.length_x,self.num_x+1)
        node_y = np.linspace(0,self.length_y,self.num_y+1)
        node_z = np.linspace(0,self.length_z,self.num_z+1)
        
        xsize = node_x[1:]-node_x[:-1]
        ysize = node_y[1:]-node_y[:-1]
        zsize = node_z[1:]-node_z[:-1]
        
        self.size = np.zeros((self.num,3))
        self.size[:,0] = np.tile(xsize,self.num_y*self.num_z)
        self.size[:,1] = np.tile(ysize.repeat(self.num_x),self.num_z)
        self.size[:,2] = zsize.repeat(self.num_x*self.num_y)

        """
        self.area is the area of three faces of grids in x,y,z direction
        """
        
        self.area = np.zeros((self.num,3))
        self.area[:,0] = self.size[:,1]*self.size[:,2]
        self.area[:,1] = self.size[:,2]*self.size[:,0]
        self.area[:,2] = self.size[:,0]*self.size[:,1]

        """
        self.volume is the volume of grids in x,y,z direction
        """

        self.volume = np.prod(self.size,axis=1)

        xcenter = node_x[:-1]+xsize/2
        ycenter = node_y[:-1]+ysize/2
        zcenter = node_z[:-1]+zsize/2

        """
        self.center is the x,y,z coordinate of the center of grids
        """
        
        self.center = np.zeros((self.num,3))
        self.center[:,0] = np.tile(xcenter,self.num_y*self.num_z)
        self.center[:,1] = np.tile(ycenter.repeat(self.num_x),self.num_z)
        self.center[:,2] = zcenter.repeat(self.num_x*self.num_y)

    def initialize(self,
                   permeability=(1,1,1),
                   porosity=1,
                   viscosity=1,
                   compressibility=1,
                   timetot=None,
                   timestep=None,
                   pressure=None):

        if isinstance(permeability,tuple):
            permeability = np.array(permeability)
        elif isinstance(permeability,float):
            permeability = np.array([permeability])
        elif isinstance(permeability,int):
            permeability = np.array([permeability])
            
        if permeability.shape==(self.num,3):
            self.permeability = permeability
        elif permeability.shape==(1,):
            self.permeability = np.tile(permeability,(self.num,3))
        elif permeability.shape==(3,):
            self.permeability = permeability.repeat(self.num).reshape((-1,self.num)).T
        elif permeability.shape==(self.num,):
            self.permeability = permeability.repeat(3).reshape((-1,3))
            
        self.porosity = porosity
        
        self.viscosity = viscosity
        
        self.compressibility = compressibility #total compressibility

        if timetot is None: return
        
        self.time = np.arange(0.,timetot+timestep/2,timestep)

        self.timestep = float(timestep)

        self.pressure = np.zeros((self.num,self.time.size))

        self.pressure[:,0] = pressure

    def transmissibility(self):
        
        dx_m = (self.size[:,0]+self.size[self.id[:,1],0])/2
        dx_p = (self.size[:,0]+self.size[self.id[:,2],0])/2
        dy_m = (self.size[:,1]+self.size[self.id[:,3],1])/2
        dy_p = (self.size[:,1]+self.size[self.id[:,4],1])/2
        dz_m = (self.size[:,2]+self.size[self.id[:,5],2])/2
        dz_p = (self.size[:,2]+self.size[self.id[:,6],2])/2

        kx_m = (2*dx_m)/(self.size[:,0]/self.permeability[:,0]+
                         self.size[self.id[:,1],0]/self.permeability[self.id[:,1],0])
        kx_p = (2*dx_p)/(self.size[:,0]/self.permeability[:,0]+
                         self.size[self.id[:,2],0]/self.permeability[self.id[:,2],0])
        ky_m = (2*dy_m)/(self.size[:,1]/self.permeability[:,1]+
                         self.size[self.id[:,3],1]/self.permeability[self.id[:,3],1])
        ky_p = (2*dy_p)/(self.size[:,1]/self.permeability[:,1]+
                         self.size[self.id[:,4],1]/self.permeability[self.id[:,4],1])
        kz_m = (2*dz_m)/(self.size[:,2]/self.permeability[:,2]+
                         self.size[self.id[:,5],2]/self.permeability[self.id[:,5],2])
        kz_p = (2*dz_p)/(self.size[:,2]/self.permeability[:,2]+
                         self.size[self.id[:,6],2]/self.permeability[self.id[:,6],2])

        etax_m = kx_m/(self.porosity*self.viscosity*self.compressibility)
        etax_p = kx_p/(self.porosity*self.viscosity*self.compressibility)
        etay_m = ky_m/(self.porosity*self.viscosity*self.compressibility)
        etay_p = ky_p/(self.porosity*self.viscosity*self.compressibility)
        etaz_m = kz_m/(self.porosity*self.viscosity*self.compressibility)
        etaz_p = kz_p/(self.porosity*self.viscosity*self.compressibility)

        self.transmissibility = np.zeros((self.num,6))

        self.transmissibility[:,0] = (2*etax_m)/(dx_m*(dx_m+dx_p))
        self.transmissibility[:,1] = (2*etax_p)/(dx_p*(dx_m+dx_p))
        self.transmissibility[:,2] = (2*etay_m)/(dy_m*(dy_m+dy_p))
        self.transmissibility[:,3] = (2*etay_p)/(dy_p*(dy_m+dy_p))
        self.transmissibility[:,4] = (2*etaz_m)/(dz_m*(dz_m+dz_p))
        self.transmissibility[:,5] = (2*etaz_p)/(dz_p*(dz_m+dz_p))

    def central(self,order=2):

        noxmin = ~(self.id[:,0]==self.id[:,1])
        noxmax = ~(self.id[:,0]==self.id[:,2])
        noymin = ~(self.id[:,0]==self.id[:,3])
        noymax = ~(self.id[:,0]==self.id[:,4])
        nozmin = ~(self.id[:,0]==self.id[:,5])
        nozmax = ~(self.id[:,0]==self.id[:,6])

        id_noxmin = self.id[noxmin,0] #id of grids not at xmin boundary
        id_noxmax = self.id[noxmax,0] #id of grids not at xmax boundary
        id_noymin = self.id[noymin,0] #id of grids not at ymin boundary
        id_noymax = self.id[noymax,0] #id of grids not at ymax boundary
        id_nozmin = self.id[nozmin,0] #id of grids not at zmin boundary
        id_nozmax = self.id[nozmax,0] #id of grids not at zmax boundary

        idNnoxmin = self.id[noxmin,1] #id of xmin neighbors for id_noxmin grids
        idNnoxmax = self.id[noxmax,2] #id of xmax neighbors for id_noxmax grids
        idNnoymin = self.id[noymin,3] #id of ymin neighbors for id_noymin grids
        idNnoymax = self.id[noymax,4] #id of ymax neighbors for id_noymax grids
        idNnozmin = self.id[nozmin,5] #id of zmin neighbors for id_nozmin grids
        idNnozmax = self.id[nozmax,6] #id of zmax neighbors for id_nozmax grids

        cx_m = self.transmissibility[noxmin,0]
        cx_p = self.transmissibility[noxmax,1]
        cy_m = self.transmissibility[noymin,2]
        cy_p = self.transmissibility[noymax,3]
        cz_m = self.transmissibility[nozmin,4]
        cz_p = self.transmissibility[nozmax,5]

        shape = (self.num,self.num)

        self.Amatrix = csr(shape)

        self.Amatrix += csr((cx_m,(id_noxmin,idNnoxmin)),shape=shape)
        self.Amatrix -= csr((cx_m,(id_noxmin,id_noxmin)),shape=shape)
        
        self.Amatrix += csr((cx_p,(id_noxmax,idNnoxmax)),shape=shape)
        self.Amatrix -= csr((cx_p,(id_noxmax,id_noxmax)),shape=shape)
        
        self.Amatrix += csr((cy_m,(id_noymin,idNnoymin)),shape=shape)
        self.Amatrix -= csr((cy_m,(id_noymin,id_noymin)),shape=shape)
        
        self.Amatrix += csr((cy_p,(id_noymax,idNnoymax)),shape=shape)
        self.Amatrix -= csr((cy_p,(id_noymax,id_noymax)),shape=shape)
        
        self.Amatrix += csr((cz_p,(id_nozmin,idNnozmin)),shape=shape)
        self.Amatrix -= csr((cz_p,(id_nozmin,id_nozmin)),shape=shape)
        
        self.Amatrix += csr((cz_m,(id_nozmax,idNnozmax)),shape=shape)
        self.Amatrix -= csr((cz_p,(id_nozmax,id_nozmax)),shape=shape)
        
    def implement_bc(self,
                     b_xmin=(0,1,0),
                     b_xmax=(0,1,0),
                     b_ymin=(0,1,0),
                     b_ymax=(0,1,0),
                     b_zmin=(0,1,0),
                     b_zmax=(0,1,0)):

        """
        b_xmin,b_xmax,b_ymin,b_ymax,b_zmin and b_zmax have three entries:
        - dirichlet boundary condition coefficient,
        - neumann boundary condition coefficient,
        - function value of boundary condition
        If not specified, no flow boundary conditions are implemented.
        """

        self.b_correction = np.zeros(self.num)

        questbound = lambda x: True if x>1 else False

        if questbound(self.num_x):

            xmin = self.id[:,0]==self.id[:,1]
            xmax = self.id[:,0]==self.id[:,2]

            id_xmin = self.id[xmin,0]
            id_xmax = self.id[xmax,0]

            dx_xmin = self.size[id_xmin,0]
            dx_xmax = self.size[id_xmax,0]

            tx_xmin = self.transmissibility[id_xmin,0]
            tx_xmax = self.transmissibility[id_xmax,1]

            bc_xmin = (2*tx_xmin*dx_xmin)/(b_xmin[0]*dx_xmin-2*b_xmin[1])
            bc_xmax = (2*tx_xmax*dx_xmax)/(b_xmax[0]*dx_xmax+2*b_xmax[1])
            
            self.Amatrix[id_xmin,id_xmin] -= bc_xmin*b_xmin[0]
            self.Amatrix[id_xmax,id_xmax] -= bc_xmax*b_xmax[0]
            
            self.b_correction[id_xmin] -= bc_xmin*b_xmin[2]
            self.b_correction[id_xmax] -= bc_xmax*b_xmax[2]

        if questbound(self.num_y):

            ymin = self.id[:,0]==self.id[:,3]
            ymax = self.id[:,0]==self.id[:,4]

            id_ymin = self.id[ymin,0]
            id_ymax = self.id[ymax,0]

            dy_ymin = self.size[id_ymin,1]
            dy_ymax = self.size[id_ymax,1]

            ty_ymin = self.transmissibility[id_ymin,2]
            ty_ymax = self.transmissibility[id_ymax,3]

            bc_ymin = (2*ty_ymin*dy_ymin)/(b_ymin[0]*dy_ymin-2*b_ymin[1])
            bc_ymax = (2*ty_ymax*dy_ymax)/(b_ymax[0]*dy_ymax+2*b_ymax[1])
            
            self.Amatrix[id_ymin,id_ymin] -= bc_ymin*b_ymin[0]
            self.Amatrix[id_ymax,id_ymax] -= bc_ymax*b_ymax[0]
            
            self.b_correction[id_ymin] -= bc_ymin*b_ymin[2]
            self.b_correction[id_ymax] -= bc_ymax*b_ymax[2]
            
        if questbound(self.num_z):

            zmin = self.id[:,0]==self.id[:,5]
            zmax = self.id[:,0]==self.id[:,6]

            id_zmin = self.id[zmin,0]
            id_zmax = self.id[zmax,0]

            dz_zmin = self.size[id_zmin,2]
            dz_zmax = self.size[id_zmax,2]

            tz_zmin = self.transmissibility[id_zmin,4]
            tz_zmax = self.transmissibility[id_zmax,5]

            bc_zmin = (2*tz_zmin*dz_zmin)/(b_zmin[0]*dz_zmin-2*b_zmin[1])
            bc_zmax = (2*tz_zmax*dz_zmax)/(b_zmax[0]*dz_zmax+2*b_zmax[1])

            self.Amatrix[id_zmin,id_zmin] -= bc_zmin*b_zmin[0]
            self.Amatrix[id_zmax,id_zmax] -= bc_zmax*b_zmax[0]
            
            self.b_correction[id_zmin] -= bc_zmin*b_zmin[2]
            self.b_correction[id_zmax] -= bc_zmax*b_zmax[2]
            
    def solve(self,rhs=0):

        if not hasattr(self,'time'):
            
            bvector = np.full(self.num,rhs).astype('float')
            
            bvector += self.b_correction
            
            self.unknown = sps(self.Amatrix,bvector)

            return
        
        t_correction = csr((np.ones(self.num)/self.timestep,
                           (self.id[:,0],self.id[:,0])),
                            shape=(self.num,self.num))
        
        A = self.Amatrix-t_correction
        
        for i in range(1,self.time.size):
            
            b = -self.pressure[:,i-1]/self.timestep+self.b_correction
            
            self.pressure[:,i] = sps(A,b)

class formation_complex_fracture_singlephase():

    def __init__(self):
        pass
    
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)

##    Sw = 0.3
##    Sg = 0.3
##
##    RP = relative_permeability(Sw,Sg,Som=0.1125,Sorow=0.15,Sorgo=0.05,Swc=0.15,Sgc=0.1,krowc=0.88)
##
##    RP.system3phase(model="Stone's Model II")
##
##    print(RP.kro,RP.krw,RP.krg)

##    k = 50 #mD
##    k *= 0.986923e-15
##
##    phi = 0.15
##    mu = 1e-3
##    c = 2e-9
##    
##    sp = diffusivity(k/(phi*mu*c))
##
##    PL = 2000 #psi
##    PU = 3500 #psi
##
##    PL *= 6894.76
##    PU *= 6894.76
##
##    sp.cartesian_1D((0,50),(PL,PU),np.array([60,600]),x=np.array([12,27,41]),N=4)
##
##    print(sp.pressure/6894.76)

##    beta = 10
##
##    bound_points = np.array([[0],[7]])
##
##    bound_conditions = np.array([[1,0,300],[0,1,0]])
##
##    analytical = slightly_compressible(0)
##    
##    analytical.cartesian_poisson_1D(bound_points,bound_conditions,beta)
##    
##    plt.plot(analytical.x,analytical.u,'k',label='Analytical Solution')
##
##    plt.tight_layout()
##
##    plt.show()

##    ## ALL CALCULATIONS ARE CARRIED IN BUCKLEYLEVERETT CLASS ABOVE
#### BASED ON THE INPUT PROVIDED BELOW
##
##    Sor = 0.25
##    Swi = 0.15
##    Swr = 0.15
####    koro = 1.0
####    korw = 0.78
####    m = 2.6
####    n = 3.7
##    muo = 8.
##    muw = 1.
##    
##    q = 1000*5.615
##    A = 2500
##    L = 1000
##    phi = 0.2
##    
##    BL = buckleyleverett(Sor,Swr,muo,muw)
##    
####    BL.coreymodel(koro,korw,m,n)
##    BL.k_model()
##    BL.fractionalflow()
##    BL.shockfront(Swi)
##
##    BL.production(q,A,L,phi)
##    BL.profile(q,A,phi,BL.tbt*0.3)#0.02 is for the time
##
##    Np = BL.Np*BL.Vp/5.615          # oil produced, bbl
##
##    qo = np.empty_like(Np)
##    qo[0] = Np[1]/BL.tp[1]
##    qo[1:] = Np[1:]/BL.tp[1:]       # bbl/day
##    
##    qw = q/5.615-qo                 # bbl/day
##    
##    WOR = qw/qo
##
##    WOR[WOR<1e-15] = 0

## INPUTS END HERE

## THE SECTION BELOW IS ONLY FOR PLOTTING AND ANIMATION
## FOR SHOWING A PLOT YOU WANT, UNCOMMENT OUT THE LINES ACCORDINGLY

## RELATIVE PERMEABILITY PLOT

##    plt.plot(BL.Sw,BL.kro)
##    plt.plot(BL.Sw,BL.krw)
##    plt.xlim([0,1])
##    plt.ylim(bottom=0)
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('relative permeability',fontsize=14)
##    plt.legend(('oil','water'),fontsize=14)
##    plt.show()

## FRACTIONAL FLOW PLOT

##    plt.plot(BL.Sw,BL.fw,c='k')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('fractional water flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim((0,1))
####    plt.xticks([0,1])
####    plt.yticks([0,1])
##    plt.show()

## FRACTIONAL FLOW DERIVATIVE PLOT

####    A = (BL.Sw-BL.Swr)**3*BL.muo
####    B = A+2*(1-BL.Sw-BL.Sor)**2*BL.muw
####
####    C = 3*(BL.Sw-BL.Swr)**2*BL.muo
####    D = C-4*(1-BL.Sw-BL.Sor)*BL.muw
####
####    F = C/B-A/B**2*D
##
##    plt.plot(BL.Sw,BL.fw_der,c='k')
####    plt.plot(BL.Sw,F,'r--')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('derivative of water fractional flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim(bottom=0)
####    plt.xticks([0,1])
####    plt.yticks([])
##    plt.show()

## FRACTIONAL FLOW AND ITS DERIVATIVE TOGETHER PLOT

##    fig, ax1 = plt.subplots()
##
##    color = 'tab:blue'
##    ax1.plot(BL.Sw,BL.fw,color=color)
##    ax1.set_ylabel('water fractional flow',color=color)
##    ax1.set_xlabel('water saturation')
##    ax1.tick_params(axis='y', labelcolor=color)
##
##    ax2 = ax1.twinx()
##
##    color = 'tab:red'
##    ax2.plot(BL.Sw,BL.fw_der,color=color)
##    ax2.set_ylabel('water fractional flow derivative',color=color)
##    ax2.tick_params(axis='y', labelcolor=color)
##
##    fig.tight_layout()
##
##    plt.xlim((0,1))
##
##    plt.show()

## FRACTIONAL FLOW PLOT FOR THE SHOCK FRONT DETERMINATION

##    plt.plot(BL.Sw,BL.fw,c='k')
##    plt.plot((BL.Swi,BL.Sw_avg),(BL.fwi,1),c='r')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('fractional water flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim((0,1))
####    plt.xticks([0,1])
####    plt.yticks([0,1])
##    plt.show()

## WATER SATURATION PORFILE WITH RESPECT TO DISTANCE
    
##    x = np.insert(BL.x_Sw,0,2*L)
##    y = np.insert(BL.Sw_IC,0,BL.Swi)
##    plt.plot(x,y,'k')
##    plt.xlim((0,L))
##    plt.ylim((0,1))
##    plt.xlabel('x-direction',fontsize=14)
##    plt.ylabel('water saturation',fontsize=14)
##    plt.tight_layout()
##    plt.show()

## OIL PRODUCTION RATE VS INJECTED WATER
    
##    plt.plot(BL.Wi,BL.Np*BL.Vp/5.615/BL.tp)
##    plt.xlim([0,3])
##    plt.ylim(bottom=0)
##    plt.xlabel('Pore Volume of Water injected',fontsize=14)
##    plt.ylabel('Oil Production Rate [bbl/day]',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## WATER OIL RATIO VS CUMULATIVE OIL PRODUCTION

##    plt.semilogx(WOR,Np)
##    plt.ylim(bottom=90000)
##    plt.xlim(left=0.00001)
##    plt.xlabel('water oil ratio',fontsize=14)
##    plt.ylabel('total oil production [bbl]',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## CUMULATIVE OIL RECOVERY VS INJECTED WATER
    
##    plt.plot(BL.Wi,BL.Np)
##    plt.xlim([0,10])
##    plt.ylim(bottom=0)
##    plt.xlabel('Injected Pore Volume of Water',fontsize=14)
##    plt.ylabel('Produced Pore Volume of Oil',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## ANIMATION OF WATER SATURATION WITH RESPECT TO DISTANCE CHANGING WITH TIME WHILE SWEEPING OIL

##    ax = plt.subplot(1,1,1)
##
##    ax.set_xlim((0,L))
##    ax.set_ylim((0,1))
##    ax.set_xlabel('x-direction',fontsize=14)
##    ax.set_ylabel('water saturation',fontsize=14)
##    
##    plt.tight_layout()
##
##    N_tn = 100
##    
##    t_n = np.linspace(0,0.035,N_tn)
##
##    frames = []
##    
##    for i,t in enumerate(t_n):
##        
##        BL.profile(q,A,phi,t)
##        
##        x = np.insert(BL.x_Sw,0,2*L)
##        y = np.insert(BL.Sw_IC,0,BL.Swi)
##
##        line, = ax.plot(x,y,'k')
##
##        #below the image is created in the memory and appended to frames
##        buf = io.BytesIO()
##        plt.savefig(buf)
##        img = Image.open(buf)
##        frames.append(img)
##
##        line.remove()
##        
##    plt.close()
##    
##    #generates the animation from the images created
##    frames[0].save('buckley.gif',
##                   format='GIF',
##                   append_images=frames[1:],
##                   save_all=True,
##                   duration=50,
##                   loop=0)
