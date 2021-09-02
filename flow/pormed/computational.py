import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr

from scipy.sparse.linalg import spsolve as sps

if __name__ == "__main__":
    import setup

class singlephase():
    
    """
    This class is supposed to generate regular grids in cartesian and
    radial coordinates and perform the given first or second order central
    derivative operation by generating A matrix and implementing boundary
    condition and solving for the given right hand side.
    """
    
    def __init__(self):
        
        pass

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
    
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)