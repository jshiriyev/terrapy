"""1-Standard Library Imports"""
import os
import sys

##sys.path.append(os.path.dirname(os.getcwd()))

"""2-Third Party Imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr
from scipy.sparse.linalg import spsolve

"""3-Local Application Imports"""

class finite_difference():
    
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

    def set_property(self,permeability,porosity,viscosity,compressibility):

        self.permeability = np.zeros((self.num,3))
        self.permeability[:,0] = permeability[0]
        self.permeability[:,1] = permeability[1]
        self.permeability[:,2] = permeability[2]

        self.porosity = porosity
        self.viscosity = viscosity
        self.compressibility = compressibility

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

##        dx_m = (self.size[:,0]+self.size[self.id[:,1],0])/2
##        dx_p = (self.size[:,0]+self.size[self.id[:,2],0])/2
##        dy_m = (self.size[:,1]+self.size[self.id[:,3],1])/2
##        dy_p = (self.size[:,1]+self.size[self.id[:,4],1])/2
##        dz_m = (self.size[:,2]+self.size[self.id[:,5],2])/2
##        dz_p = (self.size[:,2]+self.size[self.id[:,6],2])/2

##        cx_m = (2/(dx_m*(dx_m+dx_p)))[noxmin]
##        cx_p = (2/(dx_p*(dx_m+dx_p)))[noxmax]
##        cy_m = (2/(dy_m*(dy_m+dy_p)))[noymin]
##        cy_p = (2/(dy_p*(dy_m+dy_p)))[noymax]
##        cz_m = (2/(dz_m*(dz_m+dz_p)))[nozmin]
##        cz_p = (2/(dz_p*(dz_m+dz_p)))[nozmax]

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
                     bvector,
                     b_xmin=(0,1,0),
                     b_xmax=(0,1,0),
                     b_ymin=(0,1,0),
                     b_ymax=(0,1,0),
                     b_zmin=(0,1,0),
                     b_zmax=(0,1,0)):
        
        self.bvector = bvector

        """
        b_xmin,b_xmax,b_ymin,b_ymax,b_zmin and b_zmax have three entries:
        - dirichlet boundary condition coefficient,
        - neumann boundary condition coefficient,
        - function value of boundary condition
        If not specified, no flow boundary conditions are implemented.
        """

        questbound = lambda x: True if x>1 else False

        if questbound(self.num_x):

            xmin = self.id[:,0]==self.id[:,1]
            xmax = self.id[:,0]==self.id[:,2]

            id_xmin = self.id[xmin,0]
            id_xmax = self.id[xmax,0]

            idNxmin = self.id[xmin,2]
            idNxmax = self.id[xmax,1]

            dx_xmin = self.size[id_xmin,0]
            dx_xmax = self.size[id_xmax,0]
            
            dxNxmin = self.size[idNxmin,0]
            dxNxmax = self.size[idNxmax,0]
            
            bc_xmin = 8/((3*dx_xmin+dxNxmin)*(b_xmin[0]*dx_xmin-2*b_xmin[1]))
            bc_xmax = 8/((3*dx_xmax+dxNxmax)*(b_xmax[0]*dx_xmax+2*b_xmax[1]))
            
            self.Amatrix[id_xmin,id_xmin] -= bc_xmin*b_xmin[0]
            self.Amatrix[id_xmax,id_xmax] -= bc_xmax*b_xmax[0]
            
            self.bvector[id_xmin,0] -= bc_xmin*b_xmin[2]
            self.bvector[id_xmax,0] -= bc_xmax*b_xmax[2]

        if questbound(self.num_y):

            ymin = self.id[:,0]==self.id[:,3]
            ymax = self.id[:,0]==self.id[:,4]

            id_ymin = self.id[ymin,0]
            id_ymax = self.id[ymax,0]

            idNymin = self.id[ymin,4]
            idNymax = self.id[ymax,3]

            dy_ymin = self.size[id_ymin,1]
            dy_ymax = self.size[id_ymax,1]

            dyNymin = self.size[idNymin,1]
            dyNymax = self.size[idNymax,1]

            bc_ymin = 8/((3*dy_ymin+dyNymin)*(b_ymin[0]*dy_ymin-2*b_ymin[1]))
            bc_ymax = 8/((3*dy_ymax+dyNymax)*(b_ymax[0]*dy_ymax+2*b_ymax[1]))
            
            self.Amatrix[id_ymin,id_ymin] -= bc_ymin*b_ymin[0]
            self.Amatrix[id_ymax,id_ymax] -= bc_ymax*b_ymax[0]
            
            self.bvector[id_ymin,0] -= bc_ymin*b_ymin[2]
            self.bvector[id_ymax,0] -= bc_ymax*b_ymax[2]
            
        if questbound(self.num_z):

            zmin = self.id[:,0]==self.id[:,5]
            zmax = self.id[:,0]==self.id[:,6]

            id_zmin = self.id[zmin,0]
            id_zmax = self.id[zmax,0]

            idNzmin = self.id[zmin,6]
            idNzmax = self.id[zmin,5]

            dz_zmin = self.size[id_zmin,2]
            dz_zmax = self.size[id_zmax,2]

            dzNzmin = self.size[idNzmin,2]
            dzNzmax = self.size[idNzmax,2]

            bc_zmin = 8/((3*dz_zmin+dzNzmin)*(b_zmin[0]*dz_zmin-2*b_zmin[1]))
            bc_zmax = 8/((3*dz_zmax+dzNzmax)*(b_zmax[0]*dz_zmax+2*b_zmax[1]))

            self.Amatrix[id_zmin,id_zmin] -= bc_zmin*b_zmin[0]
            self.Amatrix[id_zmax,id_zmax] -= bc_zmax*b_zmax[0]
            
            self.bvector[id_zmin,0] -= bc_zmin*b_zmin[2]
            self.bvector[id_zmax,0] -= bc_zmax*b_zmax[2]
            
    def solve(self):

        self.unknown = spsolve(self.Amatrix,self.bvector)

    def radial(self):
        pass

if __name__ == "__main__":

    from tests import test_3D_finite_difference

    def myfunc1(x,order=0):
        if order==0:
            return 0.96*np.exp(0.5*x)
        elif order==1:
            return 0.48*np.exp(0.5*x)
        elif order==2:
            return 0.24*np.exp(0.5*x)

    def myfunc2(x,order=0):
        #x is in radians
        if order==0:
            return np.sin(x)
        elif order==1:
            return np.cos(x)
        elif order==2:
            return -np.sin(x)

    test_3D_finite_difference
 
