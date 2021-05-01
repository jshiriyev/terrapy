# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr

from scipy.sparse.linalg import spsolve

#Local application imports

from flow.tests import test_3D_finite_difference

class finite_difference():
    
    """
    This version 2 calculates matrix entries referencing grid
    center for 1D problems. dx must be zero dimensional array.
    
    order must be either 1 or 2.
    """
    
    """
    regular grids are generated in this class,
    cartesian grids are generated in 3D
    radial grids are in progress...
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

    def radial(self):
        pass

    def central(self,order=2):

        idxm = ~(self.id[:,0]==self.id[:,1])
        idxp = ~(self.id[:,0]==self.id[:,2])
        
        idym = ~(self.id[:,0]==self.id[:,3])
        idyp = ~(self.id[:,0]==self.id[:,4])
        
        idzm = ~(self.id[:,0]==self.id[:,5])
        idzp = ~(self.id[:,0]==self.id[:,6])

        dxm = (self.size[self.id[idxm,0],0]+self.size[self.id[idxm,1],0])/2
        dxp = (self.size[self.id[idxp,0],0]+self.size[self.id[idxp,2],0])/2
        
        dym = (self.size[self.id[idym,0],1]+self.size[self.id[idym,3],1])/2
        dyp = (self.size[self.id[idyp,0],1]+self.size[self.id[idyp,4],1])/2
        
        dzm = (self.size[self.id[idzm,0],2]+self.size[self.id[idzm,5],2])/2
        dzp = (self.size[self.id[idzp,0],2]+self.size[self.id[idzp,6],2])/2

##        cxp = 2/(dxp*(dxp+dxm))
##        cxm = 2/(dxm*(dxp+dxm))
##
##        cyp = 2/(dyp*(dyp+dym))
##        cym = 2/(dym*(dyp+dym))
##
##        czp = 2/(dzp*(dzp+dzm))
##        czm = 2/(dzm*(dzp+dzm))

        self.Amatrix = csr((self.num,self.num))

        self.Amatrix += csr((dxm,(self.id[idxm,0],self.id[idxm,1])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dxm,(self.id[idxm,0],self.id[idxm,0])),
                            shape=(self.num,self.num))
        
        self.Amatrix += csr((dxp,(self.id[idxp,0],self.id[idxp,2])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dxp,(self.id[idxp,0],self.id[idxp,0])),
                            shape=(self.num,self.num))
        
        self.Amatrix += csr((dym,(self.id[idym,0],self.id[idym,3])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dym,(self.id[idym,0],self.id[idym,0])),
                            shape=(self.num,self.num))
        
        self.Amatrix += csr((dyp,(self.id[idyp,0],self.id[idyp,4])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dyp,(self.id[idyp,0],self.id[idyp,0])),
                            shape=(self.num,self.num))
        
        self.Amatrix += csr((dzp,(self.id[idzm,0],self.id[idzm,5])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dzp,(self.id[idzm,0],self.id[idzm,0])),
                            shape=(self.num,self.num))
        
        self.Amatrix += csr((dzp,(self.id[idzp,0],self.id[idzp,6])),
                            shape=(self.num,self.num))
        self.Amatrix += csr((-dzp,(self.id[idzp,0],self.id[idzp,0])),
                            shape=(self.num,self.num))

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
        - dirichlet coefficient,
        - neumann coefficient,
        - function value of boundary condition
        
        If not specified, no flow boundary conditions are implemented.
        """

        questbound = lambda x: True if x>1 else False

        if questbound(self.num_x):
            
            ii = self.id[self.id[:,0]==self.id[:,1],0]
            jj = self.id[self.id[:,0]==self.id[:,1],2]
            
            dxii = self.size[ii,0]
            dxjj = self.size[jj,0]
            
            bc = 8/((3*dxii+dxjj)*(b_xmin[0]*dxii-2*b_xmin[1]))
            
            self.Amatrix[ii,ii] -= bc*b_xmin[0]
            self.bvector[ii,0] -= bc*b_xmin[2]

            ii = self.id[self.id[:,0]==self.id[:,2],0]
            jj = self.id[self.id[:,0]==self.id[:,2],1]
            
            dxii = self.size[ii,0]
            dxjj = self.size[jj,0]
            
            bc = 8/((3*dxii+dxjj)*(b_xmax[0]*dxii+2*b_xmax[1]))
            
            self.Amatrix[ii,ii] -= bc*b_xmax[0]
            self.bvector[ii,0] -= bc*b_xmax[2]

        if questbound(self.num_y):

            ii = self.id[self.id[:,0]==self.id[:,3],0]
            jj = self.id[self.id[:,0]==self.id[:,3],4]

            dyii = self.size[ii,1]
            dyjj = self.size[jj,1]

            bc = 8/((3*dyii+dyjj)*(b_ymin[0]*dxii-2*b_ymin[1]))
            
            self.Amatrix[ii,ii] -= bc*b_ymin[0]
            self.bvector[ii,0] -= bc*b_ymin[2]

            ii = self.id[self.id[:,0]==self.id[:,4],0]
            jj = self.id[self.id[:,0]==self.id[:,4],3]

            dyii = self.size[ii,1]
            dyjj = self.size[jj,1]

            bc = 8/((3*dyii+dyjj)*(b_ymax[0]*dxii+2*b_ymax[1]))
            
            self.Amatrix[ii,ii] -= bc*b_ymax[0]
            self.bvector[ii,0] -= bc*b_ymax[2]

        if questbound(self.num_z):

            ii = self.id[self.id[:,0]==self.id[:,5],0]
            jj = self.id[self.id[:,0]==self.id[:,5],6]

            dzii = self.size[ii,2]
            dzjj = self.size[jj,2]

            bc = 8/((3*dzii+dzjj)*(b_zmin[0]*dzii-2*b_zmin[1]))
            
            self.Amatrix[ii,ii] -= bc*b_zmin[0]
            self.bvector[ii,0] -= bc*b_zmin[2]

            ii = self.id[self.id[:,0]==self.id[:,6],0]
            jj = self.id[self.id[:,0]==self.id[:,6],5]

            dzii = self.size[ii,2]
            dzjj = self.size[jj,2]

            bc = 8/((3*dzii+dzjj)*(b_zmax[0]*dzii+2*b_zmax[1]))
            
            self.Amatrix[ii,ii] -= bc*b_zmax[0]
            self.bvector[ii,0] -= bc*b_zmax[2]
        
##        dbc = self.boundary[:,7]
##        nbc = self.boundary[:,8]
##        fbc = self.boundary[:,9]

##        dxii = self.size[ii,0] # self
##        dxjj = self.size[jj,0] # neighbour

##        bc = 8/((3*dxii+dxjj)*(dbc*dxii-2*nbc))

##        self.Amatrix[ii,ii] -= bc*dbc
##        self.bvector[ii,0] -= bc*fbc

    def solve(self):

        self.unknown = spsolve(self.Amatrix,self.bvector)

if __name__ == "__main__":

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
 
