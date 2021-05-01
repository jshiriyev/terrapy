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

##from flow.tests import test_3D_finite_difference

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

    def cartesian(self,length,
                       grid_num,
                       b_xmin=(0,1,0),
                       b_xmax=(0,1,0),
                       b_ymin=(0,1,0),
                       b_ymax=(0,1,0),
                       b_zmin=(0,1,0),
                       b_zmax=(0,1,0)):

        self.length = np.array(length)

        # required correction
        b_xmin = np.array(b_xmin)
        b_xmax = np.array(b_xmax)
        b_ymin = np.array(b_ymin)
        b_ymax = np.array(b_ymax)
        b_zmin = np.array(b_zmin)
        b_zmax = np.array(b_zmax)
        
        b_xmax[1] = -b_xmax[1]
        b_ymax[1] = -b_ymax[1]
        b_zmax[1] = -b_zmax[1]

        #length and grid_num are tuples with three entries for
        # size and discretization in x,y,z direction
        # for rectangular parallelepiped:

        self.num_x = grid_num[0]
        self.num_y = grid_num[1]
        self.num_z = grid_num[2]

        node_x = np.linspace(0,length[0],self.num_x+1)
        node_y = np.linspace(0,length[1],self.num_y+1)
        node_z = np.linspace(0,length[2],self.num_z+1)

        #self.num: total number of grids for rectangular parallelepiped
        self.num = self.num_x*self.num_y*self.num_z
        
        #self.id is a connectivity map and contain index of all grids
        # their neighbours.

        idx = np.arange(self.num)
        
        self.id = np.tile(idx,(7,1)).T

        xmin = np.arange(self.num).astype('int')
        xmax = np.arange(self.num).astype('int')
        ymin = np.arange(self.num).astype('int')
        ymax = np.arange(self.num).astype('int')
        zmin = np.arange(self.num).astype('int')
        zmax = np.arange(self.num).astype('int')

        self.id[idx.reshape(-1,self.num_x)[:,1:].ravel(),1] -= 1
        self.id[idx.reshape(-1,self.num_x)[:,:-1].ravel(),2] += 1
        self.id[idx.reshape(self.num_z,-1)[:,self.num_x:],3] -= self.num_x
        self.id[idx.reshape(self.num_z,-1)[:,:-self.num_x],4] += self.num_x
        self.id[idx.reshape(self.num_z,-1)[1:,:],5] -= self.num_x*self.num_y
        self.id[idx.reshape(self.num_z,-1)[:-1,:],6] += self.num_x*self.num_y

        #self.boundary is id of boundary grids,
        # the id of its neighbors and boundary conditions.

        idb = np.zeros(7)
        
        questbound = lambda x: True if x>1 else False
        
        if questbound(self.num_x):
            idb[1] = self.num_y*self.num_z
            idb[2] = self.num_y*self.num_z

        if questbound(self.num_y):
            idb[3] = self.num_z*self.num_x
            idb[4] = self.num_z*self.num_x

        if questbound(self.num_z):
            idb[5] = self.num_x*self.num_y
            idb[6] = self.num_x*self.num_y
            
        idb = np.cumsum(idb).astype('int')

        self.boundary = np.zeros((idb[-1],1+2*3+3))

        if questbound(self.num_x):
            self.boundary[idb[0]:idb[1],:-3] = self.id[idx==self.id[:,1],:]
            self.boundary[idb[1]:idb[2],:-3] = self.id[idx==self.id[:,2],:]

        if questbound(self.num_y):
            self.boundary[idb[2]:idb[3],:-3] = self.id[idx==self.id[:,3],:]
            self.boundary[idb[3]:idb[4],:-3] = self.id[idx==self.id[:,4],:]

        if questbound(self.num_z):
            self.boundary[idb[4]:idb[5],:-3] = self.id[idx==self.id[:,5],:]
            self.boundary[idb[5]:idb[6],:-3] = self.id[idx==self.id[:,6],:]

        #boundaries (b_xmin,b_xmax,b_ymin,b_ymax,b_zmin,b_zmax)
        #have three entries:
        # - dirichlet coefficient,
        # - neumann coefficient,
        # - function value of boundary condition
        #by default no flow boundary conditions are implemented

        self.boundary[idb[0]:idb[1],7:] = b_xmin
        self.boundary[idb[1]:idb[2],7:] = b_xmax
        self.boundary[idb[2]:idb[3],7:] = b_ymin
        self.boundary[idb[3]:idb[4],7:] = b_ymax
        self.boundary[idb[4]:idb[5],7:] = b_zmin
        self.boundary[idb[5]:idb[6],7:] = b_zmax

        #calculation of grid geometrical properties
        xsize = node_x[1:]-node_x[:-1]
        ysize = node_y[1:]-node_y[:-1]
        zsize = node_z[1:]-node_z[:-1]

        xcenter = node_x[:-1]+xsize/2
        ycenter = node_y[:-1]+ysize/2
        zcenter = node_z[:-1]+zsize/2
        
        self.size = np.zeros((self.num,3))
        self.size[:,0] = np.tile(xsize,self.num_y*self.num_z)
        self.size[:,1] = np.tile(ysize.repeat(self.num_x),self.num_z)
        self.size[:,2] = zsize.repeat(self.num_x*self.num_y)

        self.volume = np.prod(self.size,axis=1)
        
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

        cxp = 2/(dxp*(dxp+dxm))
        cxm = 2/(dxm*(dxp+dxm))

        cyp = 2/(dyp*(dyp+dym))
        cym = 2/(dym*(dyp+dym))

        czp = 2/(dzp*(dzp+dzm))
        czm = 2/(dzm*(dzp+dzm))

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

    def implement_bc(self,bvector):
        
        self.bvector = bvector

        iicc = self.id[:,0].astype('int')

        jjxm = self.id[:,1].astype('int')
        jjxp = self.id[:,2].astype('int')

        jjym = self.id[:,3].astype('int')
        jjyp = self.id[:,4].astype('int')

        jjzm = self.id[:,5].astype('int')
        jjzp = self.id[:,6].astype('int')

        ii = np.array([]).astype('int')
        jj = np.array([]).astype('int')

        questbound = lambda x: True if x>1 else False

        if questbound(self.num_x):
            ii = np.append(ii,iicc[iicc==jjxm])
            jj = np.append(jj,jjxp[iicc==jjxm])
            ii = np.append(ii,iicc[iicc==jjxp])
            jj = np.append(jj,jjxm[iicc==jjxp])

        if questbound(self.num_y):
            ii = np.append(ii,iicc[iicc==jjym])
            jj = np.append(jj,jjyp[iicc==jjym])
            ii = np.append(ii,iicc[iicc==jjyp])
            jj = np.append(jj,jjym[iicc==jjyp])

        if questbound(self.num_z):
            ii = np.append(ii,iicc[iicc==jjzm])
            jj = np.append(jj,jjzp[iicc==jjzm])
            ii = np.append(ii,iicc[iicc==jjzp])
            jj = np.append(jj,jjzm[iicc==jjzp])
        
        dbc = self.boundary[:,7]
        nbc = self.boundary[:,8]
        fbc = self.boundary[:,9]

        dxii = self.size[ii,0] # self
        dxjj = self.size[jj,0] # neighbour

        bc = 8/((3*dxii+dxjj)*(dbc*dxii-2*nbc))

        self.Amatrix[ii,ii] -= bc*dbc
        self.bvector[ii,0] -= bc*fbc

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

##    test_3D_finite_difference
 
