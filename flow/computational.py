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

class mesh():

    """
    regular grids are generated in this class
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

        idx = np.arange(self.num).astype('float')

        xmin = idx-1
        xmax = idx+1
        ymin = idx-self.num_x
        ymax = idx+self.num_x
        zmin = idx-self.num_x*self.num_y
        zmax = idx+self.num_x*self.num_y

        xmin.reshape((-1,self.num_x))[:,0] = np.nan
        xmax.reshape((-1,self.num_x))[:,-1] = np.nan
        ymin.reshape((self.num_z,-1))[:,:self.num_x] = np.nan
        ymax.reshape((self.num_z,-1))[:,-self.num_x:] = np.nan
        zmin.reshape((self.num_z,-1))[0,:] = np.nan
        zmax.reshape((self.num_z,-1))[-1,:] = np.nan

        self.id = np.zeros((self.num,1+2*3))
        
        self.id[:,0] = idx
        self.id[:,1] = xmin
        self.id[:,2] = xmax
        self.id[:,3] = ymin
        self.id[:,4] = ymax
        self.id[:,5] = zmin
        self.id[:,6] = zmax

        #self.boundary is id of boundary grids,
        # the id of its neighbors and boundary conditions.

        questbound = lambda x: 1 if x>1 else 0

        num_bxmin = self.num_y*self.num_z*questbound(self.num_x)
        num_bxmax = self.num_y*self.num_z*questbound(self.num_x)
        
        num_bymin = self.num_z*self.num_x*questbound(self.num_y)
        num_bymax = self.num_z*self.num_x*questbound(self.num_y)
        
        num_bzmin = self.num_x*self.num_y*questbound(self.num_z)
        num_bzmax = self.num_x*self.num_y*questbound(self.num_z)

        idb = np.array([0,num_bxmin,num_bxmax,
                          num_bymin,num_bymax,
                          num_bzmin,num_bzmax])

        idb = np.cumsum(idb)

        self.boundary = np.zeros((idb[-1],1+2*3+3))

        self.boundary[idb[0]:idb[1],:-3] = self.id[np.isnan(xmin),:]
        self.boundary[idb[1]:idb[2],:-3] = self.id[np.isnan(xmax),:]
        self.boundary[idb[2]:idb[3],:-3] = self.id[np.isnan(ymin),:]
        self.boundary[idb[3]:idb[4],:-3] = self.id[np.isnan(ymax),:]
        self.boundary[idb[4]:idb[5],:-3] = self.id[np.isnan(zmin),:]
        self.boundary[idb[5]:idb[6],:-3] = self.id[np.isnan(zmax),:]

        #boundaries (b_xmin,b_xmax,b_ymin,b_ymax,b_zmin,b_zmax)
        #have three entries:
        # - dirichlet coefficient,
        # - neumann coefficient,
        # - function value of boundary condition
        #by default no flow boundary conditions are implemented

        self.boundary[idb[0]:idb[1],7:] = np.array(b_xmin)
        self.boundary[idb[1]:idb[2],7:] = np.array(b_xmax)
        self.boundary[idb[2]:idb[3],7:] = np.array(b_ymin)
        self.boundary[idb[3]:idb[4],7:] = np.array(b_ymax)
        self.boundary[idb[4]:idb[5],7:] = np.array(b_zmin)
        self.boundary[idb[5]:idb[6],7:] = np.array(b_zmax)

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
        
        self.center =  np.zeros((self.num,3))
        self.center[:,0] = np.tile(xcenter,self.num_y*self.num_z)
        self.center[:,1] = np.tile(ycenter.repeat(self.num_x),self.num_z)
        self.center[:,2] = zcenter.repeat(self.num_x*self.num_y)

    def radial(self):
        pass

class finite_difference():
    
    """

    This version 2 calculates matrix entries referencing grid
    center for 1D problems. dx must be zero dimensional array.
    
    order must be either 1 or 2.
    
    """
    
    def __init__(self,grids):
        
        self.grids = grids            # grids should be an input
        
    def central(self,order=1):
        
        dx_imag = np.insert(self.grids.sizes,(0,-1),
                            (self.grids.sizes[0],self.grids.sizes[-1]))

        dx_cntr = (dx_imag[:-1]+dx_imag[1:])/2

        dx_upp_diag = dx_cntr[:-1]
        dx_low_diag = dx_cntr[1:]
        
        two_dx = (dx_upp_diag+dx_low_diag)

        idf_low_diag = two_dx**(-1)*(-1)**order*(2*dx_low_diag)**(order-1)
        idf_upp_diag = two_dx**(-1)*(2*dx_upp_diag)**(order-1)

        dnm = (dx_upp_diag*dx_low_diag)**(1-order)
        
        r_low_diag = self.grids.ids[1:]
        c_low_diag = self.grids.ids[:-1]
        f_low_diag = idf_low_diag[1:]

        r_upp_diag = self.grids.ids[:-1]
        c_upp_diag = self.grids.ids[1:]
        f_upp_diag = idf_upp_diag[:-1]

        r_off_diag = np.append(r_low_diag,r_upp_diag)
        c_off_diag = np.append(c_low_diag,c_upp_diag)
        f_off_diag = np.append(f_low_diag,f_upp_diag)

        row = np.append(r_off_diag,r_off_diag)
        col = np.append(c_off_diag,r_off_diag)
        fij = np.append(f_off_diag,-f_off_diag)

        dof = csr((fij,(row,col)),shape=(self.grids.number,
                                         self.grids.number))

        self.Amatrix = dof.multiply(csr(dnm).T)

    def implement_bc(self,bvector):
        
        self.bvector = bvector
        
##        xL = lower_boundary[0]
##        dL = lower_boundary[1]
##        nL = lower_boundary[2]
##        fL = lower_boundary[3]
##        
##        xU = upper_boundary[0]
##        dU = upper_boundary[1]
##        nU = upper_boundary[2]
##        fU = upper_boundary[3]

        for j,row in enumerate(self.grids.boundary_ids):

            dxS = self.grids.sizes[row[0]] # self
            dxN = self.grids.sizes[row[1]] # neighbour

            Dbc = self.grids.boundary_conditions[j,0]
            Nbc = self.grids.boundary_conditions[j,1]
            Fbc = self.grids.boundary_conditions[j,2]

            bc = 8/((3*dxS+dxN)*(Dbc*dxS-2*Nbc))

            self.Amatrix[row[0],row[0]] -= bc*Dbc
            self.bvector[row[0]] -= bc*Fbc

##        bL = 8/((3*self.dx[0]+self.dx[1])*(dL*self.dx[0]-2*nL))
##        self.Amatrix[0,0] = self.Amatrix[0,0]-bL*dL
##        self.bvector[0] = self.bvector[0]-bL*fL
##
##        bU = 8/((3*self.dx[-1]+self.dx[-2])*(dU*self.dx[-1]-2*nU))
##        self.Amatrix[-1,-1] = self.Amatrix[-1,-1]-bU*dU
##        self.bvector[-1] = self.bvector[-1]-bU*fU

    def solve(self):

        self.unknowns = spsolve(self.Amatrix,self.bvector)

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

def central(x0,xL,dx,order=1,horder=2):
    """
    version 1: calculating derivatives at the nodes
    """

    x = np.arange(x0,xL+dx/2,dx)

    N = x.shape[0]

    idx = np.arange(N)

    if horder==2:

        dtw = np.zeros(2*N-2)

        dtw[0    :1*N-1] = order*(-1)**(order)
        dtw[1*N-1:2*N-2] = order
        
        rwn = np.concatenate((idx[1:],idx[:-1]))
        cln = np.concatenate((idx[:-1],idx[1:]))

        denominator = 2*dx**(order)
            
    elif horder==4:

        dtw = np.zeros(4*N-6)

        dtw[0    :1*N-2] = (-1)**(order+1)
        dtw[1*N-2:2*N-3] = 8*order*(-1)**(order)
        dtw[2*N-3:3*N-4] = 8*order
        dtw[3*N-4:4*N-6] = (-1)
        
        rwn = np.concatenate((idx[2:],idx[1:],idx[:-1],idx[:-2]))
        cln = np.concatenate((idx[:-2],idx[:-1],idx[1:],idx[2:]))

        denominator = 12*dx**(order)

    row = np.concatenate((rwn,rwn))
    col = np.concatenate((cln,rwn))
    dat = np.concatenate((dtw,-dtw))

    D = csr((dat,(row,col)),shape=(N,N))

    return D/denominator

if __name__ == "__main__":

##    x0 = 0
##    xL = np.pi*2
##    dx = np.pi/30
##    xN = np.arange(x0,xL+dx/2,dx)

    x0 = 0
    xL = 1
    dx = 1/6.

    xN = np.arange(x0,xL+dx/2,dx)

    N = xN.size

##    dN,G2 = central(myfunc2,x0,xL,dx,order=2,horder=2)
##    dN,G1 = central(myfunc2,x0,xL,dx,order=1,horder=2)
##
##    A = G2/(dx**2)+G1/(2*dx)
##    
##    b = np.ones(N)
##    
##    A[0,:] = 0
##    A[0,0] = 1
##    
##    b[0] = 0
##    
##    A[-1,: ] = 0
##    A[-1,-1] = 1
##    
##    b[-1] = 0
##
##    uN = np.linalg.solve(A,b)
##
####    uA = xN+np.exp(-xN)-1
##    uA = xN+np.exp(1)/((np.exp(1)-1))*(np.exp(-xN)-1)
##
####    xA = np.arange(x0,xL+np.pi/180,np.pi/90)
####    dA = myfunc2(xA,2)
    G = central(x0,xL,dx,order=2,horder=2)
    A = myfunc2(xN,2)

    plt.scatter(xN[1:-1],G.dot(myfunc2(xN,0))[1:-1])
    plt.plot(xN,A)
    plt.show()
##
##    plt.plot(xN,uA,c='k')
##    plt.scatter(xN,uN,c='b')
##    plt.show()
 
