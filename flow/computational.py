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

    def __init__(self,dimension):

        self.dimension = dimension

    def cartesian(self,length,
                       number_of_grids,
                       boundary1,
                       boundary2,
                       boundary3=None,
                       boundary4=None,
                       boundary5=None,
                       boundary6=None):

        #length is a tuple with three dimensional size information
        # for rectangular parallelepiped:

        self.xsize = length[0]
        self.ysize = length[1]
        self.zsize = length[2]

        #number of grids is a tuple with three entries:
        # - number of grids in x direction
        # - number of grids in y direction
        # - number of grids in z direction

        self.Nx = number_of_grids[0]
        self.Ny = number_of_grids[1]
        self.Nz = number_of_grids[2]

        #boundary have three entries:
        # - dirichlet coefficient,
        # - neumann coefficient,
        # - function value of boundary condition

        #total number of grids we have in the rectangular parallelepiped:
        self.number = self.Nx*self.Ny*self.Nz

        #self.id should contain id of grid of interest
        # and the id of neighbour grids.

        connectivity = np.arange(self.number)
        connectivity = np.repeat(connectivity,1+2*self.dimension)
        connectivity = connectivity.reshape(self.number,-1)
        
        self.id = connectivity.astype(float)

        coeff = np.append(np.array([1]),np.cumprod(number_of_grids)[:-1])

        for i in range(self.dimension):
            self.id[:,2*i+1] = self.id[:,2*i+1]-coeff[i]
            self.id[:,2*i+2] = self.id[:,2*i+2]+coeff[i]

        #index correction for the boundaries
        
        self.id[::self.Nx,1] = np.nan
        self.id[self.Nx-1::self.Nx,2] = np.nan

        self.id[:self.Nx,3] = np.nan
        self.id[self.Nx*(self.Ny-1):,4] = np.nan

        ##self.id[,5] = np.nan
        ##self.id[,6] = np.nan

        #self.boundary should contain id of boundary grids,
        # the id of its neighbors and boundary conditions.

        Nboundary = lambda x: 1 if x>1 else 0

        Bx1 = self.Ny*self.Nz*Nboundary(self.Nx)
        Bx2 = self.Ny*self.Nz*Nboundary(self.Nx)
        
        By1 = self.Nz*self.Nx*Nboundary(self.Ny)
        By2 = self.Nz*self.Nx*Nboundary(self.Ny)
        
        Bz1 = self.Nx*self.Ny*Nboundary(self.Nz)
        Bz2 = self.Nx*self.Ny*Nboundary(self.Nz)

        idb = np.array([Bx1,Bx2,By1,By2,Bz1,Bz2])
        idb = np.cumsum(idb)

        self.boundary = np.zeros((Bx1+Bx2+By1+By2+Bz1+Bz2,1+2*self.dimension+3))

        self.boundary[:idb[0],:-3] = self.id[::self.Nx,:]
        self.boundary[idb[0]:idb[1],:-3] = self.id[self.Nx-1::self.Nx,:]

        self.boundary[idb[1]:idb[2],:-3] = self.id[:self.Nx,:]
        self.boundary[idb[2]:idb[3],:-3] = self.id[self.Nx*(self.Ny-1):,:]

        ##self.boundary[idb[3]:idb[5],0] =
        ##self.boundary[idb[3]:idb[5],0] =

        self.boundary[:idb[0],5:] = np.array(boundary1)
        self.boundary[idb[0]:idb[1],5:] = np.array(boundary2)
        self.boundary[idb[1]:idb[2],5:] = np.array(boundary3)
        self.boundary[idb[2]:idb[3],5:] = np.array(boundary4)
        ##self.boundary[idb[3]:idb[4],5:] = np.array(boundary5)
        ##self.boundary[idb[4]:,5:] = np.array(boundary6)

        xnodes = np.linspace(0,self.xsize,self.Nx+1)
        ynodes = np.linspace(0,self.ysize,self.Ny+1)
        znodes = np.linspace(0,self.zsize,self.Nz+1)

        ##self.size = np.array([dx,dy,dz]).repeat(self.number).reshape((-1,3))
        ##self.center = self.node[:-1]+self.size/2

        xsize = xnodes[1:]-xnodes[:-1]
        ysize = ynodes[1:]-ynodes[:-1]
        zsize = znodes[1:]-znodes[:-1]

        cx = xnodes[:-1]+xsize/2
        cy = ynodes[:-1]+ysize/2
        cz = znodes[:-1]+zsize/2

        cx = cx.reshape((-1,1)).repeat(self.Ny*self.Nz,axis=1).T.reshape((-1,1))
        cy = cy.reshape((-1,1)).repeat(self.Nz*self.Nx).reshape((-1,1))
        cz = cz.reshape((-1,1)).repeat(self.Nx*self.Ny,axis=1).T.reshape((-1,1))#there can be error

        self.center_x = cx
        self.center_y = cy
        self.center_z = cz

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
 
