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

    def __init__(self,number):

        self.number = number # number of elements

    def cartesian(self,x0,x1,BC):

        #ids should contain id of element of interest
        #and the id of neighbour elements

        id0 = np.arange(self.number)
        
        idxp = np.append(id0[1:],np.nan)
        idxm = np.insert(id0[:-1].astype(float),0,np.nan)

        self.id = np.concatenate((id0,idxm,idxp)).reshape(3,-1).T

##        id0[id2[~np.isnan(id2)].astype(int)]

        id0[np.any(np.isnan(idx),axis=1)]
        
        self.node = np.linspace(x0,x1,self.number+1)
        self.size = self.node[1:]-self.node[:-1]
        self.center = self.node[:-1]+self.size/2

##        BC = np.array([[1,0,200],[0,1,0]])
        
        self.boundary = self.id[np.any(np.isnan(self.id),axis=1)]
        self.boundary = np.append(self.boundary,BC,axis=1)

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
 
