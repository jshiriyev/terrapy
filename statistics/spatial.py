import os
import sys

import numpy as np

from scipy.stats import norm

from connectivity import variogram

class item():

    def __init__(self,props,header=None,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given property"""

        if props is not None:
            self.property = props
            ones = np.ones(props.shape[0])
        elif X is not None:
            ones = np.ones(X.shape)
        elif Y is not None:
            ones = np.ones(Y.shape)
        elif Z is not None:
            ones = np.ones(Z.shape)
        else:
            return

        if header is not None:
            self.header = header
        
        if X is None:
            try:
                self.x = (np.cumsum(ones,0)-1)*dX
            except:
                self.x = ones
        else:
            self.x = X

        if Y is None:
            try:
                self.y = (np.cumsum(ones,1)-1)*dY
            except:
                self.y = ones
        else:
            self.y = Y

        if Z is None:
            try:
                self.z = (np.cumsum(ones,2)-1)*dZ
            except:
                self.z = ones
        else:
            self.z = Z

class estimation(item):

    """
    estimation class used to represent estimated points

    ...

    Attributes
    ----------
    var:
    type:
    nugget:
    sill:
    range:
    
    distance:
    theoretical:
    covariance:
    lambdas:
    property:
    variance:
    mean:
    beta:
    
    Methods
    -------
    interpolation()
    kriging_simple()
        calculates simple kriging values at estimation points
    kriging_ordinary()
        calculates ordinary kriging values at estimation points
    simulation_gaussian()
        calculates gaussian simulation values at estimation points
    
    """

    def __init__(self,var,**kwargs):
        """var must be theoretical variogram at observation points"""
        self.var = var

        self.type = self.var.type
        self.nugget = self.var.nugget
        self.sill = self.var.sill
        self.range = self.var.range
        
        super(estimation, self).__init__(None,**kwargs)

    def set_distance(self):
        """here distance is calculated in 2-dimensional array form"""
        self.dx = self.x-self.var.x.reshape((-1,1))
        self.dy = self.y-self.var.y.reshape((-1,1))
        self.dz = self.z-self.var.z.reshape((-1,1))

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

    def interpolation(self,X,Y,x):
        """1D interpolation, but here it must be 3D"""

        """
        X are the locations where Y values are given
        x are the locations where we want to calculate y
        based on the interpolation of Y values
        """
        
        xadded = X[x.max()<X]
        
        x = np.append(x,xadded)
        
        N = x.size
        L = X.size

        d1 = np.array(list(range(N)))
        d2 = np.array(list(range(N,N+L)))
        
        row = np.concatenate(((d1[0],d1[-1]),d1[:-1],d1[1:],d1[1:-1]))
        col = np.concatenate(((d1[0],d1[-1]),d1[1:],d1[:-1],d1[1:-1]))

        Mone = np.ones(2)*(-1)
        Pone = np.ones(2*(N-1))
        Mtwo = np.ones((N-2))*(-2)

        data = np.concatenate((Mone,Pone,Mtwo))

        G = sps.csr_matrix((data,(row,col)),shape=(N,N))
        d = sps.csr_matrix((Y,(d2,np.zeros(L))),shape=(N+L,1))

        x = x.reshape((1,-1))
        X = X.reshape((-1,1))
        
        Glow = np.zeros((L,N))

        dmat = np.abs(x-X)              # distance matrix for the given x and X vectors
        
        colID = dmat.argsort()[:,:2]    # column indices of two minimum row values in dmat
        rowID = np.tile(np.arange(L).reshape((-1,1)),2)
                                        # row indices of two minimum row values in dmat
        
        dmin = dmat[rowID,colID]        # two minimum distance values of each row in dmat

        Glow[rowID,colID] = 1-dmin/np.sum(dmin,axis=1,keepdims=True)

        G = sps.vstack([G,sps.csr_matrix(Glow)])

        A = G.transpose()*G
        b = G.transpose()*d

        y = sps.linalg.spsolve(A,b)

        if xadded.size:
            return y[:-xadded.size]
        else:
            return y

    def kriging_simple(self,perc=0.5):
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.set_distance()
        
        variogram.set_theoretical(self)
        
        self.covariance = self.sill-self.theoretical

        self.lambdas = np.linalg.solve(self.var.covariance,self.covariance)
        
        calc_diff_arr = np.reshape(self.var.property,(-1,1))-self.mean
        calc_prop_mat = self.lambdas*(calc_diff_arr)
        calc_vars_mat = self.lambdas*self.covariance
        
        self.property = self.mean+calc_prop_mat.sum(axis=0)
        self.variance = self.sill-calc_vars_mat.sum(axis=0)

        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def kriging_ordinary(self,perc=0.5):
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.set_distance()
        
        variogram.set_theoretical(self)

        self.covariance = self.sill-self.theoretical
        
        Am = self.var.covariance
        Ar = np.ones(Am.shape[0]).reshape((-1,1))
        Ab = np.ones(Am.shape[0]).reshape((1,-1))
        Ab = np.append(Ab,np.array([[0]]),axis=1)
        Am = np.append(Am,Ar,axis=1)
        Am = np.append(Am,Ab,axis=0)

        bm = self.covariance
        bb = np.ones(bm.shape[1]).reshape((1,-1))
        bm = np.append(bm,bb,axis=0)

        xm = np.linalg.solve(Am,bm)
        
        self.lambdas = xm[:-1,:]
        self.beta = xm[-1,:]

        calc_prop_mat = self.lambdas*np.reshape(self.var.property,(-1,1))
        calc_vars_mat = self.lambdas*self.covariance

        self.property = calc_prop_mat.sum(axis=0)
        self.variance = self.sill-self.beta-calc_vars_mat.sum(axis=0)
        
        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def simulation_gaussian(self):

        perc = np.random.rand(self.x.size)

        self.kriging_ordinary(perc=perc)

if __name__ == "__main__":

    import unittest
    
    from tests import test_spatial
    
    unittest.main(test_spatial)
