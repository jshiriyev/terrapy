import os
import sys

import numpy as np

from scipy.stats import norm

if __name__ == "__main__":
    import setup

from geomodel.connectivity import SpatProp

class interpolation():
    
    def __init__(self):
        pass

    def smooth(self):
        pass

    def interpolation(self,X,Y,x):

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

class kriging():

    def __init__(self,obsSpatProp,estSpatProp):
        
        # super(kriging,self).__init__(None,**kwargs)
        
        self.obs = obsSpatProp
        self.est = estSpatProp

    def simple(self,mean=None):

        if mean is None:
            self.mean = self.obs.mean()
        else:
            self.mean = mean
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.distance = SpatProp.get_distance(self.est,self.obs)
        
        _,self.covariance = SpatProp.get_varmodel(
            self.distance,self.obs.type,
            self.obs.sill,self.obs.range,
            self.obs.nugget)

        self.lambdas = np.linalg.solve(self.obs.covariance,self.covariance)
        
        calc_diff_arr = self.obs.reshape((-1,1))-self.mean
        calc_prop_mat = self.lambdas*(calc_diff_arr)
        calc_vars_mat = self.lambdas*self.covariance
        
        self.property = self.mean+calc_prop_mat.sum(axis=0)
        self.variance = self.obs.sill-calc_vars_mat.sum(axis=0)

    def ordinary(self):
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.distance = SpatProp.get_distance(self.est,self.obs)
        
        _,self.covariance = SpatProp.get_varmodel(
            self.distance,self.obs.type,
            self.obs.sill,self.obs.range,
            self.obs.nugget)
        
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

        calc_prop_mat = self.lambdas*self.obs.reshape((-1,1))
        calc_vars_mat = self.lambdas*self.covariance

        self.property = calc_prop_mat.sum(axis=0)
        self.variance = self.sill-self.beta-calc_vars_mat.sum(axis=0)

    def get_percentile(self,perc=0.5):

        return self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def gaussian_simulation(self):

        perc = np.random.rand(self.x.size)

        self.get_percentile(perc=perc)

if __name__ == "__main__":

    import unittest
    
    from tests import test_spatial
    
    unittest.main(test_spatial)
