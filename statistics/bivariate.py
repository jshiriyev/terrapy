# Standard library imports
import os
import sys

sys.path.append(os.getcwd())

# Third party imports
import numpy as np

from scipy import sparse as sps

# Local application imports
from geostat import csvreader

"""
bivariate analysis:
- findslope: calculates slope and intercept from linear regression
- correlation: correlation coefficient calculator

the functions below looks like more of a univariate analysis:
    
- ranking: ranks the given 1D numpy array (more of a function for now)
- bootstrap: bootstraps the given 1D numpy array
- interpolation: 1D dimensional analysis
"""

class item():
    """statistical item locating data and its spatial and time attachment"""

    def __init__(self,props,**kwargs):
        
        self.set_property(props,**kwargs)

    def set_property(self,props,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given properties"""
        
        ## it inputs props as dictionary and does not really do any check
        
        if type(props) is dict:
            ones = np.ones_like(np.array(list(props.values())[0]))
            for key,value in props.items():
                setattr(self,key,value.ravel())
        ##if props is not None:
            ##ones = np.ones_like(props)
            ##self.property = props.ravel()
        elif X is not None:
            ones = np.ones_like(X)
        elif Y is not None:
            ones = np.ones_like(Y)
        elif Z is not None:
            ones = np.ones_like(Z)
        else:
            return

        if X is None:
            try: self.x = (np.cumsum(ones,0)-1).ravel()*dX
            except: self.x = ones.ravel()
        else: self.x = X.ravel()

        if Y is None:
            try: self.y = (np.cumsum(ones,1)-1).ravel()*dY
            except: self.y = ones.ravel()
        else: self.y = Y.ravel()

        if Z is None:
            try: self.z = (np.cumsum(ones,2)-1).ravel()*dZ
            except: self.z = ones.ravel()
        else: self.z = Z.ravel()

class heterogeneity(item):

    """
    univariate class carries calculations on non-spatial data

    ...

    Attributes
    ----------
    permeability
    porosity
    thickness
    
    Methods
    -------
    lorenz():
        calculates Lorenz coefficient
    
    """

    """
    demonstrates standard variance calculations
    demonstrates Dykstra-Parson coefficient calculations
    demonstrates Lorenz coefficient calculations
    """

    def __init__(self,props,**kwargs):

        self.set_property(props,**kwargs)
    
    def lorenz(self):

        permt = getattr(self,"permeability")
        pores = getattr(self,"porosity")
        thick = getattr(self,"thickness")

        pr = np.flip(permt.argsort())

        sk = permt[pr]
        sp = pores[pr]
        st = thick[pr]

        flowing_capacity = sk*st
        storing_capacity = sp*st
        
        Xaxis = np.cumsum(flowing_capacity)/np.sum(flowing_capacity)
        Yaxis = np.cumsum(storing_capacity)/np.sum(storing_capacity)

        ##plt.plot(Xaxis,Yaxis)
        ##plt.show()

        area = np.trapz(Xaxis,Yaxis)
        
        coefficient = (area-0.5)/0.5
        
        return coefficient

class bivariate():

    def __init__(self,filename):

        self = csvreader(self,filename)

    def findslope(self,X,Y,*args):
        
        if not args:
            flag = False
        else:
            flag = args[0]

        N = X.size

        O = np.ones((N,1))
        X = X.reshape((-1,1))
        Y = Y.reshape((-1,1))

        G = np.concatenate((O,X),axis=1)

        A = np.dot(G.transpose(),G)
        b = np.dot(G.transpose(),Y)

        m = np.linalg.solve(A,b)
        
        if flag:
            yapp = np.dot(G,m)	# approximated y
            return m, yapp.flatten()
        else:
            return m

    def correlation(self,x,y):

        X = getattr(self,x)
        Y = getattr(self,y)

        N = X.shape[0]

        std_X = np.sqrt(1/(N-1)*np.sum((X-X.mean())**2))
        std_Y = np.sqrt(1/(N-1)*np.sum((Y-Y.mean())**2))
        
        cov_XY = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))  
        rho_XY = cov_XY/(std_X*std_Y)
        
        return rho_XY

    def ranking(self,X):
        
        rank = np.empty_like(X)
        
        rank[X.argsort()] = np.arange(len(X))

        return rank

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

if __name__ == "__main__":

    raman = bivariate("univariate.csv")

    print(raman.correlation('porosity','permeability'))
