"""Standard library imports"""
import os
import sys

"""Third party imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

"""
univariate classes uses a single property data item with optional
spatial information and includes following ananlysis:

    - non-spatial similarity measurements (heterogeneity)
    - non-spatial estimation calculations (uncertainty)
    
    - spatial continuity measurements (variogram)
    - spatial estimation calculations (kriging, gaussian simulation)
    
"""

class heterogeneity(item):

    """
    univariate class carries calculations on non-spatial data

    ...

    Attributes
    ----------
    
    Methods
    -------
    set_property():
        assigns input properties to the self
    standard():
        calculates standard variance
    dykstraparson():
        calculates Dykstra-Parson coefficient
    """

    def __init__(self,prop,**kwargs):

        self.set_property(prop,**kwargs)

    def standard(self,prop):

        values = getattr(self,prop)

        return values.std()/values.mean()
    
    def dykstraparson(self,prop):

        values = getattr(self,prop)

        pr = np.flip(values.argsort())

        sk = values[pr]
        
        numdata = sk.shape[0]

        probs = 1/(numdata+1)

        xaxis = np.linspace(1,numdata,numdata)
        xaxis = norm.ppf(xaxis*probs)

        yaxis = np.log(sk)
        ##yaxis2 = np.log10(sortedPerm)
        ##plt.plot(xaxis,yaxis,'k.')

        m,c = np.polyfit(xaxis,yaxis,1)

        ybestfit = m*xaxis+c

        ##plt.plot(xaxis,ybestfit,'k')
        ##plt.show()

        k50p0 = np.exp(m*norm.ppf(0.5)+c)
        k84p1 = np.exp(m*norm.ppf(0.841)+c)

        coefficient = (k50p0-k84p1)/k50p0
        
        return coefficient

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

class uncertainty():

    def __init__(self,prop,**kwargs):

        self.set_property(prop,**kwargs)

    def jacknife(self):

        pass

    def bootstrap(self,X,Nrealization):

        """
        X should be an array with one dimension,
        The size of X defines number of rows, and
        Nrealization specifies number of columns of an array
        created for bootstrap analyzes
        """
        
        N = X.size
        
        idx = np.random.randint(0,N,(N,Nrealization))
        
        return idx

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




if __name__ == "__main__":

    pass
