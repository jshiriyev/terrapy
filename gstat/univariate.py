# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

# Local application imports
from bhos.geostat import item

"""
demonstrates standard variance calculations
demonstrates Dykstra-Parson coefficient calculations
demonstrates Lorenz coefficient calculations
"""

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
    set_property():
        assigns input properties to the self
    standard():
        calculates standard variance
    dykstraparson():
        calculates Dykstra-Parson coefficient
    lorenz():
        calculates Lorenz coefficient
    
    """

    def __init__(self,props,**kwargs):

        self.set_property(props,**kwargs)

##    def set_property(self,permeability=None,porosity=None,thickness=None):
##
##        if permeability is not None:
##            ones = np.ones_like(permeability)
##        elif porosity is not None:
##            ones = np.ones_like(porosity)
##        elif thickness is not None:
##            ones = np.ones_like(thickness)
##        else:
##            return
##
##        self.k = permeability
##
##        if porosity is not None:
##            self.p = ones*porosity
##
##        if thickness is not None:
##            self.t = ones*thickness

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

    def __init__(self,*args):

        for i,arg in enumerate(args):
            name = "prop"+str(i)
            setattr(self,name,arg)

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

if __name__ == "__main__":
    
    parf = os.path.dirname(os.getcwd())

    data = np.loadtxt(str(parf)+"\\"+"sample.txt",skiprows=1)

    p = data[:,0]
    k = data[:,1]
    t = np.ones_like(k)

    pm = heterogeneity({"porosity":p,
                        "permeability":k,
                        "thickness":t
                        })
    
    print(pm.standard("permeability"))
    print(pm.dykstraparson("permeability"))
    print(pm.lorenz())
