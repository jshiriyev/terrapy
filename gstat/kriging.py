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
from bhos.variogram import variogram

"""
demonstrates simple kriging calculations
demonstrates ordinary kriging calculations
"""

class kriging(item):

    """
    kriging class used to represent estimated points

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
    simple()
        calculates simple kriging values at observation points
    ordinary()
        calculates ordinary kriging values at observation points
    
    """

    def __init__(self,var,**kwargs):
        """var must be theoretical variogram at observation points"""
        self.var = var

        self.type = self.var.type
        self.nugget = self.var.nugget
        self.sill = self.var.sill
        self.range = self.var.range
        
        self.set_property(None,**kwargs)

    def simple(self,prop,perc=0.5):

        "prop -> which property to krig"
        "perc -> percentile, perc=0.5 gives mean values"

        values = getattr(self.var,prop)

        variogram.set_distance(self,self.var)
        variogram.set_theoretical(self)
        
        self.covariance = self.sill-self.theoretical

        self.lambdas = np.linalg.solve(self.var.covariance,self.covariance)
        
        calc_diff_arr = np.reshape(values,(-1,1))-self.mean
        calc_prop_mat = self.lambdas*(calc_diff_arr)
        calc_vars_mat = self.lambdas*self.covariance
        
        self.property = self.mean+calc_prop_mat.sum(axis=0)
        self.variance = self.sill-calc_vars_mat.sum(axis=0)

        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def ordinary(self,prop,perc=0.5):

        "prop -> which property to krig"
        "perc -> percentile, perc=0.5 gives mean values"
        
        values = getattr(self.var,prop)

        variogram.set_distance(self,self.var)
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

        calc_prop_mat = self.lambdas*np.reshape(values,(-1,1))
        calc_vars_mat = self.lambdas*self.covariance

        self.property = calc_prop_mat.sum(axis=0)
        self.variance = self.sill-self.beta-calc_vars_mat.sum(axis=0)

        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

if __name__ == "__main__":

    ## Class Exercise 1

    x = np.array([2,4,6])
    y = np.array([30,50,20])
    
    plt.scatter(x,y,c='k')
    plt.grid(alpha=0.2)
    plt.xlabel('x-axis',fontsize=14)
    plt.ylabel('property',fontsize=14)
    plt.xlim([0,9])
    plt.ylim([0,70])

    V = variogram({"porosity": y},X=x)
    
    V.set_distance(V)

    V.type = 'exponential'
    V.nugget = 0
    V.sill = 100
    V.range = 10

    V.set_theoretical()

    X = np.array([1,2,3,4,5,6,7,8])

    x = np.linspace(1,8,701)

    E = kriging(V,X=x)

    E.ordinary("porosity")

    plt.plot(E.x,E.property,c='k')

    E.ordinary("porosity",perc=0.975)

    y1 = E.property

    E.ordinary("porosity",perc=0.025)
    
    y2 = E.property

    plt.fill_between(E.x,y1,y2,fc='lightgrey')

    plt.show()

    ## Class Exercise 2

##    x = np.array([600,400,800])
##    y = np.array([800,700,100])
##
##    z = np.array([0.25,0.43,0.56])
##
##    V = variogram({"porosity": z},X=x,Y=y)
##    
##    V.set_distance(V)
##
##    V.type = 'spherical'
##    V.nugget = 0
##    V.sill = 0.0025
##    V.range = 700
##
##    V.set_theoretical()
##    
##    plt.figure(1)
##
##    plt.scatter(V.x,V.y,s=20,c=V.porosity,alpha=0.5)
##    plt.colorbar()
##
##    plt.xlim([0,1000])
##    plt.ylim([0,1000])
##
##    plt.xlabel('x-axis')
##    plt.ylabel('y-axis')
##
####    plt.show()
##    
####    x = np.array([500])
####    y = np.array([500])
##
####    E = kriging(V,X=x,Y=y)
##    
####    E.simple()
##    
##    xlin = np.linspace(0,1000,50)
##    ylin = np.linspace(0,1000,50)
##
##    [Xmesh,Ymesh] = np.meshgrid(xlin,ylin)
##    
##    E = kriging(V,X=Xmesh.flatten(),Y=Ymesh.flatten())
##
##    E.mean = 0.38
##    
##    E.simple("porosity")
##
##    plt.figure(2)
##
##    plt.contourf(Xmesh,Ymesh,E.property.reshape(50,50));
##    plt.colorbar()
##
##    plt.xlabel('x-axis')
##    plt.ylabel('y-axis')
##
##    plt.xlim([0,1000])
##    plt.ylim([0,1000])
##
##    plt.show()
