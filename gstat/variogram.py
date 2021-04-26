# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports
from bhos.geostat import item

"""
demonstrates experimental variogram calculations
demonstrates theoretical variogram calculation
"""

class variogram(item):

    """
    variogram class used to represent observation points

    ...

    Attributes
    ----------
    property:
    x:
    y:
    z:
    
    dx:
    dy:
    dz:
    distance:

    bins:

    lagdis:
    lagdistol:
    azimuth:
    azimuth_tol:
    experimental:

    theoretical:
    covariance:
    
    Methods
    -------
    set_property():
        assigns input properties to the self
    set_distance(other):
        calculates distances between the provided two set of points
    set_experimental(prop,lagdis,lagmax,azimuth,azimuth_tol):
        calculates experimental variogram values
    set_theoretical():
        calculates theoretical variogram and covariance values
    
    """

    def __init__(self,props,**kwargs):
        
        self.set_property(props,**kwargs)

    def set_distance(self,other):
        """here distance is calculated in 2-dimensional array form"""

        # input x,y,z values should be zero dimensional array

        self.dx = np.reshape(other.x,(-1,1))-self.x
        self.dy = np.reshape(other.y,(-1,1))-self.y
        self.dz = np.reshape(other.z,(-1,1))-self.z

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

    def set_experimental(self,
                         prop,
                         lagdis,
                         lagmax,
                         azimuth=0,
                         azimuth_tol=22.5,
                         bandwidth=np.inf):

        # which property to use for experimental variogram generation

        values = getattr(self,prop)

        """bins is the array of lag distances"""

        self.lagdis = lagdis
        self.lagdistol = lagdis/2.
        self.lagmax = lagmax

        self.outbound = self.lagmax+self.lagdistol
        
        self.bins = np.arange(self.lagdis,self.outbound,self.lagdis)

        """
        - only directional experimental variogram calculation exist for now
        - calculations were verified for 2D data only
        """

        while azimuth<0:
            azimuth += 360

        while azimuth>360:
            azimuth -= 360

        self.azimuth = np.radians(azimuth)
        self.azimuth_tol = np.radians(azimuth_tol)
        self.bandwidth = bandwidth

        """
        if we set x direction as east and y direction as north
        then the following azimuth will be zero toward east and
        will be positive in counterclockwise direction
        """
        
        self.degree = np.arctan2(self.dy,self.dx)+np.pi
        self.degree = np.where(self.degree==2*np.pi,0,self.degree)

        """
        finding indexes when lag_distance matches data spacing, disMatch
        and when dip angle matches azimuth, azmMatch
        for non uniform spacing most modification will be here probably.
        """

        conAzm = np.logical_and(self.degree>self.azimuth-self.azimuth_tol,
                                self.degree<self.azimuth+self.azimuth_tol)
        
        azmMatch = np.asfortranarray(np.where(conAzm)).T

        self.experimental = np.zeros_like(self.bins)

        for i,h in enumerate(self.bins):
            
            conDis = np.logical_and(self.distance>h-self.lagdistol,self.distance<h+self.lagdistol)

            disMatch = np.asfortranarray(np.where(conDis)).T

            """
            comparing disMatch to azmMatch to find indices matching both
            """

            dtype={'names':['f{}'.format(i) for i in range(2)],'formats':2*[disMatch.dtype]}

            match = np.intersect1d(disMatch.view(dtype),azmMatch.view(dtype))
            match = match.view(disMatch.dtype).reshape(-1,2)

            p1 = values[match[:,0]]
            p2 = values[match[:,1]]

            semivariance = ((p1-p2)**2).sum()/(2*match.shape[0])

            self.experimental[i] = semivariance

    def draw_search_box(self,origin_x=0,origin_y=0):

        """Nomenclature-BEGINNING"""
        ## alpha  : azimuth_tol at bandwidth dominated section
        ## omega  : bandwidth at azimuth_tol dominated section
        ## theta  : azimuth range at the specified distance
        """Nomenclature-END"""

        def azmtol(bandwidth,bound,azm_tol):
            return(np.arcsin(min(np.sin(azm_tol),bandwidth/bound)))

        def bndwdt(bandwidth,bound,azm_tol):
            return min(bandwidth,bound*np.sin(azm_tol))

        alpha = azmtol(self.bandwidth,self.outbound,self.azimuth_tol)
        omega = bndwdt(self.bandwidth,self.outbound,self.azimuth_tol)

        theta = np.linspace(self.azimuth-alpha,self.azimuth+alpha)
        sides = omega/np.sin(self.azimuth_tol)

        xO1 = self.outbound*np.cos(self.azimuth)
        yO1 = self.outbound*np.sin(self.azimuth)

        xO2 = self.outbound*np.cos(self.azimuth-alpha)
        yO2 = self.outbound*np.sin(self.azimuth-alpha)

        xO3 = self.outbound*np.cos(self.azimuth+alpha)
        yO3 = self.outbound*np.sin(self.azimuth+alpha)

        xO4 = sides*np.cos(self.azimuth-self.azimuth_tol)
        yO4 = sides*np.sin(self.azimuth-self.azimuth_tol)

        xO5 = sides*np.cos(self.azimuth+self.azimuth_tol)
        yO5 = sides*np.sin(self.azimuth+self.azimuth_tol)

        x1 = np.linspace(0,xO1)
        y1 = np.linspace(0,yO1)

        x2 = np.linspace(xO4,xO2)
        y2 = np.linspace(yO4,yO2)

        x3 = np.linspace(xO5,xO3)
        y3 = np.linspace(yO5,yO3)

        x4 = np.linspace(0,xO4)
        y4 = np.linspace(0,yO4)

        x5 = np.linspace(0,xO5)
        y5 = np.linspace(0,yO5)

        x6 = self.outbound*np.cos(theta)
        y6 = self.outbound*np.sin(theta)

        plt.plot(origin_x+x1,origin_y+y1,'b--')
        plt.plot(origin_x+x2,origin_y+y2,'k')
        plt.plot(origin_x+x3,origin_y+y3,'k')
        plt.plot(origin_x+x4,origin_y+y4,'k')
        plt.plot(origin_x+x5,origin_y+y5,'k')
        plt.plot(origin_x+x6,origin_y+y6,'k')

        for h in self.bins:
            
            hmin = h-self.lagdistol
            
            hmin_alpha = azmtol(self.bandwidth,hmin,self.azimuth_tol)
            hmin_theta = np.linspace(self.azimuth-hmin_alpha,self.azimuth+hmin_alpha)
            
            hmin_x = hmin*np.cos(hmin_theta)
            hmin_y = hmin*np.sin(hmin_theta)

            plt.plot(origin_x+hmin_x,origin_y+hmin_y,'r')

    def set_theoretical(self,bins=None):

        if bins is not None:
            h = bins
        else:
            h = self.distance
        
        self.theoretical = np.zeros_like(h)
        
        Co = self.nugget
        Cd = self.sill-self.nugget
        
        if self.type == 'power':
            self.theoretical[h>0] = Co+Cd*(h[h>0])**self.power
        elif self.type == 'spherical':
            self.theoretical[h>0] = Co+Cd*(3/2*(h[h>0]/self.range)-1/2*(h[h>0]/self.range)**3)
            self.theoretical[h>self.range] = self.sill
        elif self.type == 'exponential':
            self.theoretical[h>0] = Co+Cd*(1-np.exp(-3*(h[h>0]/self.range)))
        elif self.type == 'gaussian':
            self.theoretical[h>0] = Co+Cd*(1-np.exp(-3*(h[h>0]/self.range)**2))
        elif self.type == 'hole_effect':
            self.theoretical[h>0] = Co+Cd*(1-np.sin((h[h>0]/self.range))/(h[h>0]/self.range))

        self.covariance = self.sill-self.theoretical
        
if __name__ == "__main__":

    ## Class Exercise
    
    z = np.array([[32,24,20,10],
                  [28,20,17,12],
                  [12,16,10,9],
                  [18,12,7,8]])
    
    V = variogram({"porosity": z},dX=10,dY=10)
    
    V.set_distance(V)
    
    V.set_experimental("porosity",10,30,azimuth=-90,azimuth_tol=2)
    print(V.experimental)
    V.set_experimental("porosity",20*np.sqrt(2),20*np.sqrt(2),azimuth=-135,azimuth_tol=2)
    print(V.experimental)

    ## Theoretical Semi-Variogram Models

##    V = variogram(None)
##
##    V.type = 'spherical'
##    
##    V.nugget = 0
##    V.sill = 10
##    V.range = 10
##
##    x = np.linspace(0,20,100)
##
##    V.set_theoretical(bins=x)
##
##    plt.plot(x,V.theoretical)
##
##    plt.show()
