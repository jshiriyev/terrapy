# Standard library imports
import os
import sys

##sys.path.append(os.getcwd())

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

# Local application imports
from data import item

"""
hetereogeneity
uncertainty
variogram
spatial_estimation
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

    """
    demonstrates standard variance calculations
    demonstrates Dykstra-Parson coefficient calculations
    demonstrates Lorenz coefficient calculations
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

    """
    demonstrates experimental variogram calculations
    demonstrates theoretical variogram calculation
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

class spatial_estimation(item):

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
    simple_kriging()
        calculates simple kriging values at estimation points
    ordinary_kriging()
        calculates ordinary kriging values at estimation points
    gaussian_simulation()
        calculates gaussian simulation values at estimation points
    
    """

    """
    demonstrates simple kriging calculations
    demonstrates ordinary kriging calculations
    demonstrates gaussian simulation calculations
    """

    def __init__(self,var,**kwargs):
        """var must be theoretical variogram at observation points"""
        self.var = var

        self.type = self.var.type
        self.nugget = self.var.nugget
        self.sill = self.var.sill
        self.range = self.var.range
        
        self.set_property(None,**kwargs)

    def simple_kriging(self,prop,perc=0.5):

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

    def ordinary_kriging(self,prop,perc=0.5):

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

    def gaussian_simulation(self,prop):

        perc = np.random.rand(self.x.size)

        self.ordinary_kriging(prop,perc=perc)

if __name__ == "__main__":

    ## Exercise: Heterogeneity Measures
    
##    parf = os.path.dirname(os.getcwd())
##
##    data = np.loadtxt(str(parf)+"\\"+"sample.txt",skiprows=1)
##
##    p = data[:,0]
##    k = data[:,1]
##    t = np.ones_like(k)
##
##    pm = heterogeneity({"porosity":p,
##                        "permeability":k,
##                        "thickness":t
##                        })
##    
##    print(pm.standard("permeability"))
##    print(pm.dykstraparson("permeability"))
##    print(pm.lorenz())

    ## Exercise: Experimental Variogram
    
##    z = np.array([[32,24,20,10],
##                  [28,20,17,12],
##                  [12,16,10,9],
##                  [18,12,7,8]])
##    
##    V = variogram({"porosity": z},dX=10,dY=10)
##    
##    V.set_distance(V)
##    
##    V.set_experimental("porosity",10,30,azimuth=-90,azimuth_tol=2)
##    print(V.experimental)
##    V.set_experimental("porosity",20*np.sqrt(2),20*np.sqrt(2),azimuth=-135,azimuth_tol=2)
##    print(V.experimental)

    ## Exercise: Theoretical Semi-Variogram Models

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

    ## Example 4.2 (Kriging) and 4.3 (Simulation) page 187, Peters Volume 1

##    x = np.array([2,4,6])
##    y = np.array([30,50,20])
##    
##    plt.grid(alpha=0.2)
##    plt.xlabel('x-axis',fontsize=14)
##    plt.ylabel('property',fontsize=14)
##    
##    plt.xlim([0,9])
##    plt.ylim([0,70])
##
##    V = variogram({"porosity": y},X=x)
##    
##    V.set_distance(V)
##
##    V.type = 'exponential'
##    V.nugget = 0
##    V.sill = 100
##    V.range = 10
##
##    V.set_theoretical()
##
##    X = np.array([1,2,3,4,5,6,7,8])
##
##    xe = np.linspace(1,8,701)
##
##    E = spatial_estimation(V,X=xe)
##
##    E.ordinary_kriging("porosity")
##
##    plt.plot(E.x,E.property,c='k')
##
##    E.ordinary_kriging("porosity",perc=0.975)
##
##    y1 = E.property
##
##    E.ordinary_kriging("porosity",perc=0.025)
##    
##    y2 = E.property
##
##    plt.fill_between(E.x,y1,y2,fc='lightgrey')
##
##    xe = np.linspace(1,8,71)
##
##    E = spatial_estimation(V,X=xe)
##
##    E.gaussian_simulation("porosity")
##
##    plt.scatter(E.x,E.property,s=4,c='r')
##
##    plt.scatter(x,y,marker='X',c='k')
##
##    plt.show()

    ## Class Exercise 2

    x = np.array([600,400,800])
    y = np.array([800,700,100])

    z = np.array([0.25,0.43,0.56])

    V = variogram({"porosity": z},X=x,Y=y)
    
    V.set_distance(V)

    V.type = 'spherical'
    V.nugget = 0
    V.sill = 0.0025
    V.range = 700

    V.set_theoretical()
    
    plt.figure(1)

    plt.scatter(V.x,V.y,s=20,c=V.porosity,alpha=0.5)
    plt.colorbar()

    plt.xlim([0,1000])
    plt.ylim([0,1000])

    plt.xlabel('x-axis')
    plt.ylabel('y-axis')

##    plt.show()
    
##    x = np.array([500])
##    y = np.array([500])

##    E = kriging(V,X=x,Y=y)
    
##    E.simple_kriging()
    
    xlin = np.linspace(0,1000,50)
    ylin = np.linspace(0,1000,50)

    [Xmesh,Ymesh] = np.meshgrid(xlin,ylin)
    
    E = spatial_estimation(V,X=Xmesh.flatten(),Y=Ymesh.flatten())

##    E.mean = 0.38
    
##    E.simple_kriging("porosity")

    E.gaussian_simulation("porosity")

    plt.figure(2)

    plt.contourf(Xmesh,Ymesh,E.property.reshape(50,50));
    plt.colorbar()

    plt.xlabel('x-axis')
    plt.ylabel('y-axis')

    plt.xlim([0,1000])
    plt.ylim([0,1000])

    plt.show()

    ## Exercise 4.14 Peters, page 206 Volume 1 Sequential GAUSSIAN SIMULATION

##    x = np.array([1,2,4,5,6])
##    y = np.array([2,10,4,6,14])
##    z = np.array([15,30,25,18,30])
##
##    V = variogram({'porosity': z},X=x,Y=y)
##
##    V.set_distance(V)
##
##    V.type = 'spherical'
##    V.nugget = 0
##    V.sill = 60
##    V.range = 8
##
##    V.set_theoretical()
##
##    X = np.array([2,4])#([1,2,2,4,4,5,6])
##    Y = np.array([6,12])#([2,6,10,12,4,6,14])
##
##    E = simulation(V,X=X,Y=Y)
##
##    E.sequential_gaussian("porosity")
