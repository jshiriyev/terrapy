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

    - non-spatial homogeneity
    - non-spatial estimation (uncertainty)
    - spatial continuity (variogram)
    - spatial estimation (kriging, gaussian simulation)
    
"""

class item():
    """statistical item with a single (spatio-temporal) property type"""

    def __init__(self,prop,**kwargs):
        
        self.set_property(prop,**kwargs)

    def set_property(self,prop,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given property"""
        
        ones = np.ones(prop.shape)
        
        setattr(self,"property",prop.ravel())
        
        if X is None:
            try:
                self.x = (np.cumsum(ones,0)-1).ravel()*dX
            except:
                self.x = ones.ravel()
        else:
            self.x = X.ravel()

        if Y is None:
            try:
                self.y = (np.cumsum(ones,1)-1).ravel()*dY
            except:
                self.y = ones.ravel()
        else:
            self.y = Y.ravel()

        if Z is None:
            try:
                self.z = (np.cumsum(ones,2)-1).ravel()*dZ
            except:
                self.z = ones.ravel()
        else:
            self.z = Z.ravel()

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
    """

    def __init__(self,props,**kwargs):

        self.set_property(props,**kwargs)

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

        """calculating distance and angle between observation points"""
        self.set_connection()

    def set_connection(self):
        """distance is calculated in 2-dimensional array form"""
        self.dx = self.x-self.x.reshape((-1,1))
        self.dy = self.y-self.y.reshape((-1,1))
        self.dz = self.z-self.z.reshape((-1,1))

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

        """angle is calculated in 2-dimensional array form"""
        self.angle = np.arctan2(self.dy,self.dx)

    def set_experimental(self,
                         prop,
                         lagdis,
                         lagmax,
                         azimuth=None,
                         azimuthtol=None,
                         bandwidth=None):

        """selecting the property for experimental variogram generation"""
        values = getattr(self,prop)
        
        properr = (values-values.reshape((-1,1)))**2

        """bins is the array of lag distances"""
        self.lagdis = lagdis
        self.lagdistol = lagdis/2.
        self.lagmax = lagmax

        self.outbound = self.lagmax+self.lagdistol
        
        self.bins = np.arange(self.lagdis,self.outbound,self.lagdis)

        self.experimental = np.zeros_like(self.bins)

        """
        azimuth range is (-\pi,\pi] in radians or (-180,180] in degrees
        if we set +x to east and +y to north then the azimuth is selected
        to be zero in the +x direction and positive counterclockwise
        """

        """for an anisotropy only 2D data can be used FOR NOW"""
        self.azimuth = np.radians(azimuth)
        self.azimuthtol = np.radians(azimuthtol)
        self.bandwidth = bandwidth

        if self.azimuth is not None:
            delta_angle = np.abs(self.angle-azimuth)
            
            con_azmtol = delta_angle<=self.azimuthtol
            con_banwdt = np.sin(delta_angle)*self.distance<=(self.bandwidth/2.)
            con_direct = np.logical_and(con_azmtol,con_banwdt)
        else:
            con_direct = np.ones(self.angle.shape,dtype=bool)

        for h in self.bins:

            con_distnc = np.abs(self.distance-h)<=self.lagdistol

            conoverall = np.logical_and(con_distnc,con_direct)

            num_matchcon = np.count_nonzero(conoverall)

            semivariance = properr[conoverall].sum()/(2*num_matchcon)
            
            self.experimental[i] = semivariance

##    def set_experimental(self,
##                         prop,
##                         lagdis,
##                         lagmax,
##                         azimuth=0,
##                         azimuthtol=22.5,
##                         bandwidth=np.inf):
##
##        # which property to use for experimental variogram generation
##
##        values = getattr(self,prop)
##
##        """bins is the array of lag distances"""
##
##        self.lagdis = lagdis
##        self.lagdistol = lagdis/2.
##        self.lagmax = lagmax
##
##        self.outbound = self.lagmax+self.lagdistol
##        
##        self.bins = np.arange(self.lagdis,self.outbound,self.lagdis)
##
##        """
##        - only directional experimental variogram calculation exist for now
##        - calculations were verified for 2D data only
##        """
##
##        while azimuth<0:
##            azimuth += 360
##
##        while azimuth>360:
##            azimuth -= 360
##
##        self.azimuth = np.radians(azimuth)
##        self.azimuthtol = np.radians(azimuthtol)
##        self.bandwidth = bandwidth
##
##        """
##        if we set x direction as east and y direction as north
##        then the following azimuth will be zero toward east and
##        will be positive in counterclockwise direction
##        """
##        
##        self.degree = np.arctan2(self.dy,self.dx)+np.pi
##        self.degree = np.where(self.degree==2*np.pi,0,self.degree)
##
##        """
##        finding indexes when lag_distance matches data spacing, disMatch
##        and when dip angle matches azimuth, azmMatch
##        for non uniform spacing most modification will be here probably.
##        """
##
##        conAzm = np.logical_and(self.degree>self.azimuth-self.azimuthtol,
##                                self.degree<self.azimuth+self.azimuthtol)
##        
##        azmMatch = np.asfortranarray(np.where(conAzm)).T
##
##        self.experimental = np.zeros_like(self.bins)
##
##        for i,h in enumerate(self.bins):
##            
##            conDis = np.logical_and(self.distance>h-self.lagdistol,self.distance<h+self.lagdistol)
##
##            disMatch = np.asfortranarray(np.where(conDis)).T
##
##            """
##            comparing disMatch to azmMatch to find indices matching both
##            """
##
##            dtype={'names':['f{}'.format(i) for i in range(2)],'formats':2*[disMatch.dtype]}
##
##            match = np.intersect1d(disMatch.view(dtype),azmMatch.view(dtype))
##            match = match.view(disMatch.dtype).reshape(-1,2)
##
##            p1 = values[match[:,0]]
##            p2 = values[match[:,1]]
##
##            semivariance = ((p1-p2)**2).sum()/(2*match.shape[0])
##
##            self.experimental[i] = semivariance

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

        alpha = azmtol(self.bandwidth,self.outbound,self.azimuthtol)
        omega = bndwdt(self.bandwidth,self.outbound,self.azimuthtol)

        theta = np.linspace(self.azimuth-alpha,self.azimuth+alpha)
        sides = omega/np.sin(self.azimuthtol)

        xO1 = self.outbound*np.cos(self.azimuth)
        yO1 = self.outbound*np.sin(self.azimuth)

        xO2 = self.outbound*np.cos(self.azimuth-alpha)
        yO2 = self.outbound*np.sin(self.azimuth-alpha)

        xO3 = self.outbound*np.cos(self.azimuth+alpha)
        yO3 = self.outbound*np.sin(self.azimuth+alpha)

        xO4 = sides*np.cos(self.azimuth-self.azimuthtol)
        yO4 = sides*np.sin(self.azimuth-self.azimuthtol)

        xO5 = sides*np.cos(self.azimuth+self.azimuthtol)
        yO5 = sides*np.sin(self.azimuth+self.azimuthtol)

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
            
            hmin_alpha = azmtol(self.bandwidth,hmin,self.azimuthtol)
            hmin_theta = np.linspace(self.azimuth-hmin_alpha,self.azimuth+hmin_alpha)
            
            hmin_x = hmin*np.cos(hmin_theta)
            hmin_y = hmin*np.sin(hmin_theta)

            plt.plot(origin_x+hmin_x,origin_y+hmin_y,'r')

    def set_theoretical(self):

        d = self.distance
            
        self.theoretical = np.zeros_like(d)
        
        Co = self.nugget
        Cd = self.sill-self.nugget
        
        if self.type == 'power':
            self.theoretical[d>0] = Co+Cd*(d[d>0])**self.power
        elif self.type == 'spherical':
            self.theoretical[d>0] = Co+Cd*(3/2*(d[d>0]/self.range)-1/2*(d[d>0]/self.range)**3)
            self.theoretical[d>self.range] = self.sill
        elif self.type == 'exponential':
            self.theoretical[d>0] = Co+Cd*(1-np.exp(-3*(d[d>0]/self.range)))
        elif self.type == 'gaussian':
            self.theoretical[d>0] = Co+Cd*(1-np.exp(-3*(d[d>0]/self.range)**2))
        elif self.type == 'hole_effect':
            self.theoretical[d>0] = Co+Cd*(1-np.sin((d[d>0]/self.range))/(d[d>0]/self.range))

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

    def set_distance(self):
        """here distance is calculated in 2-dimensional array form"""
        self.dx = self.x-self.var.x.reshape((-1,1))
        self.dy = self.y-self.var.y.reshape((-1,1))
        self.dz = self.z-self.var.z.reshape((-1,1))

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

    def simple_kriging(self,prop,perc=0.5):

        "prop -> which property to krig"
        "perc -> percentile, perc=0.5 gives mean values"

        values = getattr(self.var,prop)

        self.set_distance()
        
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

        calc_prop_mat = self.lambdas*np.reshape(values,(-1,1))
        calc_vars_mat = self.lambdas*self.covariance

        self.property = calc_prop_mat.sum(axis=0)
        self.variance = self.sill-self.beta-calc_vars_mat.sum(axis=0)
        
        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def gaussian_simulation(self,prop):

        perc = np.random.rand(self.x.size)

        self.ordinary_kriging(prop,perc=perc)

if __name__ == "__main__":

    pass
