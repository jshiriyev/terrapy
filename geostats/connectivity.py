import numpy as np

from scipy.stats import norm

if __name__ == "__main__":
    import setup

class variogram():

    """
    variogram class used to represent observational spatial points
    and calculates semi-variance values
    ...
    Attributes
    ----------
    Methods
    -------
    """

    def __init__(self,prop):

        # self.__dict__ = prop.__dict__.copy()

        self.property = prop

        self.x = prop.x
        self.y = prop.y
        self.z = prop.z

        self.dx = prop.dx
        self.dy = prop.dy
        self.dz = prop.dz

        self.distance = prop.distance
        self.angle = prop.angle

    def set_experimental(self,lag=None,lagtol=None,lagmax=None,
                             azimuth=None,azimuthtol=None,bandwidth=None,returnFlag=False):

        """
        azimuth range is (-\\pi,\\pi] in radians [(-180,180] in degrees]
        if we set +x to east and +y to north then the azimuth is selected
        to be zero in the +x direction and positive counterclockwise
        """
        
        prop_err = (self.property-self.property.reshape((-1,1)))**2

        """for an anisotropy only 2D data can be used FOR NOW"""

        if azimuth is None:
            self.azimuth = 0
            self.azimuthtol = np.pi
            self.bandwidth = np.inf
        else:
            self.azimuth = np.radians(azimuth)
            self.azimuthtol = np.radians(azimuthtol)
            self.bandwidth = bandwidth

        delta_angle = np.abs(self.angle-self.azimuth)
        
        con_azmtol = delta_angle<=self.azimuthtol
        con_banwdt = np.sin(delta_angle)*self.distance<=(self.bandwidth/2.)
        con_direct = np.logical_and(con_azmtol,con_banwdt)

        if lag is None:
            cond0 = self.distance!=0
            condM = np.logical_and(cond0,con_azmtol)
            self.lag = self.distance[condM].min()
        else:
            self.lag = lag

        if lagtol is None:
            self.lagtol = self.lag/2.
        else:
            self.lagtol = lagtol

        if lagmax is None:
            self.lagmax = self.distance[con_azmtol].max()
        else:
            self.lagmax = lagmax

        self.outbound = self.lagmax+self.lagtol

        """bins is the array of lag distances"""

        self.bins_experimental = np.arange(self.lag,self.outbound,self.lag)

        self.experimental = np.zeros_like(self.bins_experimental)
        
        for i,h in enumerate(self.bins_experimental):

            con_distnc = np.abs(self.distance-h)<=self.lagtol

            conoverall = np.logical_and(con_distnc,con_direct)

            num_matchcon = np.count_nonzero(conoverall)

            if num_matchcon==0:
                self.experimental[i] = np.nan
            else:
                semivariance = prop_err[conoverall].sum()/(2*num_matchcon)
                self.experimental[i] = semivariance

        if returnFlag:
            return self.bins_experimental,self.experimental

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

        for h in self.bins_experimental:
            
            hmin = h-self.lagtol
            
            hmin_alpha = azmtol(self.bandwidth,hmin,self.azimuthtol)
            hmin_theta = np.linspace(self.azimuth-hmin_alpha,self.azimuth+hmin_alpha)
            
            hmin_x = hmin*np.cos(hmin_theta)
            hmin_y = hmin*np.sin(hmin_theta)

            plt.plot(origin_x+hmin_x,origin_y+hmin_y,'r')

    def set_theoretical(self,vbins=None,vtype='spherical',vsill=None,vrange=None,vnugget=0):

        if vbins is None:
            if hasattr(self,"bins_experimental"):
                d = self.bins_experimental
            elif hasattr(self,"distance"):
                d = self.distance
        else:
            self.bins_theoretical = vbins
            d = vbins
        
        self.type = vtype

        if vsill is None:
            self.sill = self.property.var()
        else:
            self.sill = vsill
        
        self.set_attribute(vrange,"range",(d.max()-d.min())/5)
        
        self.nugget = vnugget
            
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

if __name__ == "__main__":

    pass
