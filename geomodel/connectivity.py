import numpy as np

from scipy.stats import norm

if __name__ == "__main__":
    import setup

class SpatProp(np.ndarray):

    """It is a numpy array of shape (N,) with additional spatial attributes x,y,z"""

    def __new__(cls,values=None,shape=None,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):

        """if provided, X,Y,Z must be one dimensional numpy array"""

        if values is not None:
            inp1 = (values.size,)
            inp2 = values.ravel()
            inp3 = values.dtype
            ones = np.ones(values.shape)
        else:
            inp1 = (np.prod(shape),)
            inp2 = np.zeros(shape).ravel()
            inp3 = np.float64
            ones = np.ones(shape)

        self = super().__new__(cls,shape=inp1,buffer=inp2,dtype=inp3)

        if X is not None:
            self.x = X
        else:
            self.x = (np.cumsum(ones,0)-1).ravel()*dX

        if Y is not None:
            self.y = Y
        elif ones.ndim>1:
            self.y = (np.cumsum(ones,1)-1).ravel()*dY
        else:
            self.y = ones.flatten()

        if Z is not None:
            self.z = Z
        elif ones.ndim>2:
            self.z = (np.cumsum(ones,2)-1).ravel()*dZ
        else:
            self.z = ones.flatten()

        return self

    def set_connection(self):

        """
        It calculates distance and angle between observation points;
        both are calculated in 2-dimensional array form.
        """
        self.dx,self.dy,self.dz = SpatProp.get_distance(self,returnDeltaFlag=True)

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

        self.angle = np.arctan2(self.dy,self.dx)

    def set_experimentalVariogram(self,lag=None,lagtol=None,lagmax=None,
        azimuth=None,azimuthtol=None,bandwidth=None,returnFlag=False):

        """
        azimuth range is (-\\pi,\\pi] in radians [(-180,180] in degrees]
        if we set +x to east and +y to north then the azimuth is selected
        to be zero in the +x direction and positive counterclockwise
        """
        
        prop_err = (self-np.reshape(self,(-1,1)))**2

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

    def set_theoreticalVariogram(self,vbins=None,vtype='spherical',vsill=None,vrange=None,vnugget=0,**kwars):

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
            self.sill = self.var().tolist()
        else:
            self.sill = vsill
        
        if vrange is None:
            self.range = (d.max()-d.min())/5
        else:
            self.range = vrange

        self.nugget = vnugget

        self.theoretical,self.covariance = SpatProp.get_varmodel(
            d,self.type,self.sill,self.range,self.nugget,**kwars)

    @staticmethod
    def get_distance(A,B=None,returnDeltaFlag=False):

        if B is None:
            B = A

        dx = A.x-B.x.reshape((-1,1))
        dy = A.y-B.y.reshape((-1,1))
        dz = A.z-B.z.reshape((-1,1))

        if returnDeltaFlag:
            return dx,dy,dz
        else:
            return np.sqrt(dx**2+dy**2+dz**2)

    @staticmethod
    def get_varmodel(vbins,vtype,vsill,vrange,vnugget,power=1):
            
        theoretical = np.zeros_like(vbins)
        
        Co = vnugget
        Cd = vsill-vnugget
        
        if vtype == 'power':
            theoretical[vbins>0] = Co+Cd*(vbins[vbins>0])**power
        elif vtype == 'spherical':
            theoretical[vbins>0] = Co+Cd*(3/2*(vbins[vbins>0]/vrange)-1/2*(vbins[vbins>0]/vrange)**3)
            theoretical[vbins>vrange] = vsill
        elif vtype == 'exponential':
            theoretical[vbins>0] = Co+Cd*(1-np.exp(-3*(vbins[vbins>0]/vrange)))
        elif vtype == 'gaussian':
            theoretical[vbins>0] = Co+Cd*(1-np.exp(-3*(vbins[vbins>0]/vrange)**2))
        elif vtype == 'hole_effect':
            theoretical[vbins>0] = Co+Cd*(1-np.sin((vbins[vbins>0]/vrange))/(vbins[vbins>0]/vrange))

        covariance = vsill-theoretical

        return theoretical,covariance

if __name__ == "__main__":

    A = SpatProp(np.array([[1,2,3,4]]))

    A.set_connection()

    A.set_experimentalVariogram()

