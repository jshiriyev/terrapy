"""Standard library imports"""
import os
import sys

"""Third party imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

class dataReader(object):
    
    def __init__(self, filename, header_lines=0):
        
        self.filename = filename
        self.header_lines = header_lines
        
        self.get_labels()
        self.format_data()
        
        return
    
    def read_file(self):
        
        with open(self.filename) as f:
            
            data = f.readlines()[self.header_lines:]
        
        return [data[i:i+4] for i in range(0, len(data), 4)]
    
    
    def get_labels(self):
        
        with open(self.filename) as f:
            
            header = f.readlines()[:self.header_lines]
            
        occurance_count = 0
        self.labels = []
        for line_number in range(len(header)):
            if 'LINE' in header[line_number].strip():
                line1 = header[line_number + 1]
                line2 = header[line_number + 2]
                self.labels.extend((line1 + line2).strip().split())
                occurance_count += 1
            if occurance_count == 4:
                break
                
        return
    
    def format_sample(self, sample_data):
        
        clean_data = []
        
        for data in sample_data:
            clean_data.extend(data.strip().split())
        
        return clean_data
    
    def format_data(self):
        
        data = self.read_file()
        
        data =  pd.DataFrame([self.format_sample(sample_data) for sample_data in data], columns=self.labels)
        
        #Clean up data using Panda's dataframe functions
        data = data.replace('.', np.nan)
        data = data.apply(pd.to_numeric, errors='ignore')
        
        #Drop the rows where KLH is NaN because these are our training values
        self.data = data.drop(data[data['KLH'].isna()].index)

    
    def get_dataframe(self):
        return self.data

class item():
    
    """
    statistical item with a spatial property

    ...
    Attributes
    ----------
    
    Methods
    -------
    set_property():
        assigns input properties to the self
    """

    def __init__(self,prop,**kwargs):
        
        self.set_property(prop,**kwargs)

    def set_property(self,prop,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given property"""

        if prop is not None:
            ones = np.ones(prop.shape)
            self.property = prop.ravel()
        elif X is not None:
            ones = np.ones(X.shape)
        elif Y is not None:
            ones = np.ones(Y.shape)
        elif Z is not None:
            ones = np.ones(Z.shape)
        else:
            return
        
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

class variogram(item):

    """
    variogram class used to represent observation points
    and calculates semivariogram values

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

    def __init__(self,prop,**kwargs):
        
        self.set_property(prop,**kwargs)

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

    def set_experimental(self,lagdis,lagmax,azimuth=None,azimuthtol=None,bandwidth=None):
        
        prop_err = (self.property-self.property.reshape((-1,1)))**2

        """bins is the array of lag distances"""
        self.lagdis = lagdis
        self.lagdistol = lagdis/2.
        self.lagmax = lagmax

        self.outbound = self.lagmax+self.lagdistol
        
        self.bins = np.arange(self.lagdis,self.outbound,self.lagdis)

        self.experimental = np.zeros_like(self.bins)

        """
        azimuth range is (-\pi,\pi] in radians [(-180,180] in degrees]
        if we set +x to east and +y to north then the azimuth is selected
        to be zero in the +x direction and positive counterclockwise
        """

        """for an anisotropy only 2D data can be used FOR NOW"""
        self.azimuth = np.radians(azimuth)
        self.azimuthtol = np.radians(azimuthtol)
        self.bandwidth = bandwidth

        if self.azimuth is not None:
            delta_angle = np.abs(self.angle-self.azimuth)
            
            con_azmtol = delta_angle<=self.azimuthtol
            con_banwdt = np.sin(delta_angle)*self.distance<=(self.bandwidth/2.)
            con_direct = np.logical_and(con_azmtol,con_banwdt)
            
        else:
            con_direct = np.ones(self.angle.shape,dtype=bool)

        for i,h in enumerate(self.bins):

            con_distnc = np.abs(self.distance-h)<=self.lagdistol

            conoverall = np.logical_and(con_distnc,con_direct)

            num_matchcon = np.count_nonzero(conoverall)

            if num_matchcon==0:
                self.experimental[i] = np.nan
            else:
                semivariance = prop_err[conoverall].sum()/(2*num_matchcon)
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

if __name__ == "__main__":

    pass
