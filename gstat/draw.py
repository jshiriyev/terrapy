# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports

def search_box(lag_dist,
               lag_max,
               azimuth,
               azimuth_tol,
               bandwidth,
               origin_x=0,
               origin_y=0):

    """Nomenclature-BEGINNING"""
    ## alpha  : azimuth_tol at bandwidth dominated section
    ## omega  : bandwidth at azimuth_tol dominated section
    ## theta  : azimuth range at the specified distance
    """Nomenclature-END"""

    lag_dist_tol = lag_dist/2

    outbound = lag_max+lag_dist_tol

    bins = np.arange(lag_dist,outbound,lag_dist)

    def azmtol(bandwidth,bound,azm_tol):
        return(np.arcsin(min(np.sin(azm_tol),bandwidth/bound)))

    def bndwdt(bandwidth,bound,azm_tol):
        return min(bandwidth,bound*np.sin(azm_tol))

    alpha = azmtol(bandwidth,outbound,azimuth_tol)
    omega = bndwdt(bandwidth,outbound,azimuth_tol)

    theta = np.linspace(azimuth-alpha,azimuth+alpha)
    sides = omega/np.sin(azimuth_tol)

    xO1 = outbound*np.cos(azimuth)
    yO1 = outbound*np.sin(azimuth)

    xO2 = outbound*np.cos(azimuth-alpha)
    yO2 = outbound*np.sin(azimuth-alpha)

    xO3 = outbound*np.cos(azimuth+alpha)
    yO3 = outbound*np.sin(azimuth+alpha)

    xO4 = sides*np.cos(azimuth-azimuth_tol)
    yO4 = sides*np.sin(azimuth-azimuth_tol)

    xO5 = sides*np.cos(azimuth+azimuth_tol)
    yO5 = sides*np.sin(azimuth+azimuth_tol)

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

    x6 = outbound*np.cos(theta)
    y6 = outbound*np.sin(theta)

    plt.figure(figsize = (8,8))

    plt.plot(origin_x+x1,origin_y+y1,'b--')
    plt.plot(origin_x+x2,origin_y+y2,'k')
    plt.plot(origin_x+x3,origin_y+y3,'k')
    plt.plot(origin_x+x4,origin_y+y4,'k')
    plt.plot(origin_x+x5,origin_y+y5,'k')
    plt.plot(origin_x+x6,origin_y+y6,'k')

    plt.xlim((-60,60))
    plt.ylim((-60,60))

    ax = plt.gca()
    ax.set_aspect(1)

    plt.tight_layout()
    ##plt.axis('equal')

    for h in bins:
        
        hmin = h-lag_dist_tol
        
        hmin_alpha = azmtol(bandwidth,hmin,azimuth_tol)
        hmin_theta = np.linspace(azimuth-hmin_alpha,azimuth+hmin_alpha)
        
        hmin_x = hmin*np.cos(hmin_theta)
        hmin_y = hmin*np.sin(hmin_theta)

        plt.plot(origin_x+hmin_x,origin_y+hmin_y,'r')

    plt.show()

if __name__ == "__main__":

    lag_dist = 10
    lag_max = 50

    azimuth = np.radians(45)
    azimuth_tol = np.radians(22.5)

    bandwidth = 10

    origin_x = 0
    origin_y = 0

    search_box(lag_dist,
               lag_max,
               azimuth,
               azimuth_tol,
               bandwidth,
               origin_x,
               origin_y)
