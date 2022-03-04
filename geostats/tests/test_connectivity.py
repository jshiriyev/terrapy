# Standard library imports
import csv
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports
##from bhos.geostat import item
##from bhos.univariate import heterogeneity
##from bhos.univariate import uncertainty
from univariate import variogram
from univariate import spatial_estimation

with open('reservoir_pyrcz.csv') as csvfile:
    
    intext = list(csv.reader(csvfile))
    
    header = intext[0]
    values = np.array(intext[1:]).T.astype('float64')

data = {header[3]: values[3], #porosity
        header[4]: values[4]} #permeability

V = variogram(values[3],X=values[0],Y=values[1])

##plt.figure(1)

##plt.scatter(V.x,V.y,s=20,c=V.porosity,alpha=0.2,cmap="PuOr")
##plt.colorbar()

##plt.tight_layout()

##ax = plt.gca()
##ax.set_aspect(1)

##plt.show()

##V.draw_search_box(origin_x=0,origin_y=0)

V.set_experimental(25,3000,azimuth=45,azimuthtol=22.5,bandwidth=200)

plt.plot(V.bins,V.experimental,'.')
plt.show()

##V.type = 'spherical'
##V.nugget = 0
##V.sill = 0.001
##V.range = 500
##
##V.set_theoretical()
##
##xmin = 0;
##xmax = 4000;
##
##ymin = 0;
##ymax = 4000;
##
##Nx = 200;
##Ny = 200;
##
##Xlin = np.linspace(xmin,xmax,Nx+1)
##Ylin = np.linspace(ymin,ymax,Ny+1)
##
##[Xmesh,Ymesh] = np.meshgrid(Xlin,Ylin)
##
##XX = Xmesh.ravel()
##YY = Ymesh.ravel()
##
####plt.scatter(XX,YY,s=5,c='k')
##
##E = spatial_estimation(V,X=XX,Y=YY)
##
##E.ordinary_kriging()
##
##plt.contourf(Xmesh,Ymesh,E.property.reshape(Nx+1,Ny+1),alpha=1,cmap="PuOr")
##plt.colorbar()
##
##plt.title('Porosity Map',fontsize=14)
##plt.xlabel('x-axis',fontsize=14)
##plt.ylabel('y-axis',fontsize=14)
##
##plt.xlim([xmin,xmax])
##plt.ylim([ymin,ymax])
##
##plt.show()
    
