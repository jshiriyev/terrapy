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

with open('reservoir.csv') as csvfile:
    
    intext = list(csv.reader(csvfile))
    
    header = intext[0]
    values = np.array(intext[1:]).T.astype('float64')

data = {header[3]: values[3],
        header[4]: values[4]}

raman = variogram(data)

raman.x = values[0]
raman.y = values[1]

plt.figure(1)

##plt.scatter(raman.x,raman.y,s=20,c=raman.porosity,alpha=0.2,cmap="PuOr")
##plt.colorbar()

##plt.tight_layout()

ax = plt.gca()
ax.set_aspect(1)

##plt.show()

raman.set_distance(raman)

raman.set_experimental('porosity',
                       200,
                       3000,
                       azimuth=45,
                       azimuth_tol=22.5,
                       bandwidth=400)

dist = np.sqrt(raman.x**2+raman.y**2)

##raman.draw_search_box(origin_x=raman.x[dist.argsort()[50]],
##                      origin_y=raman.y[dist.argsort()[50]])

raman.type = 'spherical'
raman.nugget = 0
raman.sill = 0.001
raman.range = 500

raman.set_theoretical()

xmin = 0;
xmax = 4000;

ymin = 0;
ymax = 4000;

Nx = 200;
Ny = 200;

Xlin = np.linspace(xmin,xmax,Nx+1)
Ylin = np.linspace(ymin,ymax,Ny+1)

[Xmesh,Ymesh] = np.meshgrid(Xlin,Ylin)

XX = Xmesh.ravel()
YY = Ymesh.ravel()

##plt.scatter(XX,YY,s=5,c='k')

est = spatial_estimation(raman,X=XX,Y=YY)

est.ordinary_kriging("porosity",perc=0.975)

plt.contourf(Xmesh,Ymesh,est.property.reshape(Nx+1,Ny+1),alpha=1,cmap="PuOr")
plt.colorbar()

plt.title('Porosity Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([xmin,xmax])
plt.ylim([ymin,ymax])

plt.show()
    
## Class Exercise 1

##x = np.array([2,4,6])
##y = np.array([30,50,20])
##
##plt.scatter(x,y,c='k')
##plt.grid(alpha=0.2)
##plt.xlabel('x-axis',fontsize=14)
##plt.ylabel('property',fontsize=14)
##plt.xlim([0,9])
##plt.ylim([0,70])
##
##V = variogram(props=y,X=x)
##
##V.set_distance(V)
##
##V.type = 'exponential'
##V.nugget = 0
##V.sill = 100
##V.range = 10
##
##V.set_theoretical()
##
##X = np.array([1,2,3,4,5,6,7,8])
##
##x = np.linspace(1,8,701)
##
##E = kriging(V,X=x)
##
##E.ordinary()
##
##plt.plot(E.x,E.property,c='k')
##
##y1 = E.property+1.96*np.sqrt(E.variance)
##y2 = E.property-1.96*np.sqrt(E.variance)
##
##plt.fill_between(E.x,y1,y2,fc='lightgrey')
##
##plt.show()

##w1 = np.loadtxt("well1.txt",skiprows=1)
##w2 = np.loadtxt("well2.txt",skiprows=1)
##
##d1 = w1[:,0]
##d2 = w2[:,0]
##
##t1 = d1-np.append(np.array([0]),d1[:-1])
##t2 = d2-np.append(np.array([0]),d2[:-1])
##
##hw1 = heterogeneity(permeability=w1[:,2],porosity=w1[:,1],thickness=t1)
##hw2 = heterogeneity(permeability=w2[:,2],porosity=w2[:,1],thickness=t2)

##z = np.array([30,50,20])
##x = np.array([2,4,6])
##
##V = variogram(props=z,X=x)
##
##V.type = 'exponential'
##V.nugget = 0
##V.sill = 100
##V.range = 10
##
##V.set_distance()
##V.set_theoretical()
##
##x = np.array([8])
##
##E = kriging(V,X=x)
##E.ordinary()
##
##
##z = np.array([[32,24,20,10],
##              [28,20,17,12],
##              [12,16,10,9],
##              [18,12,7,8]])
##
##V = variogram(props=z,dX=10,dY=10)
##
##V.set_distance()
##
##V.set_experimental(10,30,90,3)
##
##plt.scatter(V.bins,V.experimental)
##
##plt.show()

##F = np.array([10,20,24,32,12,17,20,28,9,10,16,12,8,7,12,18])
##
##X = np.array([0,0,0,0,10,10,10,10,20,20,20,20,30,30,30,30])
##Y = -np.array([0,10,20,30,0,10,20,30,0,10,20,30,0,10,20,30])
##
##V.set_property(F)
##
##
##V.set_distance()
##
##V.set_bins(10,30)
##
##V.set_experimental(-90,2)
##
##plt.scatter(V.bins,V.experimental)
##
##plt.show()
##
##V.set_distance()

##V.type = 'spherical'
##
####V.power = 2
##V.nugget = 5
##V.sill = 20
##V.range = 10
##
##h = np.linspace(0,30,200)
##
##V.set_theoretical(h)
##
##plt.plot(h[1:],V.theoretical[1:],'k')
##
##plt.scatter(0,V.theoretical[0],facecolors='k',edgecolors='k')
##plt.scatter(0,V.nugget,facecolors='w',edgecolors='k')
##
##plt.grid(alpha=0.2)
##
##plt.xlabel('Lag-distance',fontsize=14)
##plt.ylabel('Semi-Variogram',fontsize=14)
##
##plt.title('Spherical Model',fontsize=14)
##
##plt.show()

##class bhos():
##    pass
##
##def bullet(x):
##
##    return x**2+2
##
##a = [1,2,3,4]
##
##print(bullet(5))
##
##b = 'cavid'
##
##sum2 = 0
##
##for l in b:
####    pass
##
##    print(l)
##
##for i in a:
##
##    if i > 3:
##        sum2 += i

##    print('hello world')
