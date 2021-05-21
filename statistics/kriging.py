import numpy as np

import matplotlib.pyplot as plt

from geostatistics.setpy import setup
from geostatistics.kriging import kriging

class observation: pass
class estimation: pass

sheets = {"num_cols": 4,"dataTypes": "col"}

setup('observation.csv',sheets,observation)

plt.figure(1)

plt.scatter(observation.X,observation.Y,s=20,c=observation.F,alpha=0.5)
plt.colorbar()

plt.title('Porosity Map');
plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.show()

xmin = 0;
xmax = 4000;

ymin = 0;
ymax = 4000;

Nx = 200;
Ny = 200;

Xlin = np.linspace(xmin,xmax,Nx+1).T
Ylin = np.linspace(ymin,ymax,Ny+1).T

[Xmesh,Ymesh] = np.meshgrid(Xlin,Ylin)

observation.type = 'spherical'
observation.nugget = 0
observation.sill = 0.001
observation.range = 500

estimation.X = Xmesh.flatten()
estimation.Y = Ymesh.flatten()
estimation.Z = np.ones_like(estimation.X)

estimation.mean = observation.F.mean()

estimation = kriging(observation).simple(estimation)

##Zmesh = griddata((X,Y),Z,(Xmesh,Ymesh),method='linear');
##
plt.figure(2)

plt.contourf(Xmesh,Ymesh,estimation.F_variance.reshape(201,201));
plt.colorbar()

plt.title('Porosity Map');
plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.show()
