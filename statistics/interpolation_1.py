import numpy as np

import matplotlib.pyplot as plt

data = np.loadtxt("porosity_data.txt",skiprows=1)

X = data[:,0]
Y = data[:,1]
Z = data[:,2]

plt.figure(1)

plt.scatter(X,Y,s=20,c=Z)

plt.colorbar()

plt.title('Porosity Map')
plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.show()

##from scipy.interpolate import Rbf
##
##plt.colorbar()
##
##plt.title('Porosity Map');
##plt.xlabel('x-axis')
##plt.ylabel('y-axis')
##
##xmin = X.min();
##xmax = X.max();
##
##ymin = Y.min();
##ymax = Y.max();
##
##rbf = Rbf(X,Y,Z,function='linear')
##
##Nx = 200;
##Ny = 200;
##
##Xlin = np.linspace(xmin,xmax,Nx)
##Ylin = np.linspace(ymin,ymax,Ny)
##
##[Xmesh,Ymesh] = np.meshgrid(Xlin,Ylin)
##
##Zmesh = rbf(Xmesh,Ymesh)
##
##plt.figure(2)
##
##plt.contourf(Xmesh,Ymesh,Zmesh);
##plt.colorbar()
##
##plt.title('Porosity Map');
##plt.xlabel('x-axis')
##plt.ylabel('y-axis')
##
##plt.show()
