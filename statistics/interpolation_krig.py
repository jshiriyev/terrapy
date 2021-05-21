import numpy as np

import matplotlib.pyplot as plt

from scipy import interpolate

##x = np.linspace(0,np.pi,100)
##y = np.cos(x)+np.random.rand(1,100)
####z = np.linspace(1,10,len(x))
##z = x**2-y**2

x = np.array([600,400,800,600,500,800])
y = np.array([800,700,400,600,400,800])
z = np.array([0.32,0.43,0.56,0.31,0.62,0.61])

zz = np.repeat(z.reshape(1,-1),len(z),axis=0)

plt.figure(1)

plt.scatter(x,y,s=20,c=z,alpha=0.5)
plt.colorbar()

plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.show()

f = interpolate.interp2d(x,y,zz,kind='cubic')

xmin = x.min();
xmax = x.max();

ymin = y.min();
ymax = y.max();

Nx = 200;
Ny = 200;

xlin = np.linspace(xmin,xmax,Nx)
ylin = np.linspace(ymin,ymax,Ny)

Zmesh = f(xlin,ylin)

[Xmesh,Ymesh] = np.meshgrid(xlin,ylin)

plt.figure(2)

plt.contourf(Xmesh,Ymesh,Zmesh);
plt.colorbar()

plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.show()
