import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

from analytical import poisson

from computational import mesh
from computational import finite_difference

grids = mesh()

num_x = 50
num_y = 50

grids.cartesian((20,10,10),
                (num_x,num_y,1),
                 b_xmin=(1,0,0),
                 b_xmax=(0,1,0),
                 b_ymin=(1,0,0),
                 b_ymax=(1,0,100))

solver = finite_difference(grids)

solver.central(order=2)

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector)

solver.solve()

X = solver.grids.center[:,0].reshape(num_x,num_y)
Y = solver.grids.center[:,1].reshape(num_x,num_y)

Z = solver.unknown.reshape(num_x,num_y)

plt.contourf(X,Y,Z,alpha=1,cmap=cm.PuBu)
plt.colorbar()

plt.title('Pressure Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([0,20])
plt.ylim([0,10])

plt.show()
