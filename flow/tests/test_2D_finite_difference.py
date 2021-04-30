import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import poisson

from computational import mesh
from computational import finite_difference

bnd1 = (1,0,0)
bnd2 = (0,1,0)
bnd3 = (1,0,0)
bnd4 = (1,0,100)

grids = mesh()

num_x = 50
num_y = 50

grids.cartesian((20,10,10),
                (num_x,num_y,1),
                b_xmin=bnd1,
                b_xmax=bnd2,
                b_ymin=bnd3,
                b_ymax=bnd4)

solver = finite_difference(grids)

solver.central(order=2)

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector)

solver.solve()

X = solver.grids.center[:,0].reshape(num_x,num_y)
Y = solver.grids.center[:,1].reshape(num_x,num_y)

Z = solver.unknown.reshape(num_x,num_y)

plt.contourf(X,Y,Z,alpha=1,cmap="PuOr")
plt.colorbar()

plt.title('Pressure Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([0,20])
plt.ylim([0,10])

plt.show()
