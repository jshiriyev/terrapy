import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import poisson

from computational import mesh
from computational import finite_difference

bnd1 = (1,0,0)
bnd2 = (1,0,0)
bnd3 = (1,0,0)
bnd4 = (1,0,100)

grids = mesh()

num_x = 5
num_y = 5

grids.cartesian((20,10,1),
                (num_x,num_y,1),
                b_xmin=bnd1,
                b_xmax=bnd2,
                b_ymin=bnd3,
                b_ymax=bnd4)

solver = finite_difference(grids)

solver.central(order=2)

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector)

analytical = poisson()
analytical.onedimensional((0,7),(bnd1,bnd2),0)

solver.solve()
##
##plt.scatter(solver.grids.center[:,0],solver.unknown,c='r')
##plt.plot(analytical.x,analytical.u,'k')
##
##plt.show()

Xlin = np.linspace(10/num_x,20-10/num_x,num_x)
Ylin = np.linspace( 5/num_y,10- 5/num_y,num_y)

[Xmesh,Ymesh] = np.meshgrid(Xlin,Ylin)

XX = Xmesh.ravel()
YY = Ymesh.ravel()

##plt.scatter(XX,YY,s=5,c='k')

plt.contourf(Xmesh,Ymesh,solver.unknown.reshape(num_x,num_y),alpha=1,cmap="PuOr")
plt.colorbar()

plt.title('Porosity Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([0,20])
plt.ylim([0,10])

plt.show()
