import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import poisson

from computational import mesh
from computational import finite_difference

bnd1 = (0,-1,7)
bnd2 = (1,0,10)

grids = mesh()

grids.cartesian((7,1,1),
                (7,1,1),
                b_xmin=bnd1,
                b_xmax=bnd2)

solver = finite_difference(grids)

solver.central(order=2)

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector)

analytical = poisson()
analytical.onedimensional((0,7),(bnd1,bnd2),0)

solver.solve()

plt.scatter(solver.grids.center[:,0],solver.unknown,c='r')
plt.plot(analytical.x,analytical.u,'k')

plt.show()
