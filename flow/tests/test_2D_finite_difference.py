

import matplotlib.pyplot as plt
import numpy as np

from bhospy.flow.analytical import poisson

from bhospy.flow.computational import mesh
from bhospy.flow.computational import finite_difference

bnd1 = (1,0,0)
bnd2 = (1,0,0)
bnd3 = (1,0,0)
bnd4 = (1,0,100)

grids = mesh()

grids.cartesian((5,5,1),
                (5,5,1),
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

plt.scatter(solver.grids.center[:,0],solver.unknown,c='r')
plt.plot(analytical.x,analytical.u,'k')

plt.show()