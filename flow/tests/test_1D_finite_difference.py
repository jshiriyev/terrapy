import matplotlib.pyplot as plt
import numpy as np

from bhospy.flow.analytical import poisson

from bhospy.flow.computational import mesh
from bhospy.flow.computational import finite_difference

bnd1 = (1,0,200)
bnd2 = (0,-1,10)

grids = mesh()

grids.cartesian((7,1,1),
                (7,1,1),
                b_xmin=bnd1,
                b_xmax=bnd2)

solver = finite_difference(grids)

solver.central1D(order=2)

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector)

analytical = poisson()
analytical.onedimensional((0,7),(bnd1,bnd2),0)

solver.solve()

plt.scatter(solver.grids.center[:,0],solver.unknown,c='r')
plt.plot(analytical.x,analytical.u,'k')

plt.show()
