# Standard library imports

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

#Local application imports
from analytical import poisson

from computational import mesh
from computational import finite_difference 

xN0 = 0
xN1 = 7

grids = mesh(7)
grids.cartesian(xN0,xN1)

beta = 10

bvector = np.full(grids.number,beta)

solver = finite_difference(grids)

solver.central(order=2)
solver.implement_bc(bvector)
solver.solve()

analytical = poisson()
analytical.onedimensional(np.array([[xN0],[xN1]]),
                          grids.boundary_conditions,beta)

plt.scatter(grids.centers,solver.unknowns,
            c='r',label='Finite Difference')

plt.plot(analytical.x,analytical.u,
         'k',label='Analytical')

plt.xlabel('x-axis')
plt.ylabel('pressure')

plt.legend(bbox_to_anchor=(0., 1.02, 1., .102),
           loc='lower left',ncol=2, mode="expand", borderaxespad=0.)

plt.xlim([0,7])
##plt.ylim([0,None])

plt.show()
