# Standard library imports

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

#Local application imports
from analytical import poisson
from computational import finite_difference 

xN0 = 0
xN1 = 7

class grids(): pass

grids.number = 7
grids.ids = np.arange(grids.number)
grids.nodes = np.linspace(xN0,xN1,grids.number+1)
grids.sizes = grids.nodes[1:]-grids.nodes[:-1]
grids.centers = grids.nodes[:-1]+grids.sizes/2
grids.boundary_ids = np.array([[0,1],[grids.number-1,grids.number-2]])
grids.boundary_conditions = np.array([[1,0,200],[0,1,0]])

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
