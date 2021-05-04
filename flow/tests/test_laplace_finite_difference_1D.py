import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import diffusivity

from computational import finite_difference

bnd1 = (1,-1,50)
bnd2 = (1,0,100)

grids = finite_difference()

grids.cartesian((7,1,1),(7,1,1))

grids.initialize()

grids.transmissibility()

grids.central(order=2)

grids.implement_bc(b_xmin=bnd1,b_xmax=bnd2)

analytical = diffusivity()
analytical.cartesian_poisson_1D((0,7),(bnd1,bnd2),0)

grids.solve()

plt.scatter(grids.center[:,0],grids.unknown,c='r')
plt.plot(analytical.x,analytical.u,'k')

plt.show()
