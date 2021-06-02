# Standard library imports

import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

#Local application imports
##from analytical import diffusivity

from numerical import finite_difference 

"""Start Input"""
xN0 = 0
xN1 = 7
beta = 10
bL = (1,0,200)
bU = (0,1,50)
"""End Input"""

grids = finite_difference()

grids.cartesian((xN1,1,1),(70,1,1))

grids.initialize()

grids.transmissibility()

grids.central(order=2)

grids.implement_bc(bL,bU)

grids.solve(rhs=beta)

##analytical = diffusivity()
##
##analytical.cartesian_poisson_1D((xN0,xN1),(bL,bU),beta)

plt.scatter(grids.center[:,0],grids.unknown,c='r',label='Finite Difference')

##plt.plot(analytical.x,analytical.u,'k',label='Analytical')

plt.xlabel('x-axis')
plt.ylabel('pressure')

plt.legend(bbox_to_anchor=(0., 1.02, 1., .102),
           loc='lower left',ncol=2, mode="expand", borderaxespad=0.)

plt.xlim([0,7])
##plt.ylim([0,None])

plt.show()
