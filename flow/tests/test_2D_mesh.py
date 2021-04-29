# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from computational import mesh
from computational import finite_difference

grids = mesh()

grids.cartesian((4,3,2),
                (4,3,2),
                b_xmin=(1,0,50),
                b_xmax=(1,0,-50),
                b_ymin=(1,0,-70),
                b_ymax=(1,0,100))

plt.scatter(grids.center[:,0],grids.center[:,1],c='r')

plt.axvline(np.arange(grids.length[0])[1],0,grids.length[1],c='k',linestyle='--')
plt.axvline(np.arange(grids.length[0])[2],0,grids.length[1],c='k',linestyle='--')
plt.axvline(np.arange(grids.length[0])[3],0,grids.length[1],c='k',linestyle='--')

plt.axhline(np.arange(grids.length[1])[1],0,grids.length[0],c='k',linestyle='--')
plt.axhline(np.arange(grids.length[1])[2],0,grids.length[0],c='k',linestyle='--')

plt.xlim([0,grids.length[0]])
plt.ylim([0,grids.length[1]])

plt.xlabel('x-axis')
plt.ylabel('y-axis')

for i, txt in enumerate(grids.id[:12,0].astype('int')):
    plt.annotate(txt,(grids.center[i,0]+0.05,grids.center[i,1]+0.05))

plt.show()
