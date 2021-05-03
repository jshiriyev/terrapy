# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from computational import finite_difference

grids = finite_difference()

num_x = 5
num_y = 6

grids.cartesian((5,6,2),(num_x,num_y,1))

layer = np.arange(grids.num_x*grids.num_y)

plt.scatter(grids.center[layer,0],grids.center[layer,1],c='r')

for i in np.arange(grids.size[0,0],grids.length_x,grids.size[0,0]):
    plt.axvline(i,0,grids.length_y,c='k',linestyle='--')

for j in np.arange(grids.size[0,1],grids.length_y,grids.size[0,1]):
    plt.axhline(j,0,grids.length_x,c='k',linestyle='--')

plt.xlim([0,grids.length_x])
plt.ylim([0,grids.length_y])

plt.xlabel('x-axis')
plt.ylabel('y-axis')

for idx in grids.id[layer,0]:
    plt.annotate(idx,(grids.center[idx,0]+0.05,grids.center[idx,1]+0.05))

plt.show()
