# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from computational import finite_difference

grids = finite_difference()

num_x = 4
num_y = 3
num_z = 2

grids.cartesian((num_x*1.,num_y*1.,num_z*1.),
                (num_x,num_y,num_z))

X = grids.center[:,0].reshape(num_x,num_y,num_z)
Y = grids.center[:,1].reshape(num_x,num_y,num_z)
Z = grids.center[:,2].reshape(num_x,num_y,num_z)

fig = plt.figure()

ax = fig.add_subplot(111, projection='3d')

ax.scatter(X,Y,Z,alpha=0.5,c='r')

ax.set_xlim3d([0,grids.length_x])
ax.set_ylim3d([0,grids.length_y])
ax.set_zlim3d([0,grids.length_z])

ax.set_xlabel('x-axis')
ax.set_ylabel('y-axis')
ax.set_zlabel('z-axis')
##fig.colorbar(scat, shrink=0.5, aspect=5)

plt.show()
