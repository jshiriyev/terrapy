# Standard library imports
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from computational import mesh
from computational import finite_difference

grids = mesh()

num_x = 4
num_y = 3
num_z = 2

grids.cartesian((num_x*1.,num_y*1.,num_z*1.),
                (num_x,num_y,num_z),
                 b_xmin=(1,0,50),
                 b_xmax=(1,0,-50),
                 b_ymin=(1,0,-70),
                 b_ymax=(1,0,100),
                 b_zmin=(0,1,0),
                 b_zmax=(0,1,0))

X = grids.center[:,0].reshape(num_x,num_y,num_z)
Y = grids.center[:,1].reshape(num_x,num_y,num_z)
Z = grids.center[:,2].reshape(num_x,num_y,num_z)

fig = plt.figure()

ax = fig.add_subplot(111, projection='3d')

ax.scatter(X,Y,Z,alpha=0.5,c='r')

ax.set_xlim3d([0,grids.length[0]])
ax.set_ylim3d([0,grids.length[1]])
ax.set_zlim3d([0,grids.length[2]])

ax.set_xlabel('x-axis')
ax.set_ylabel('y-axis')
ax.set_zlabel('z-axis')
##fig.colorbar(scat, shrink=0.5, aspect=5)

plt.show()
